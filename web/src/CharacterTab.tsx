import { useState } from 'react'
import type { CharacterView, EffectView, StatDetailView, StatLineView, StatView } from './types'
import { Meter, num, Sheet } from './ui'

type StatSheet = {
  icon: string
  label: string
  current: number
  lines: StatLineView[]
}

const signed = (v: number) => (v > 0 ? `+${num(v)}` : v < 0 ? `−${num(-v)}` : '0')

/** Характеристика: крупно — текущая, с ней герой выходит в бой; под ней —
  * родная, если раса, травмы или зелья её сдвинули. */
function Stat(props: { icon: string; label: string; stat: StatView; onOpen: (s: StatSheet) => void }) {
  const { current, native } = props.stat
  const shift = current > native ? 'stat-up' : current < native ? 'stat-down' : ''
  return (
    <button type="button" className="stat stat-btn" onClick={() => props.onOpen({ icon: props.icon, label: props.label, current, lines: props.stat.lines })}>
      <span className="stat-label">
        {props.icon} {props.label}
      </span>
      <span className={`stat-value ${shift}`}>{num(current)}</span>
      {current !== native && <span className="stat-native">родное {num(native)}</span>}
    </button>
  )
}

function Plain(props: { icon: string; label: string; detail: StatDetailView; onOpen: (s: StatSheet) => void }) {
  return (
    <button type="button" className="stat stat-btn" onClick={() => props.onOpen({ icon: props.icon, label: props.label, current: props.detail.current, lines: props.detail.lines })}>
      <span className="stat-label">
        {props.icon} {props.label}
      </span>
      <span className="stat-value">{num(props.detail.current)}</span>
    </button>
  )
}

/** Группы эффектов — в том же порядке, что список «Эффекты» в боте. */
const effectGroups: { label: string; kinds: EffectView['kind'][] }[] = [
  { label: 'Зелья и бафы', kinds: ['buffs'] },
  { label: 'Умения', kinds: ['active'] },
  { label: 'Пассивные', kinds: ['passive'] },
  { label: 'Камни и наборы', kinds: ['weapon', 'armor', 'set'] },
]

function Effects(props: { effects: EffectView[]; onOpen: (e: EffectView) => void }) {
  if (props.effects.length === 0)
    return (
      <p className="muted-note">
        Эффектов пока нет — наденьте оружие, нагрудник или другое снаряжение с руной, либо выжгите руну у Казимира.
      </p>
    )
  return (
    <div className="groups">
      {effectGroups.map((g) => {
        const list = props.effects.filter((e) => g.kinds.includes(e.kind))
        if (list.length === 0) return null
        return (
          <div key={g.label} className="group">
            <div className="group-label">{g.label}</div>
            <div className="list">
              {list.map((e) => (
                <button key={e.kind + e.label} className="row row-plain" onClick={() => props.onOpen(e)}>
                  <span className="row-body">
                    <span className="row-title">{e.label}</span>
                  </span>
                  <span className="row-chevron" aria-hidden>
                    ›
                  </span>
                </button>
              ))}
            </div>
          </div>
        )
      })}
    </div>
  )
}

export function CharacterTab(props: { data: CharacterView }) {
  const c = props.data
  const [effect, setEffect] = useState<EffectView | null>(null)
  const [stat, setStat] = useState<StatSheet | null>(null)
  const notes = [
    c.expFrozen && '❄ Опыт остановлен',
    c.rested && `🛏 Выспались дома: +${c.rested.expPct}% опыта, ещё ${c.rested.left}`,
    c.blessingLeft && `✨ Благословение: ещё ${c.blessingLeft}`,
    c.instantRests > 0 && `⚡ Быстрых отдыхов: ${num(c.instantRests)}`,
  ].filter((n): n is string => typeof n === 'string')
  const hidden = c.achievements.total - c.achievements.got.length

  return (
    <div className="character">
      <section className="card">
        {c.name && <div className="char-name">{c.name}</div>}
        <div className="char-sub">
          {c.race} · {num(c.hero.lvl)} уровень · ☠ Убито: {num(c.kills)}
        </div>
        <Meter icon="📖" label="Опыт" pool={{ current: c.exp, max: c.neededExp }} tone="exp" />
        {notes.length > 0 && (
          <ul className="char-notes">
            {notes.map((n) => (
              <li key={n}>{n}</li>
            ))}
          </ul>
        )}
      </section>

      <section className="card">
        <Meter icon="❤" label="Здоровье" pool={c.hp} tone="hp" />
        <Meter icon="🧥" label="Броня" pool={c.armor} tone="armor" />
        <Meter icon="⚡" label="Энергия" pool={c.energy} tone="energy" />
      </section>

      {c.traumas.length > 0 && (
        <section className="card card-warn">
          <div className="card-title">
            ⚠ Травмы{c.traumaLeft && <span className="card-aside">пройдут через {c.traumaLeft}</span>}
          </div>
          <ul className="trauma-list">
            {c.traumas.map((t) => (
              <li key={t.name}>
                <span className="trauma-name">{t.name}</span>
                <span className="trauma-pen">{t.penalties}</span>
              </li>
            ))}
          </ul>
        </section>
      )}

      <h2 className="section-title">Характеристики</h2>
      <div className="stats">
        <Stat icon="💪" label="Сила" stat={c.str} onOpen={setStat} />
        <Stat icon="🫀" label="Телосложение" stat={c.vit} onOpen={setStat} />
        <Stat icon="🏃" label="Ловкость" stat={c.agi} onOpen={setStat} />
        <Stat icon="🧠" label="Интеллект" stat={c.int} onOpen={setStat} />
      </div>
      {c.upgradePoints > 0 && (
        <p className="char-points">
          Свободных очков: {num(c.upgradePoints)}. Распределить их можно в сообщениях сообщества: «Персонаж» →
          «Распределить характеристики».
        </p>
      )}

      <h2 className="section-title">В бою</h2>
      <div className="stats">
        <Plain icon="⚔" label="Атака" detail={c.attack} onOpen={setStat} />
        <Plain icon="🛡" label="Защита" detail={c.defence} onOpen={setStat} />
        <Plain icon="🎯" label="Точность" detail={c.accuracy} onOpen={setStat} />
        <Plain icon="👁" label="Уклонение" detail={c.evasion} onOpen={setStat} />
      </div>

      <h2 className="section-title">Эффекты</h2>
      <Effects effects={c.effects} onOpen={setEffect} />

      <h2 className="section-title">
        Достижения <span className="section-count">{c.achievements.got.length} из {c.achievements.total}</span>
      </h2>
      <div className="list">
        {c.achievements.got.length === 0 && <p className="muted-note">Пока ни одного.</p>}
        {c.achievements.got.map((a) => (
          <div key={a.title} className="row row-plain row-static">
            <span className="row-icon" aria-hidden>
              🏆
            </span>
            <span className="row-body">
              <span className="row-title">{a.title}</span>
              <span className="row-sub">{a.bonus}</span>
            </span>
          </div>
        ))}
        {hidden > 0 && (
          <p className="muted-note">
            🔒 Ещё не получено: {hidden}. Что это за достижения, узнаете, когда получите.
          </p>
        )}
      </div>

      <h2 className="section-title">Знания</h2>
      <div className="list">
        {c.knowledge.length === 0 && <p className="muted-note">Пока никаких знаний — только опыт.</p>}
        {c.knowledge.map((k) => (
          <div key={k.title} className="row row-plain row-static">
            <span className="row-icon" aria-hidden>
              📖
            </span>
            <span className="row-body">
              <span className="row-title">
                {k.title} <span className="row-sub">({k.how})</span>
              </span>
              <span className="row-sub">{k.text}</span>
            </span>
          </div>
        ))}
      </div>

      {stat && (
        <Sheet onClose={() => setStat(null)}>
          <div className="sheet-title">
            {stat.icon} {stat.label} = {num(stat.current)}
          </div>
          <ul className="stat-breakdown">
            {stat.lines.map((l) => (
              <li key={l.label} className={l.value > 0 ? 'stat-line-up' : l.value < 0 ? 'stat-line-down' : undefined}>
                <span>{l.label}</span>
                <span>{signed(l.value)}</span>
              </li>
            ))}
          </ul>
        </Sheet>
      )}

      {effect && (
        <Sheet onClose={() => setEffect(null)}>
          <div className="sheet-title">{effect.label}</div>
          <div className="item-text">{effect.text}</div>
        </Sheet>
      )}
    </div>
  )
}