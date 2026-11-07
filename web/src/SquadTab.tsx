import type { AllyView, SquadView } from './types'
import { Empty, Meter, num } from './ui'

function AllyCard(props: { ally: AllyView }) {
  const a = props.ally
  return (
    <section className="card ally">
      <div className="ally-head">
        <span className="ally-pos">{a.position}</span>
        <span className="ally-name">
          <span className="row-title">{a.name}</span>
          <span className="row-sub">
            {a.race} · стихия {a.element} · ур. {num(a.lvl)}
          </span>
        </span>
      </div>
      <Meter icon="❤" label="Здоровье" pool={a.hp} tone="hp" />
      <Meter icon="🧥" label="Броня" pool={a.armor} tone="armor" />
      <Meter icon="⚡" label="Энергия" pool={a.energy} tone="energy" />
      <div className="ally-stats">
        <span>⚔ {num(a.attack)}</span>
        <span>🛡 {num(a.defence)}</span>
        <span>🎯 {num(a.accuracy)}</span>
        <span>👁 {num(a.evasion)}</span>
      </div>
      {a.leaves && <div className="row-sub">⏳ Уйдёт через {a.leaves}</div>}
    </section>
  )
}

/** «Отряд»: строй по местам. Переставить и выгнать — в боте, здесь только смотреть. */
export function SquadTab(props: { data: SquadView }) {
  const s = props.data
  if (s.allies.length === 0)
    return (
      <Empty>
        Союзников пока нет. Наёмники ждут за дальним столом в таверне, а поднятые и крысы находят героя в лабиринте сами.
      </Empty>
    )

  // Места до последнего занятого — по порядку; хвост пустых — одной строкой.
  const last = Math.max(s.heroPosition, ...s.allies.map((a) => a.position))
  const places = Array.from({ length: last }, (_, i) => i + 1)

  return (
    <div className="character">
      <p className="muted-note">
        Позиция в строю — против какого врага стоять. В бою строй съезжает к первому месту, а промежутки между бойцами
        остаются.
      </p>
      {s.pendingRisen && (
        <section className="card card-warn">
          💀 {s.pendingRisen} поднялся и ждёт у шатра, но мест в строю нет. Решить, кем пожертвовать, можно в «Отряде» в
          сообщениях сообщества.
        </section>
      )}
      {places.map((pos) => {
        const ally = s.allies.find((a) => a.position === pos)
        if (ally) return <AllyCard key={pos} ally={ally} />
        if (pos === s.heroPosition)
          return (
            <div key={pos} className="row row-plain row-static row-hero">
              <span className="ally-pos">{pos}</span>
              <span className="row-body">
                <span className="row-title">👤 Вы</span>
              </span>
            </div>
          )
        return (
          <div key={pos} className="row row-plain row-static row-empty row-slim">
            <span className="ally-pos">{pos}</span>
            <span className="row-body row-sub">пусто</span>
          </div>
        )
      })}
      {last < s.positions && (
        <p className="muted-note">
          Места {last + 1}–{s.positions} свободны.
        </p>
      )}
    </div>
  )
}
