import { useEffect, useState } from 'react'
import { api } from './api'
import type { AllyView, SquadActionView, SquadView } from './types'
import { Empty, Meter, num } from './ui'

/** Кого приказ касается: союзник на своём месте или сам герой. */
type Pick = { ally: number } | 'hero'

/** Что можно делать со строем; нет его вовсе (`null`) — герой в бою. */
interface Orders {
  run: (act: () => Promise<SquadActionView>, after?: Pick | null) => void
  busy: boolean
}

/** Куда встать: все места, кроме своего, кнопками — номер и кто там сейчас.
  * Занятое — меняются местами, как в боте. */
function Places(props: { s: SquadView; from: number; onPick: (to: number) => void; busy: boolean }) {
  const { s } = props
  const who = (pos: number) =>
    pos === s.heroPosition ? 'Вы' : (s.allies.find((a) => a.position === pos)?.name ?? 'свободно')
  return (
    <div className="squad-places">
      {Array.from({ length: s.positions }, (_, i) => i + 1)
        .filter((pos) => pos !== props.from)
        .map((pos) => (
          <button key={pos} className="squad-place" onClick={() => props.onPick(pos)} disabled={props.busy}>
            <span className="squad-place-n">{pos}</span>
            <span className="squad-place-who">{who(pos)}</span>
          </button>
        ))}
    </div>
  )
}

/** Приказы союзнику: переставить и выгнать — с подтверждением, как в боте. */
function AllyOrders(props: { s: SquadView; ally: AllyView; orders: Orders }) {
  const { ally, orders } = props
  const [confirm, setConfirm] = useState(false)
  return (
    <div className="squad-orders" onClick={(e) => e.stopPropagation()}>
      <div className="sub-title">Переставить на место</div>
      <Places
        s={props.s}
        from={ally.position}
        busy={orders.busy}
        onPick={(to) => orders.run(() => api.squadMove(ally.position, to), { ally: to })}
      />
      {confirm ? (
        <div className="buy-confirm">
          <p className="buy-ask">Выгнать {ally.name} из отряда?</p>
          <div className="buy-row">
            <button
              className="squad-danger"
              onClick={() => orders.run(() => api.squadDismiss(ally.position), null)}
              disabled={orders.busy}
            >
              Выгнать
            </button>
            <button className="buy-no" onClick={() => setConfirm(false)} disabled={orders.busy}>
              Отмена
            </button>
          </div>
        </div>
      ) : (
        <button className="equip-off" onClick={() => setConfirm(true)} disabled={orders.busy}>
          Выгнать из отряда
        </button>
      )}
    </div>
  )
}

function AllyCard(props: {
  s: SquadView
  ally: AllyView
  open: boolean
  /** Раскрыть приказы; `null` — сейчас приказывать нельзя: бой. */
  onToggle: (() => void) | null
  orders: Orders | null
}) {
  const a = props.ally
  return (
    <section
      className={`card ally${props.onToggle ? ' ally-tap' : ''}${props.open ? ' ally-open' : ''}`}
      onClick={props.onToggle ?? undefined}
    >
      <div className="ally-head">
        <span className="ally-pos">{a.position}</span>
        <span className="ally-name">
          <span className="row-title">{a.name}</span>
          <span className="row-sub">
            {a.race} · стихия {a.element} · ур. {num(a.lvl)}
          </span>
        </span>
        {props.onToggle && (
          <span className="erow-chev" aria-hidden>
            {props.open ? '▾' : '›'}
          </span>
        )}
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
      {props.open && props.orders && <AllyOrders s={props.s} ally={a} orders={props.orders} />}
    </section>
  )
}

/** Восставший ждёт у шатра, а мест нет: кем за него пожертвовать — или
  * отпустить его. Жертву подтверждают: ушедшего не вернуть. */
function Risen(props: { s: SquadView; orders: Orders | null }) {
  const { s, orders } = props
  const [victim, setVictim] = useState<AllyView | null>(null)
  return (
    <section className="card card-warn squad-risen">
      <div>💀 {s.pendingRisen} поднялся и ждёт у шатра, но мест в строю нет. Кем пожертвуем?</div>
      {orders &&
        (victim ? (
          <div className="buy-confirm">
            <p className="buy-ask">
              {victim.name} уйдёт из отряда, а {s.pendingRisen} встанет на место {victim.position}.
            </p>
            <div className="buy-row">
              <button
                className="squad-danger"
                onClick={() => orders.run(() => api.squadRisenTake(victim.position), null)}
                disabled={orders.busy}
              >
                Пожертвовать
              </button>
              <button className="buy-no" onClick={() => setVictim(null)} disabled={orders.busy}>
                Отмена
              </button>
            </div>
          </div>
        ) : (
          <>
            <div className="squad-places">
              {s.allies.map((a) => (
                <button key={a.position} className="squad-place" onClick={() => setVictim(a)} disabled={orders.busy}>
                  <span className="squad-place-n">{a.position}</span>
                  <span className="squad-place-who">{a.name}</span>
                </button>
              ))}
            </div>
            <button
              className="equip-off"
              onClick={() => orders.run(() => api.squadRisenDismiss(), null)}
              disabled={orders.busy}
            >
              Отпустить его
            </button>
          </>
        ))}
    </section>
  )
}

/** «Отряд»: строй по местам. Нажал на союзника или на себя — куда встать, а
  * союзника ещё и выгнать; восставший без места — кем за него пожертвовать.
  * Всё как в «Отряде» в боте и где угодно, только не в бою: бот проверит это
  * сам, под тем же замком, что и ходы в чате. Что сказал бот — всплывает
  * над нижней панелью. */
export function SquadTab(props: { data: SquadView; onChange: (v: SquadView) => void }) {
  const s = props.data
  const [pick, setPick] = useState<Pick | null>(null)
  const [busy, setBusy] = useState(false)
  const [toast, setToast] = useState<string | null>(null)

  useEffect(() => {
    if (!toast) return
    const t = window.setTimeout(() => setToast(null), 5000)
    return () => window.clearTimeout(t)
  }, [toast])

  const orders: Orders | null = s.locked
    ? null
    : {
        busy,
        run: (act, after) => {
          setBusy(true)
          act()
            .then((r) => {
              props.onChange(r.squad)
              setPick(after ?? null)
              if (r.note) setToast(r.note)
            })
            .catch(() => setToast('Ответ не дошёл. Обновите (⟳) и проверьте отряд.'))
            .finally(() => setBusy(false))
        },
      }

  const toggle = (p: Pick) => () =>
    setPick((cur) => (JSON.stringify(cur) === JSON.stringify(p) ? null : p))

  if (s.allies.length === 0)
    return (
      <Empty>
        Союзников пока нет. Наёмники ждут за дальним столом в таверне, а поднятые и крысы находят героя в лабиринте сами.
      </Empty>
    )

  // Места до последнего занятого — по порядку; хвост пустых — одной строкой.
  const last = Math.max(s.heroPosition, ...s.allies.map((a) => a.position))
  const places = Array.from({ length: last }, (_, i) => i + 1)
  const heroOpen = pick === 'hero'

  return (
    <div className="character">
      {s.locked && <section className="card card-warn">{s.locked}</section>}
      <p className="muted-note">
        Позиция в строю — против какого врага стоять. В бою строй съезжает к первому месту, а промежутки между бойцами
        остаются.{orders && ' Нажмите на союзника или на себя, чтобы переставить.'}
      </p>
      {s.pendingRisen && <Risen s={s} orders={orders} />}
      {places.map((pos) => {
        const ally = s.allies.find((a) => a.position === pos)
        if (ally) {
          const open = typeof pick === 'object' && pick?.ally === pos
          return (
            <AllyCard
              key={pos}
              s={s}
              ally={ally}
              open={open}
              onToggle={orders && toggle({ ally: pos })}
              orders={orders}
            />
          )
        }
        if (pos === s.heroPosition)
          return (
            <div
              key={pos}
              className={`card row-hero squad-hero${orders ? ' ally-tap' : ''}`}
              onClick={orders ? toggle('hero') : undefined}
            >
              <div className="ally-head">
                <span className="ally-pos">{pos}</span>
                <span className="row-body">
                  <span className="row-title">👤 Вы</span>
                </span>
                {orders && (
                  <span className="erow-chev" aria-hidden>
                    {heroOpen ? '▾' : '›'}
                  </span>
                )}
              </div>
              {heroOpen && orders && (
                <div className="squad-orders" onClick={(e) => e.stopPropagation()}>
                  <div className="sub-title">Встать на место</div>
                  <Places s={s} from={pos} busy={busy} onPick={(to) => orders.run(() => api.squadHero(to), 'hero')} />
                </div>
              )}
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
      {toast && (
        <div className="toast" role="status" onClick={() => setToast(null)}>
          {toast}
        </div>
      )}
    </div>
  )
}
