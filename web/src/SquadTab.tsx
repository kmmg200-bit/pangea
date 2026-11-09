import { useState, type ReactNode } from 'react'
import { api } from './api'
import type { AllyView, SquadActionView, SquadView } from './types'
import { Empty, Meter, num, useToast } from './ui'

/** Кого приказ касается: союзник на своём месте, союзник из резерва (номер в
  * нём) или сам герой. */
type Pick = { ally: number } | { reserve: number } | 'hero'

const pickedAlly = (p: Pick | null): number | null => (p !== null && typeof p === 'object' && 'ally' in p ? p.ally : null)
const pickedReserve = (p: Pick | null): number | null =>
  p !== null && typeof p === 'object' && 'reserve' in p ? p.reserve : null

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

/** «Выгнать» с подтверждением, как в боте: ушедшего не вернуть. */
function Dismiss(props: { name: string; label: string; busy: boolean; onDismiss: () => void }) {
  const [confirm, setConfirm] = useState(false)
  return confirm ? (
    <div className="buy-confirm">
      <p className="buy-ask">Выгнать {props.name} из отряда?</p>
      <div className="buy-row">
        <button className="squad-danger" onClick={props.onDismiss} disabled={props.busy}>
          Выгнать
        </button>
        <button className="buy-no" onClick={() => setConfirm(false)} disabled={props.busy}>
          Отмена
        </button>
      </div>
    </div>
  ) : (
    <button className="equip-off" onClick={() => setConfirm(true)} disabled={props.busy}>
      {props.label}
    </button>
  )
}

/** Приказы союзнику в строю: переставить, отправить в резерв (крысу туда не
  * берут) и выгнать — с подтверждением, как в боте. */
function AllyOrders(props: { s: SquadView; ally: AllyView; orders: Orders }) {
  const { ally, orders } = props
  return (
    <div className="squad-orders" onClick={(e) => e.stopPropagation()}>
      <div className="sub-title">Переставить на место</div>
      <Places
        s={props.s}
        from={ally.position}
        busy={orders.busy}
        onPick={(to) => orders.run(() => api.squadMove(ally.position, to), { ally: to })}
      />
      {ally.reservable && (
        <button
          className="equip-off"
          onClick={() => orders.run(() => api.squadToReserve(ally.position), null)}
          disabled={orders.busy}
        >
          В резерв
        </button>
      )}
      <Dismiss
        name={ally.name}
        label="Выгнать из отряда"
        busy={orders.busy}
        onDismiss={() => orders.run(() => api.squadDismiss(ally.position), null)}
      />
    </div>
  )
}

/** Приказы союзнику из резерва: выставить в строй — на свободное место, а
  * мест нет — вместо того, кого назовут (тот уйдёт в резерв); и выгнать. */
function ReserveOrders(props: { s: SquadView; ally: AllyView; index: number; orders: Orders }) {
  const { s, ally, index, orders } = props
  const [swap, setSwap] = useState(false)
  // Мест в строю — вместе с героем; союзников на одного меньше.
  const full = s.allies.length >= s.positions - 1
  const deploy = (at: number | null) => orders.run(() => api.squadDeploy(index, ally.name, at), null)
  return (
    <div className="squad-orders" onClick={(e) => e.stopPropagation()}>
      {swap ? (
        <>
          <div className="sub-title">В строю нет мест. Кого отправить в резерв взамен?</div>
          <div className="squad-places">
            {s.allies
              .filter((a) => a.reservable)
              .map((a) => (
                <button key={a.position} className="squad-place" onClick={() => deploy(a.position)} disabled={orders.busy}>
                  <span className="squad-place-n">{a.position}</span>
                  <span className="squad-place-who">{a.name}</span>
                </button>
              ))}
          </div>
          <button className="buy-no" onClick={() => setSwap(false)} disabled={orders.busy}>
            Отмена
          </button>
        </>
      ) : (
        <button className="buy-yes" onClick={() => (full ? setSwap(true) : deploy(null))} disabled={orders.busy}>
          Выставить в отряд
        </button>
      )}
      <Dismiss
        name={ally.name}
        label="Выгнать"
        busy={orders.busy}
        onDismiss={() => orders.run(() => api.squadReserveDismiss(index, ally.name), null)}
      />
    </div>
  )
}

function AllyCard(props: {
  ally: AllyView
  /** Что в кружке слева: место в строю, у резерва — «Р». */
  badge: string
  open: boolean
  /** Раскрыть приказы; `null` — сейчас приказывать нельзя: бой. */
  onToggle: (() => void) | null
  /** Приказы — видны, когда карточка раскрыта. */
  children?: ReactNode
}) {
  const a = props.ally
  return (
    <section
      className={`card ally${props.onToggle ? ' ally-tap' : ''}${props.open ? ' ally-open' : ''}`}
      onClick={props.onToggle ?? undefined}
    >
      <div className="ally-head">
        <span className="ally-pos">{props.badge}</span>
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
      {props.open && props.children}
    </section>
  )
}

/** Восставший ждёт у шатра, а мест нет ни в строю, ни в резерве: кем за него
  * пожертвовать — или отпустить его. Жертву подтверждают: ушедшего не вернуть. */
function Risen(props: { s: SquadView; orders: Orders | null }) {
  const { s, orders } = props
  const [victim, setVictim] = useState<AllyView | null>(null)
  return (
    <section className="card card-warn squad-risen">
      <div>💀 {s.pendingRisen} поднялся и ждёт у шатра, но мест нет ни в строю, ни в резерве. Кем пожертвуем?</div>
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

/** «Отряд»: строй по местам и резерв под ним. Нажал на союзника или на себя —
  * куда встать, а союзника ещё отправить в резерв или выгнать; нажал на
  * союзника в резерве — выставить в строй или выгнать; восставший без места —
  * кем за него пожертвовать. Всё как в «Отряде» в боте и где угодно, только не
  * в бою: бот проверит это сам, под тем же замком, что и ходы в чате. Что
  * сказал бот — всплывает над нижней панелью. */
export function SquadTab(props: { data: SquadView; onChange: (v: SquadView) => void }) {
  const s = props.data
  const [pick, setPick] = useState<Pick | null>(null)
  const [busy, setBusy] = useState(false)
  const [toast, setToast] = useToast()

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

  if (s.allies.length === 0 && s.reserve.length === 0)
    return (
      <Empty>
        Союзников пока нет. Наёмники ждут за дальним столом в таверне, а поднятые и крысы находят героя в лабиринте сами.
      </Empty>
    )

  // Места до последнего занятого — по порядку; хвост пустых — одной строкой.
  const last = Math.max(s.heroPosition, ...s.allies.map((a) => a.position))
  const places = Array.from({ length: last }, (_, i) => i + 1)
  const heroOpen = pick === 'hero'
  const openAlly = pickedAlly(pick)
  const openReserve = pickedReserve(pick)

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
        if (ally)
          return (
            <AllyCard
              key={pos}
              ally={ally}
              badge={String(pos)}
              open={openAlly === pos}
              onToggle={orders && toggle({ ally: pos })}
            >
              {orders && <AllyOrders s={s} ally={ally} orders={orders} />}
            </AllyCard>
          )
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
      <div className="sub-title">
        Резерв · {s.reserve.length} из {s.reserveSize}
      </div>
      {s.reserve.length === 0 ? (
        <p className="muted-note">
          Резерв пуст. Сюда встают лишние поднятые и наёмники, когда в строю нет мест, — и те, кого вы отправите сами.
        </p>
      ) : (
        <>
          <p className="muted-note">Они идут с вами, но в бой не вступают; сроки у них идут как в строю.</p>
          {s.reserve.map((a, i) => (
            <AllyCard
              key={`${i}-${a.name}`}
              ally={a}
              badge="Р"
              open={openReserve === i}
              onToggle={orders && toggle({ reserve: i })}
            >
              {orders && <ReserveOrders s={s} ally={a} index={i} orders={orders} />}
            </AllyCard>
          ))}
        </>
      )}
      {toast}
    </div>
  )
}
