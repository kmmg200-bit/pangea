import { useState } from 'react'
import { api } from './api'
import type { GameIcon } from './gameIcons'
import type { BeltView, CandidateView, InventoryActionView, InventoryView, ItemView, SlotView, StatChange } from './types'
import { ItemText, ItemTile, num, Sheet, Tile } from './ui'

/** Что в поясе — по ячейкам, со счётом и полоской; свободные — одной строкой. */
function BeltCard(props: { belt: BeltView }) {
  const { capacity, cells } = props.belt
  const free = capacity - cells.length
  return (
    <div className="belt">
      <div className="sub-title">
        В поясе · ячеек занято {num(cells.length)} из {num(capacity)}
      </div>
      {cells.map((c, i) => {
        const pct = c.max > 0 ? Math.min(100, (c.uses / c.max) * 100) : 0
        return (
          <div key={i} className="meter">
            <div className="meter-top">
              <span className={c.uses === 0 ? 'belt-spent' : undefined}>{c.name}</span>
              <span className="meter-num">{c.uses === 0 ? 'пусто' : `${num(c.uses)} / ${num(c.max)}`}</span>
            </div>
            <div className="meter-track">
              <span className="meter-fill tone-energy" style={{ width: `${pct}%` }} />
            </div>
          </div>
        )
      })}
      {cells.length === 0 && <p className="muted-note">Пояс пуст.</p>}
      {free > 0 && cells.length > 0 && <p className="muted-note">Свободных ячеек: {num(free)}.</p>}
    </div>
  )
}

/** Значки и подписи статов — те же, что в текстах бота. */
const statInfo: Record<string, { icon: string; label: string }> = {
  attack: { icon: '⚔', label: 'Атака' },
  defence: { icon: '🛡', label: 'Защита' },
  armor: { icon: '🧥', label: 'Броня' },
  hp: { icon: '❤', label: 'HP' },
  evasion: { icon: '👁', label: 'Уклонение' },
  accuracy: { icon: '🎯', label: 'Точность' },
  energy: { icon: '⚡', label: 'Энергия' },
}

const signed = (v: number) => (v > 0 ? `+${num(v)}` : `−${num(-v)}`)

/** Плашка стата. `long` — со словом («🧥 Броня +40»); `delta` — разница при
  * замене: прибавка зелёная, потеря красная. */
function Chip(props: { stat: StatChange; long?: boolean; delta?: boolean }) {
  const info = statInfo[props.stat.key] ?? { icon: '', label: props.stat.key }
  const tone = props.delta ? (props.stat.value > 0 ? ' chip-up' : ' chip-down') : ''
  return (
    <span className={`chip-stat${tone}`}>
      {info.icon} {props.long ? `${info.label} ` : ''}
      {signed(props.stat.value)}
    </span>
  )
}

/** Значок пустого слота — чем его можно занять. */
const slotIcons: Record<string, GameIcon> = {
  Шлем: 'visored-helm',
  Наплечники: 'pauldrons',
  Нагрудник: 'chest-armor',
  Браслеты: 'bracer',
  Перчатки: 'gauntlet',
  Штаны: 'armored-pants',
  Сапоги: 'leather-boot',
  Амулет: 'gem-pendant',
  'Кольцо 1': 'diamond-ring',
  'Кольцо 2': 'diamond-ring',
  Пояс: 'belt',
  'Правая рука': 'broadsword',
  'Левая рука': 'checked-shield',
  'Доп. оружие': 'plain-dagger',
}

/** Что ещё есть у вещи, кроме статов: умение, пассивка, камни, набор. */
function extras(item: ItemView): string[] {
  return [
    item.skill && `«${item.skill}»`,
    item.passive && `«${item.passive}»`,
    item.sockets > 0 && `💎 ${num(item.gems)} из ${num(item.sockets)}`,
    item.set && `набор «${item.set}»`,
  ].filter((x): x is string => typeof x === 'string')
}

/** Надеть и снять; `busy` — ждём ответа бота. Нет его вовсе (`null`) — герой в бою. */
interface Gear {
  wear: (slot: SlotView, item: ItemView) => void
  takeOff: (slot: SlotView) => void
  busy: boolean
}

function Candidate(props: {
  c: CandidateView
  onOpen: (i: ItemView) => void
  /** Надеть — `null`, когда сейчас нельзя: бой. */
  onWear: (() => void) | null
  busy: boolean
}) {
  const { item, deltas, set } = props.c
  return (
    <div className={`cand rarity-${item.rarity}`}>
      <button className="cand-main" onClick={() => props.onOpen(item)}>
        <span className="cand-top">
          <ItemTile item={item} />
          <span className="row-title">
            {item.title}
            {item.count > 1 && <span className="row-sub"> ×{num(item.count)}</span>}
          </span>
          <span className="lvl">ур. {num(item.lvl)}</span>
        </span>
        <span className="chips-row">
          {deltas.map((d) => (
            <Chip key={d.key} stat={d} delta />
          ))}
          {deltas.length === 0 && <span className="row-sub">статы не изменятся</span>}
          {set && set.after !== set.before && (
            <span className="chip-stat">
              «{set.name}» {num(set.before)} → {num(set.after)} из {num(set.max)}
            </span>
          )}
        </span>
      </button>
      {props.onWear && (
        <button className="cand-wear" onClick={props.onWear} disabled={props.busy}>
          Надеть
        </button>
      )}
    </div>
  )
}

/** Слот строкой: вещь, уровень, главные статы. Нажал — раскрывается: описание
  * надетой (у пояса — его ячейки) и что из сумки встанет сюда, с разницей к
  * надетому. */
function SlotRow(props: {
  slot: SlotView
  open: boolean
  onToggle: () => void
  onOpen: (i: ItemView) => void
  gear: Gear | null
  note: string | null
}) {
  const { slot, open, gear } = props
  const item = slot.item
  const hasMore = !!item || slot.candidates.length > 0
  const dot = slot.candidates.length > 0 && <span className="dot" title="в сумке есть вещь на этот слот" />
  return (
    <div className={`erow${item ? ` rarity-${item.rarity}` : ' erow-empty'}${open ? ' erow-open' : ''}`}>
      <button className="erow-top" onClick={props.onToggle} disabled={!hasMore} aria-expanded={open}>
        {item ? <ItemTile item={item} /> : <Tile icon={slotIcons[slot.name] ?? 'swap-bag'} />}
        <span className="row-body">
          {item ? (
            <>
              <span className="row-sub">{slot.name}</span>
              <span className="row-title">{item.title}</span>
              <span className="chips-row">
                {item.stats.map((st) => (
                  <Chip key={st.key} stat={st} />
                ))}
                {extras(item).map((x) => (
                  <span key={x} className="chip-stat">
                    {x}
                  </span>
                ))}
              </span>
            </>
          ) : slot.busy ? (
            <>
              <span className="row-sub">{slot.name}</span>
              <span className="row-title">{slot.busy}</span>
            </>
          ) : (
            <span className="row-title">{slot.name}</span>
          )}
        </span>
        <span className="erow-side">
          {item ? <span className="lvl">ур. {num(item.lvl)}</span> : !slot.busy && <span className="row-sub">пусто</span>}
          {hasMore && (
            <span className="erow-marks">
              {dot}
              <span className="erow-chev" aria-hidden>
                {open ? '▾' : '›'}
              </span>
            </span>
          )}
        </span>
      </button>
      {open && (
        <div className="erow-body">
          {props.note && <p className="gear-note">{props.note}</p>}
          {/* У пояса описание — его ячейки; у прочих — что вещь даёт, как в боте. */}
          {slot.belt ? <BeltCard belt={slot.belt} /> : item && <div className="item-text equip-desc">{item.text}</div>}
          {item && gear && (
            <button className="equip-off" onClick={() => gear.takeOff(slot)} disabled={gear.busy}>
              Снять
            </button>
          )}
          {slot.candidates.map((c) => (
            <Candidate
              key={c.item.id}
              c={c}
              onOpen={props.onOpen}
              onWear={gear && (() => gear.wear(slot, c.item))}
              busy={!!gear?.busy}
            />
          ))}
        </div>
      )}
    </div>
  )
}

/** «Снаряжение»: все слоты подряд, как в боте. Зелёная точка — в сумке есть
  * вещь на этот слот. Надевают и снимают здесь же — где угодно, только не в
  * бою; бот проверит это сам, под тем же замком, что и ходы в чате. */
export function EquipmentTab(props: { data: InventoryView; onChange: (v: InventoryView) => void }) {
  const [openSlot, setOpenSlot] = useState<string | null>(null)
  const [sheet, setSheet] = useState<ItemView | null>(null)
  const [busy, setBusy] = useState(false)
  // Что сказал бот — под тем слотом, с которым что-то делали.
  const [note, setNote] = useState<{ slot: number; text: string } | null>(null)

  const run = (slot: SlotView, act: () => Promise<InventoryActionView>) => {
    setBusy(true)
    setNote(null)
    act()
      .then((r) => {
        props.onChange(r.inventory)
        if (r.note) setNote({ slot: slot.index, text: r.note })
      })
      .catch(() => setNote({ slot: slot.index, text: 'Ответ не дошёл. Обновите (⟳) и проверьте снаряжение.' }))
      .finally(() => setBusy(false))
  }

  const gear: Gear | null = props.data.gearLocked
    ? null
    : {
        wear: (slot, item) => run(slot, () => api.equipWear(slot.index, item.id)),
        takeOff: (slot) => run(slot, () => api.equipTakeOff(slot.index)),
        busy,
      }

  return (
    <>
      {props.data.gearLocked && <section className="card card-warn storage-note">{props.data.gearLocked}</section>}
      <p className="muted-note equip-hint">
        <span className="dot" />
        <span>— в сумке есть вещь на этот слот. Нажмите на строку.</span>
      </p>
      <div className="list">
        {props.data.equipment.map((slot) => (
          <SlotRow
            key={slot.name}
            slot={slot}
            open={openSlot === slot.name}
            onToggle={() => setOpenSlot(openSlot === slot.name ? null : slot.name)}
            onOpen={setSheet}
            gear={gear}
            note={note?.slot === slot.index ? note.text : null}
          />
        ))}
      </div>
      {sheet && (
        <Sheet onClose={() => setSheet(null)}>
          <ItemText item={sheet} />
        </Sheet>
      )}
    </>
  )
}
