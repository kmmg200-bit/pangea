import { useState } from 'react'
import { kindOf } from './kinds'
import type { BeltView, InventoryView, ItemView } from './types'
import { ItemText, num, Sheet } from './ui'

/** Что в поясе — по ячейкам, со счётом и полоской; свободные — одной строкой. */
function BeltCard(props: { belt: BeltView }) {
  const { capacity, cells } = props.belt
  const free = capacity - cells.length
  return (
    <section className="card belt">
      <div className="card-title">
        🎽 В поясе<span className="card-aside">ячеек занято {num(cells.length)} из {num(capacity)}</span>
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
    </section>
  )
}

export function EquipmentTab(props: { data: InventoryView }) {
  const [open, setOpen] = useState<ItemView | null>(null)
  return (
    <>
      <div className="list">
        {props.data.equipment.map((slot) =>
          slot.item ? (
            <div key={slot.name} className="slot">
              <button className={`row rarity-${slot.item.rarity}`} onClick={() => slot.item && setOpen(slot.item)}>
                <span className="row-icon" aria-hidden>
                  {kindOf(slot.item).icon}
                </span>
                <span className="row-body">
                  <span className="row-sub">{slot.name}</span>
                  <span className="row-title">{slot.item.title}</span>
                </span>
              </button>
              {slot.belt && <BeltCard belt={slot.belt} />}
            </div>
          ) : slot.busy ? (
            <div key={slot.name} className="row row-empty">
              <span className="row-icon" aria-hidden />
              <span className="row-body">
                <span className="row-sub">{slot.name}</span>
                <span className="row-title">{slot.busy}</span>
              </span>
            </div>
          ) : (
            <div key={slot.name} className="row row-empty row-slim">
              <span className="row-icon" aria-hidden />
              <span className="row-body">{slot.name}</span>
              <span className="row-sub">пусто</span>
            </div>
          ),
        )}
      </div>
      {open && (
        <Sheet onClose={() => setOpen(null)}>
          <ItemText item={open} />
        </Sheet>
      )}
    </>
  )
}
