import { useState } from 'react'
import { kindOf } from './kinds'
import type { InventoryView, ItemView } from './types'
import { ItemText, Sheet } from './ui'

export function EquipmentTab(props: { data: InventoryView }) {
  const [open, setOpen] = useState<ItemView | null>(null)
  return (
    <>
      <div className="list">
        {props.data.equipment.map((slot) =>
          slot.item ? (
            <button
              key={slot.name}
              className={`row rarity-${slot.item.rarity}`}
              onClick={() => slot.item && setOpen(slot.item)}
            >
              <span className="row-icon" aria-hidden>
                {kindOf(slot.item).icon}
              </span>
              <span className="row-body">
                <span className="row-sub">{slot.name}</span>
                <span className="row-title">{slot.item.title}</span>
              </span>
            </button>
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
