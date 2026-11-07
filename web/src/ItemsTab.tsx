import { useMemo, useState } from 'react'
import { BagTab } from './BagTab'
import { EquipmentTab } from './EquipmentTab'
import { Silver } from './icons'
import type { InventoryView, ItemView, StorageView } from './types'
import { Chips, Empty, ItemRow, ItemText, num, Sheet } from './ui'

type Group = StorageView['group']

const groups: { id: Group; label: string }[] = [
  { id: 'self', label: 'При себе' },
  { id: 'city', label: 'В городе' },
  { id: 'home', label: 'Дом' },
]

/** Сумка и снаряжение — свои экраны; остальные хранилища приходят списком. */
const BAG = 'bag'
const EQUIPMENT = 'equipment'

function Storage(props: { storage: StorageView; onOpen: (i: ItemView) => void }) {
  const s = props.storage
  if (s.locked)
    return (
      <div className="locked">
        <div className="locked-icon" aria-hidden>
          🔒
        </div>
        <div className="locked-title">{s.title}</div>
        <p className="muted-note">{s.locked}</p>
      </div>
    )
  const empty = s.sections.every((sec) => sec.items.length === 0) && s.counts.length === 0
  return (
    <>
      {(s.used !== null || s.silver) && (
        <div className="summary">
          {s.used !== null && s.capacity !== null && (
            <span>
              Занято {num(s.used)} из {num(s.capacity)}
            </span>
          )}
          {s.silver && (
            <span>
              {' · '}
              <Silver /> {num(s.silver.current)} из {num(s.silver.max)}
            </span>
          )}
        </div>
      )}
      {s.note && <section className="card card-warn storage-note">{s.note}</section>}
      {empty && <Empty>Пусто.</Empty>}
      {s.counts.length > 0 && (
        <div className="list">
          {s.counts.map((c) => (
            <div key={c.label} className="row row-plain row-static">
              <span className="row-body">
                <span className="row-title">{c.label}</span>
              </span>
              <span className="badge">{num(c.value)}</span>
            </div>
          ))}
        </div>
      )}
      {s.sections.map((sec, i) =>
        sec.items.length === 0 && !sec.title ? null : (
          <div key={i} className="group">
            {sec.title && <div className="group-label">{sec.title}</div>}
            <div className="list">
              {sec.items.map((item) => (
                <ItemRow key={item.id} item={item} onOpen={() => props.onOpen(item)} />
              ))}
              {sec.items.length === 0 && <p className="muted-note">Пусто.</p>}
            </div>
          </div>
        ),
      )}
    </>
  )
}

/** «Вещи»: при себе, в городе и в доме. Закрытое хранилище видно с замком —
  * нажал, и написано, как его открыть. */
export function ItemsTab(props: { data: InventoryView }) {
  const [group, setGroup] = useState<Group>('self')
  const [picked, setPicked] = useState<Record<Group, string>>({ self: BAG, city: '', home: '' })
  const [open, setOpen] = useState<ItemView | null>(null)

  const inGroup = useMemo(() => props.data.storages.filter((s) => s.group === group), [props.data.storages, group])
  const options = [
    ...(group === 'self'
      ? [
          { id: BAG, label: 'Сумка' },
          { id: EQUIPMENT, label: 'Снаряжение' },
        ]
      : []),
    ...inGroup.map((s) => ({ id: s.id, label: s.locked ? `🔒 ${s.title}` : s.title })),
  ]
  // Первое открытое хранилище группы, пока игрок ничего не выбрал.
  const current =
    picked[group] || inGroup.find((s) => !s.locked)?.id || options[0]?.id || BAG
  const storage = inGroup.find((s) => s.id === current)

  return (
    <>
      <div className="segment" role="tablist">
        {groups.map((g) => (
          <button
            key={g.id}
            role="tab"
            aria-selected={g.id === group}
            className={g.id === group ? 'segment-item segment-on' : 'segment-item'}
            onClick={() => setGroup(g.id)}
          >
            {g.label}
          </button>
        ))}
      </div>
      <Chips options={options} value={current} onChange={(id) => setPicked({ ...picked, [group]: id })} />
      {current === BAG && group === 'self' ? (
        <BagTab data={props.data} />
      ) : current === EQUIPMENT && group === 'self' ? (
        <EquipmentTab data={props.data} />
      ) : storage ? (
        <Storage storage={storage} onOpen={setOpen} />
      ) : null}
      {open && (
        <Sheet onClose={() => setOpen(null)}>
          <ItemText item={open} />
          {open.count > 1 && <p className="sheet-note">Здесь: {open.count} шт.</p>}
        </Sheet>
      )}
    </>
  )
}
