import { useMemo, useState } from 'react'
import { api } from './api'
import { BagTab } from './BagTab'
import { Silver } from './icons'
import type { InventoryActionView, InventoryView, ItemView, StorageView } from './types'
import { Chips, Empty, ItemRow, ItemText, num, Segment, Sheet, useToast } from './ui'

type Group = StorageView['group']

const groups: { id: Group; label: string }[] = [
  { id: 'self', label: 'При себе' },
  { id: 'city', label: 'В городе' },
  { id: 'home', label: 'Дом' },
]

/** Сумка — свой экран; остальные хранилища приходят списком. Снаряжение — во
  * вкладке «Персонаж». */
const BAG = 'bag'

/** Что можно сделать с хранилищем; нет (`null`) — герой не в городе. */
interface Acts {
  run: (act: () => Promise<InventoryActionView>) => void
  busy: boolean
}

/** «Переложить» в карточке вещи: хранилища, которые её возьмут, и сколько в
  * них свободно. Стопку — по одной вещи или всю. Есть ли место, скажет бот. */
function MovePanel(props: { data: InventoryView; from: string; item: ItemView; acts: Acts | null; onDone: () => void }) {
  const { data, from, item, acts } = props
  const [all, setAll] = useState(false)
  const to = data.moves.find((m) => m.from === from && m.item === item.id)?.to ?? []
  if (to.length === 0) return null
  if (!acts) return <p className="sheet-note">{data.storesAway}</p>
  const place = (id: string) => {
    if (id === BAG) return { title: 'Сумка', free: data.capacity - data.occupied }
    const s = data.storages.find((x) => x.id === id)
    const free = s && s.used !== null && s.capacity !== null ? s.capacity - s.used : null
    return { title: s?.title ?? id, free }
  }
  return (
    <div className="move">
      <div className="sub-title">Переложить</div>
      {item.count > 1 && (
        <Segment
          options={[
            { id: 'one', label: 'Одну' },
            { id: 'all', label: `Все ${item.count}` },
          ]}
          value={all ? 'all' : 'one'}
          onChange={(v) => setAll(v === 'all')}
        />
      )}
      <div className="move-list">
        {to.map((id) => {
          const p = place(id)
          return (
            <button
              key={id}
              className="move-to"
              disabled={acts.busy}
              onClick={() => {
                acts.run(() => api.storeMove(from, id, item.id, all))
                props.onDone()
              }}
            >
              <span>{p.title}</span>
              {p.free !== null && <span className="move-free">свободно {num(Math.max(0, p.free))}</span>}
            </button>
          )
        })}
      </div>
    </div>
  )
}

function Storage(props: { storage: StorageView; onOpen: (i: ItemView) => void; acts: Acts | null; away: string | null }) {
  const s = props.storage
  const { acts } = props
  const racks = s.sections.some((sec) => sec.rack !== null)
  if (s.locked)
    return (
      <div className="storage-locked">
        <div className="storage-locked-icon" aria-hidden>
          🔒
        </div>
        <div className="storage-locked-title">{s.title}</div>
        <p className="muted-note">{s.locked}</p>
      </div>
    )
  // Разделы с заголовком (стойки) пишут «Пусто.» каждый у себя.
  const empty = s.sections.every((sec) => sec.items.length === 0 && !sec.title) && s.counts.length === 0
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
      {racks && props.away && <section className="card card-warn storage-note">{props.away}</section>}
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
            {sec.rack !== null ? (
              <div className="group-label rack-head">
                <span>{sec.title}</span>
                {acts && (
                  <button className="rack-wear" onClick={() => acts.run(() => api.rackWear(sec.rack ?? 0))} disabled={acts.busy}>
                    Надеть
                  </button>
                )}
              </div>
            ) : (
              sec.title && <div className="group-label">{sec.title}</div>
            )}
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
  * нажал, и написано, как его открыть. Вещь из любого хранилища — в любое,
  * которое её возьмёт (из карточки вещи), а комплект со стойки — на героя;
  * то и другое только в городе: бот проверит это сам, под тем же замком,
  * что и ходы в чате. Что сказал бот — всплывает над нижней панелью. */
export function ItemsTab(props: { data: InventoryView; onChange: (v: InventoryView) => void }) {
  const [group, setGroup] = useState<Group>('self')
  const [picked, setPicked] = useState<Record<Group, string>>({ self: BAG, city: '', home: '' })
  // Открытая карточка вещи и откуда вещь: `bag` — сумка, иначе — хранилище.
  const [open, setOpen] = useState<{ item: ItemView; from: string } | null>(null)
  const [busy, setBusy] = useState(false)
  const [toast, setToast] = useToast()

  const acts: Acts | null = props.data.storesAway
    ? null
    : {
        busy,
        run: (act) => {
          setBusy(true)
          act()
            .then((r) => {
              props.onChange(r.inventory)
              if (r.note) setToast(r.note)
            })
            .catch(() => setToast('Ответ не дошёл. Обновите (⟳) и проверьте вещи.'))
            .finally(() => setBusy(false))
        },
      }

  const inGroup = useMemo(() => props.data.storages.filter((s) => s.group === group), [props.data.storages, group])
  const options = [
    ...(group === 'self' ? [{ id: BAG, label: 'Сумка' }] : []),
    ...inGroup.map((s) => ({ id: s.id, label: s.locked ? `🔒 ${s.title}` : s.title })),
  ]
  // Первое открытое хранилище группы, пока игрок ничего не выбрал.
  const current =
    picked[group] || inGroup.find((s) => !s.locked)?.id || options[0]?.id || BAG
  const storage = inGroup.find((s) => s.id === current)

  return (
    <>
      <Segment options={groups} value={group} onChange={setGroup} />
      <Chips options={options} value={current} onChange={(id) => setPicked({ ...picked, [group]: id })} />
      {current === BAG && group === 'self' ? (
        <BagTab data={props.data} onOpen={(item) => setOpen({ item, from: BAG })} />
      ) : storage ? (
        <Storage
          storage={storage}
          onOpen={(item) => setOpen({ item, from: storage.id })}
          acts={acts}
          away={props.data.storesAway}
        />
      ) : null}
      {open && (
        <Sheet onClose={() => setOpen(null)}>
          <ItemText item={open.item} />
          {open.item.count > 1 && (
            <p className="sheet-note">
              {open.from === BAG ? 'В сумке' : 'Здесь'}: {open.item.count} шт.
            </p>
          )}
          <MovePanel data={props.data} from={open.from} item={open.item} acts={acts} onDone={() => setOpen(null)} />
        </Sheet>
      )}
      {toast}
    </>
  )
}
