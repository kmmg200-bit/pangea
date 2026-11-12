import { useMemo, useState } from 'react'
import { api } from './api'
import { BagTab, matches, sortItems } from './BagTab'
import { Silver } from './icons'
import { groupLabels, presentGroups, type Group as Kind } from './kinds'
import type { BrewAskView, InventoryActionView, InventoryView, ItemView, StorageView } from './types'
import { BeltSwap, Chips, Empty, ItemRow, ItemText, num, Segment, Sheet, Toolbar, useToast } from './ui'

type Group = StorageView['group']

const groups: { id: Group; label: string }[] = [
  { id: 'self', label: 'При себе' },
  { id: 'city', label: 'В городе' },
  { id: 'home', label: 'Дом' },
]

/** Сумка — свой экран; остальные хранилища приходят списком. Снаряжение — во
  * вкладке «Персонаж». */
const BAG = 'bag'

/** Как выполнить действие с хранилищем и занято ли приложение предыдущим. */
interface Acts {
  run: (act: () => Promise<InventoryActionView>) => void
  busy: boolean
}

/** «Переложить» в карточке вещи: хранилища, которые её возьмут и до которых
  * герой сейчас дотянется, и сколько в них свободно. Стопку — по одной вещи
  * или всю. Есть ли место, скажет бот. */
function MovePanel(props: { data: InventoryView; from: string; item: ItemView; acts: Acts; onDone: () => void }) {
  const { data, from, item, acts } = props
  const [all, setAll] = useState(false)
  const move = data.moves.find((m) => m.from === from && m.item === item.id)
  if (!move) return null
  const { to, far } = move
  const why = far && <p className="sheet-note">{data.storesAway}</p>
  if (to.length === 0) return <>{why}</>
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
      {why}
    </div>
  )
}

/** «Выбросить» в карточке вещи: из сумки и из хранилищ, до которых герой
  * сейчас дотянется, — всё, кроме сюжетного. Стопку — одну вещь или всю. Сперва
  * переспросить: вернуть вещь не получится. */
function DropPanel(props: { data: InventoryView; from: string; item: ItemView; acts: Acts; onDone: () => void }) {
  const { data, from, item, acts } = props
  // Что переспрашиваем: одну вещь или всю стопку.
  const [ask, setAsk] = useState<'one' | 'all' | null>(null)
  // Бот старше приложения — поля нет: нет и кнопки.
  if (!(data.drops ?? []).some((d) => d.from === from && d.item === item.id)) return null
  const what = ask === 'all' ? `«${item.title}» ×${item.count}` : `«${item.title}»`
  return (
    <div className="move">
      <div className="equip-acts">
        <button className="equip-off" disabled={acts.busy} onClick={() => setAsk('one')}>
          Выбросить
        </button>
        {item.count > 1 && (
          <button className="equip-off" disabled={acts.busy} onClick={() => setAsk('all')}>
            Выбросить все {item.count}
          </button>
        )}
      </div>
      {ask && (
        <div className="buy-confirm">
          <p className="buy-ask">Выбросить {what}? Вернуть не получится.</p>
          <div className="buy-row">
            <button
              className="buy-yes"
              disabled={acts.busy}
              onClick={() => {
                acts.run(() => api.storeDrop(from, item.id, ask === 'all'))
                props.onDone()
              }}
            >
              Выбросить
            </button>
            <button className="buy-no" onClick={() => setAsk(null)}>
              Передумал
            </button>
          </div>
        </div>
      )}
    </div>
  )
}

/** Вопрос отвара: почему нельзя, на что потратить или тратить ли склянку. Выбор
  * травмы, рода или фляги сам и есть подтверждение. */
function BrewQuestion(props: { ask: BrewAskView; busy: boolean; onUse: (arg: string | null) => void; onBack: () => void }) {
  const { ask, busy } = props
  if (ask.refusal)
    return (
      <div className="buy-confirm">
        <p className="buy-ask">{ask.refusal}</p>
      </div>
    )
  return (
    <div className="buy-confirm">
      <p className="buy-ask">{ask.question}</p>
      {ask.hint && <p className="sheet-note">{ask.hint}</p>}
      {ask.options.length > 0 ? (
        <div className="move-list">
          {ask.options.map((o) => (
            <button key={o.id} className="move-to" disabled={busy} onClick={() => props.onUse(o.id)}>
              {o.label}
            </button>
          ))}
        </div>
      ) : (
        <div className="buy-row">
          <button className="buy-yes" disabled={busy} onClick={() => props.onUse(null)}>
            {ask.label}
          </button>
          <button className="buy-no" onClick={props.onBack}>
            Передумал
          </button>
        </div>
      )}
    </div>
  )
}

/** Что сделать с вещью из сумки — те же кнопки, что на её карточке в боте. */
function BagActs(props: { data: InventoryView; item: ItemView; acts: Acts; onDone: () => void }) {
  const { data, item, acts } = props
  // Открытый вопрос: название действия, `belt` — что снять с полного пояса,
  // `crush` — колоть ли камень.
  const [ask, setAsk] = useState<string | null>(null)
  const act = data.bagActs.find((a) => a.item === item.id)
  if (!act) return null
  const finish = (run: () => Promise<InventoryActionView>) => {
    acts.run(run)
    props.onDone()
  }
  const brew = act.brew.find((a) => a.action === ask)
  const belt = data.equipment.find((s) => s.belt)?.belt
  return (
    <div className="move">
      <div className="equip-acts">
        {act.brew.map((a) => (
          <button key={a.action} className="equip-off" disabled={acts.busy} onClick={() => setAsk(a.action)}>
            {a.label}
          </button>
        ))}
        {act.belt && (
          <button
            className="equip-off"
            disabled={acts.busy}
            onClick={() => (act.beltFull ? setAsk('belt') : finish(() => api.beltPut(item.id, null)))}
          >
            На пояс
          </button>
        )}
        {act.unpack && (
          <button className="equip-off" disabled={acts.busy} onClick={() => finish(() => api.bagUnpack(item.id))}>
            Распаковать
          </button>
        )}
        {act.crush && (
          <button className="equip-off" disabled={acts.busy} onClick={() => setAsk('crush')}>
            Расколоть камень
          </button>
        )}
      </div>
      {ask === 'crush' && act.crush && (
        <div className="buy-confirm">
          <p className="buy-ask">{act.crush}</p>
          <div className="buy-row">
            <button className="buy-yes" disabled={acts.busy} onClick={() => finish(() => api.bagCrushGem(item.id))}>
              Ломать
            </button>
            <button className="buy-no" onClick={() => setAsk(null)}>
              Передумал
            </button>
          </div>
        </div>
      )}
      {brew && (
        <BrewQuestion
          ask={brew}
          busy={acts.busy}
          onUse={(arg) => finish(() => api.brewUse(brew.action, item.id, arg))}
          onBack={() => setAsk(null)}
        />
      )}
      {ask === 'belt' && act.beltFull && belt && (
        <BeltSwap ask={act.beltFull} belt={belt} busy={acts.busy} onPick={(cell) => finish(() => api.beltPut(item.id, cell))} />
      )}
    </div>
  )
}

/** Сортировка в хранилище — та же, что в сумке, только первая — «как лежит». */
const storeSorts = [
  { id: 'bag', label: 'Как в хранилище' },
  { id: 'rarity', label: 'По редкости' },
  { id: 'lvl', label: 'По уровню' },
]

/** Хранилище: поиск, сортировка и группы — как в сумке (см. BagTab), по всем
  * его разделам сразу. У оружейных стоек поиска нет: там по нескольку вещей на
  * стойку и «Надеть» — весь комплект, фильтр только запутал бы. */
function Storage(props: { storage: StorageView; onOpen: (i: ItemView) => void; acts: Acts; gearLocked: string | null }) {
  const s = props.storage
  const { acts } = props
  const racks = s.sections.some((sec) => sec.rack !== null)
  const [kind, setKind] = useState<Kind | 'all'>('all')
  const [query, setQuery] = useState('')
  const [sort, setSort] = useState('bag')
  const all = useMemo(() => s.sections.flatMap((sec) => sec.items), [s.sections])
  const kinds = useMemo(() => presentGroups(all), [all])
  // Выбранной группы в хранилище больше нет (последнюю такую вещь переложили) —
  // показываем всё: иначе фильтр остался бы, а снять его было бы нечем.
  const active: Kind | 'all' = kind !== 'all' && kinds.includes(kind) ? kind : 'all'
  const searchable = !s.locked && !racks && all.length > 0
  const filtering = searchable && (query.trim() !== '' || active !== 'all')
  const pick = (items: ItemView[]) => (searchable ? sortItems(items.filter((i) => matches(i, active, query)), sort) : items)
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
      {racks && props.gearLocked && <section className="card card-warn storage-note">{props.gearLocked}</section>}
      {empty && <Empty>Пусто.</Empty>}
      {searchable && (
        <>
          <Toolbar query={query} onQuery={setQuery} sort={sort} sorts={storeSorts} onSort={setSort} />
          {kinds.length > 1 && (
            <Chips
              options={[{ id: 'all' as const, label: 'Всё' }, ...kinds.map((g) => ({ id: g, label: groupLabels[g] }))]}
              value={active}
              onChange={setKind}
            />
          )}
        </>
      )}
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
      {s.sections.map((sec, i) => {
        const items = pick(sec.items)
        // Пустой раздел без заголовка не показываем; при поиске — и с заголовком.
        if (items.length === 0 && (!sec.title || filtering)) return null
        return (
          <div key={i} className="group">
            {sec.rack !== null ? (
              <div className="group-label rack-head">
                <span>{sec.title}</span>
                {!props.gearLocked && (
                  <button className="rack-wear" onClick={() => acts.run(() => api.rackWear(sec.rack ?? 0))} disabled={acts.busy}>
                    Надеть
                  </button>
                )}
              </div>
            ) : (
              sec.title && <div className="group-label">{sec.title}</div>
            )}
            <div className="list">
              {items.map((item) => (
                <ItemRow key={item.id} item={item} onOpen={() => props.onOpen(item)} />
              ))}
              {items.length === 0 && <p className="muted-note">Пусто.</p>}
            </div>
          </div>
        )
      })}
      {filtering && s.sections.every((sec) => pick(sec.items).length === 0) && <Empty>Ничего не нашлось.</Empty>}
    </>
  )
}

/** «Вещи»: при себе, в городе и в доме. Закрытое хранилище видно с замком —
  * нажал, и написано, как его открыть. Вещь из любого хранилища — в любое,
  * которое её возьмёт (из карточки вещи), — в городе; вне города — между тем,
  * что при себе. Комплект со стойки — на героя, отвар и пояс — из карточки вещи
  * в сумке, где угодно. Выбросить — из карточки вещи, откуда её можно и
  * переложить. В бою — ни то ни другое. Бот проверит это сам, под тем же замком, что и ходы в чате. Что
  * сказал бот — всплывает над нижней панелью. */
export function ItemsTab(props: { data: InventoryView; onChange: (v: InventoryView) => void }) {
  const [group, setGroup] = useState<Group>('self')
  const [picked, setPicked] = useState<Record<Group, string>>({ self: BAG, city: '', home: '' })
  // Открытая карточка вещи и откуда вещь: `bag` — сумка, иначе — хранилище.
  const [open, setOpen] = useState<{ item: ItemView; from: string } | null>(null)
  const [busy, setBusy] = useState(false)
  const [toast, setToast] = useToast()

  const acts: Acts = {
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
          // У каждого хранилища свой поиск: перешёл в другое — всё сначала, как из сумки.
          key={storage.id}
          storage={storage}
          onOpen={(item) => setOpen({ item, from: storage.id })}
          acts={acts}
          gearLocked={props.data.gearLocked}
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
          {open.from === BAG && <BagActs data={props.data} item={open.item} acts={acts} onDone={() => setOpen(null)} />}
          <MovePanel data={props.data} from={open.from} item={open.item} acts={acts} onDone={() => setOpen(null)} />
          <DropPanel data={props.data} from={open.from} item={open.item} acts={acts} onDone={() => setOpen(null)} />
        </Sheet>
      )}
      {toast}
    </>
  )
}
