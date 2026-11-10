import { useEffect, useState } from 'react'
import { api, ApiError } from './api'
import { iconOf } from './kinds'
import type { CraftActionView, CraftView, CubeView, ItemView } from './types'
import { Icon, ItemText, Meter, num, Sheet, tone } from './ui'
import { useDrag, type Zone } from './useDrag'

/** Ячейка куба: вещь — плиткой цвета редкости, пустая — пунктиром. `fresh` —
  * только что получилась в кубе: светится. */
function CubeCell(props: {
  item: ItemView | null
  pending: boolean
  fresh: boolean
  lifted: boolean
  bind: object
  onOpen: () => void
}) {
  const { item } = props
  if (!item) return <span className="cube-cell cube-cell-empty" />
  const cls = [
    'cube-cell',
    `rarity-${tone(item)}`,
    props.pending && 'cube-cell-pending',
    props.fresh && 'cube-cell-new',
    props.lifted && 'cell-lifted',
  ]
    .filter(Boolean)
    .join(' ')
  return (
    <button className={cls} {...props.bind} onClick={props.onOpen} aria-label={item.title}>
      <Icon name={iconOf(item)} size={28} />
    </button>
  )
}

/** Вещь сумки плиткой: значок, название в две строки, счёт стопки. */
function BagCell(props: { item: ItemView; lifted: boolean; bind: object; onOpen: () => void }) {
  const { item } = props
  return (
    <button
      className={`bag-cell rarity-${tone(item)}${props.lifted ? ' cell-lifted' : ''}`}
      {...props.bind}
      onClick={props.onOpen}
    >
      <span className="bag-cell-tile">
        <Icon name={iconOf(item)} size={26} />
        {item.count > 1 && <span className="bag-cell-count">{num(item.count)}</span>}
      </span>
      <span className="bag-cell-title">{item.title}</span>
    </button>
  )
}

/** Куб до того, как ожил: замок и что сделать. */
function Locked(props: { cube: CubeView }) {
  return (
    <div className="storage-locked">
      <div className="storage-locked-icon" aria-hidden>
        🔒
      </div>
      <div className="storage-locked-title">Куб Азата</div>
      <p className="muted-note">{props.cube.locked}</p>
    </div>
  )
}

/** Убрать одну вещь стопки: последнюю — вместе со стопкой. */
const takeOne = (bag: ItemView[], item: ItemView) =>
  bag.flatMap((i) => (i.id !== item.id ? [i] : i.count > 1 ? [{ ...i, count: i.count - 1 }] : []))

/** Куб Азата: сверху ячейки и кнопки — они не уезжают, пока листаешь сумку;
  * ниже — сумка. Вещь переносят пальцем: зажал — взялась, отпустил над кубом —
  * легла. Пока ответ бота не пришёл, вещь уже стоит на новом месте, а брать
  * следующую нельзя — номера вещей в стопках обновит ответ. */
export function CraftTab(props: { data: CraftView }) {
  const [view, setView] = useState(props.data)
  const [busy, setBusy] = useState(false)
  const [note, setNote] = useState<string | null>(null)
  const [fresh, setFresh] = useState<number[]>([])
  const [pending, setPending] = useState<number | null>(null)
  const [open, setOpen] = useState<ItemView | null>(null)
  useEffect(() => setView(props.data), [props.data])

  const cube = view.cube
  const canWork = cube.status === 'active' && !cube.away && !busy

  const run = (optimistic: CraftView | null, act: () => Promise<CraftActionView>, pendingId: number | null = null) => {
    const before = view
    if (optimistic) setView(optimistic)
    setPending(pendingId)
    setBusy(true)
    setNote(null)
    act()
      .then((r) => {
        setView(r.craft)
        setNote(r.note)
        setFresh(r.made)
      })
      .catch((e: unknown) => {
        setView(before)
        setNote(e instanceof ApiError && e.code === 'offline' ? 'Нет связи — вещь осталась на месте.' : 'Не вышло — попробуйте ещё раз.')
      })
      .finally(() => {
        setBusy(false)
        setPending(null)
      })
  }

  const onDrop = (item: ItemView, from: Zone, to: Zone) => {
    if (from === 'bag' && to === 'cube') {
      if (cube.items.length >= cube.capacity) return setNote('В кубе нет места.')
      const placed = { ...item, count: 1 }
      run({ ...view, cube: { ...cube, items: [...cube.items, placed], bag: takeOne(cube.bag, item) } },
        () => api.cubeDeposit(item.id), item.id)
    } else if (from === 'cube' && to === 'bag') {
      run({ ...view, cube: { ...cube, items: cube.items.filter((i) => i.id !== item.id), bag: [...cube.bag, item] } },
        () => api.cubeWithdraw(item.id))
    }
  }

  const { drag, handlers, isTap } = useDrag<ItemView>(canWork, onDrop)
  const tapOpen = (item: ItemView) => () => isTap() && setOpen(item)

  if (cube.status !== 'active') return <Locked cube={cube} />

  const zoneCls = (zone: Zone) =>
    drag && drag.from !== zone ? (drag.over === zone ? ' drop-over' : ' drop-ready') : ''
  const slots = Array.from({ length: cube.capacity }, (_, i) => cube.items[i] ?? null)
  const liftedId = drag?.item.id

  return (
    <>
      <section className={`cube${zoneCls('cube')}`} data-drop="cube">
        <div className="cube-grid">
          {slots.map((item, i) => (
            <CubeCell
              key={item ? `${item.id}-${i}` : `empty-${i}`}
              item={item}
              pending={!!item && item.id === pending && i === cube.items.length - 1}
              fresh={!!item && fresh.includes(item.id)}
              lifted={!!item && drag?.from === 'cube' && item.id === liftedId}
              bind={item ? handlers(item, 'cube') : {}}
              onOpen={item ? tapOpen(item) : () => undefined}
            />
          ))}
        </div>
        <div className="cube-side">
          <div>
            <div className="cube-title">Куб Азата</div>
            <div className="row-sub">{cube.home ? 'дома, на верстаке' : 'в Зале Азата'}</div>
          </div>
          <Meter
            icon={cube.power.kind === 'home' ? '✨' : '⚡'}
            label={cube.power.kind === 'home' ? 'Энергия' : 'Заряды'}
            pool={cube.power}
            tone="energy"
          />
          <div className="row-sub">
            В кубе {num(cube.items.length)} из {num(cube.capacity)}
          </div>
          <button
            className="cube-activate"
            disabled={!canWork || cube.items.length === 0 || cube.power.current <= 0}
            onClick={() => run(null, api.cubeActivate)}
          >
            ✨ Активировать
          </button>
          <button
            className="cube-take"
            disabled={!canWork || cube.items.length === 0}
            onClick={() => run(null, api.cubeTakeAll)}
          >
            Забрать всё
          </button>
        </div>
        {note && <p className="cube-note">{note}</p>}
      </section>
      {cube.away && <section className="card card-warn storage-note">{cube.away}</section>}
      <section className={`cube-bag${zoneCls('bag')}`} data-drop="bag">
        <div className="group-label">Сумка</div>
        {!cube.away && <p className="muted-note cube-hint">Зажмите вещь и перетащите в куб. Обратно — так же.</p>}
        {cube.bag.length === 0 ? (
          <p className="muted-note">В сумке нечего положить.</p>
        ) : (
          <div className="bag-grid">
            {cube.bag.map((item) => (
              <BagCell
                key={item.id}
                item={item}
                lifted={drag?.from === 'bag' && item.id === liftedId}
                bind={handlers(item, 'bag')}
                onOpen={tapOpen(item)}
              />
            ))}
          </div>
        )}
      </section>
      {drag && (
        <div className={`drag-ghost rarity-${tone(drag.item)}`} style={{ left: drag.x, top: drag.y }} aria-hidden>
          <Icon name={iconOf(drag.item)} size={30} />
        </div>
      )}
      {open && (
        <Sheet onClose={() => setOpen(null)}>
          <ItemText item={open} />
          {open.count > 1 && <p className="sheet-note">Здесь: {open.count} шт.</p>}
        </Sheet>
      )}
    </>
  )
}
