import { useEffect, useState } from 'react'
import { api, ApiError } from './api'
import { iconOf } from './kinds'
import type { CraftActionView, CraftView, ItemView, KnownBrewView, ShelfView } from './types'
import { Icon, ItemText, Meter, num, Segment, Sheet, tone } from './ui'
import { useDrag, type Zone } from './useDrag'

/** Ячейка куба или котелка: вещь — плиткой цвета редкости, пустая — пунктиром.
  * `fresh` — только что получилась: светится. */
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

/** Вещь сумки или полки плиткой: значок, название в две строки, счёт стопки.
  * `fresh` — только что сварилась: светится. */
function BagCell(props: { item: ItemView; lifted: boolean; fresh?: boolean; bind: object; onOpen: () => void }) {
  const { item } = props
  return (
    <button
      className={`bag-cell rarity-${tone(item)}${props.lifted ? ' cell-lifted' : ''}`}
      {...props.bind}
      onClick={props.onOpen}
    >
      <span className={`bag-cell-tile${props.fresh ? ' cube-cell-new' : ''}`}>
        <Icon name={iconOf(item)} size={26} />
        {item.count > 1 && <span className="bag-cell-count">{num(item.count)}</span>}
      </span>
      <span className="bag-cell-title">{item.title}</span>
    </button>
  )
}

/** Станок до того, как он есть: замок и что сделать. */
function Locked(props: { title: string; text: string | null }) {
  return (
    <div className="storage-locked">
      <div className="storage-locked-icon" aria-hidden>
        🔒
      </div>
      <div className="storage-locked-title">{props.title}</div>
      <p className="muted-note">{props.text}</p>
    </div>
  )
}

/** Убрать одну вещь стопки: последнюю — вместе со стопкой. */
const takeOne = (bag: ItemView[], item: ItemView) =>
  bag.flatMap((i) => (i.id !== item.id ? [i] : i.count > 1 ? [{ ...i, count: i.count - 1 }] : []))

/** Запрос к станку: пока ответ бота не пришёл, вещь уже стоит на новом месте
  * (`optimistic`), а брать следующую нельзя — номера вещей в стопках обновит
  * ответ. Не вышло — всё как было. */
function useCraft(view: CraftView, onView: (v: CraftView) => void) {
  const [busy, setBusy] = useState(false)
  const [note, setNote] = useState<string | null>(null)
  const [fresh, setFresh] = useState<number[]>([])
  const [pending, setPending] = useState<number | null>(null)

  const run = (optimistic: CraftView | null, act: () => Promise<CraftActionView>, pendingId: number | null = null) => {
    const before = view
    if (optimistic) onView(optimistic)
    setPending(pendingId)
    setBusy(true)
    setNote(null)
    act()
      .then((r) => {
        onView(r.craft)
        setNote(r.note)
        setFresh(r.made)
      })
      .catch((e: unknown) => {
        onView(before)
        setNote(e instanceof ApiError && e.code === 'offline' ? 'Нет связи — вещь осталась на месте.' : 'Не вышло — попробуйте ещё раз.')
      })
      .finally(() => {
        setBusy(false)
        setPending(null)
      })
  }
  return { busy, note, setNote, fresh, pending, run }
}

type Part = 'cube' | 'cauldron'

const parts: { id: Part; label: string }[] = [
  { id: 'cube', label: 'Куб Азата' },
  { id: 'cauldron', label: 'Котелок' },
]

/** «Крафт»: станки переключателем сверху — куб Азата и котелок верстака
  * травника. Данные у них общие: котелок тратит ту же энергию дома, что и куб
  * дома, а «Вынуть всё» может положить в сумку. */
export function CraftTab(props: { data: CraftView }) {
  const [view, setView] = useState(props.data)
  const [part, setPart] = useState<Part>('cube')
  useEffect(() => setView(props.data), [props.data])
  return (
    <>
      <Segment options={parts} value={part} onChange={setPart} />
      {part === 'cube' ? <CubePanel view={view} onView={setView} /> : <CauldronPanel view={view} onView={setView} />}
    </>
  )
}

/** Подсветка зоны, куда можно отпустить вещь. */
const zoneClass = (drag: { from: Zone; over: Zone | null } | null, zone: Zone) =>
  drag && drag.from !== zone ? (drag.over === zone ? ' drop-over' : ' drop-ready') : ''

/** Куб Азата: сверху ячейки и кнопки — они не уезжают, пока листаешь сумку;
  * ниже — сумка. Вещь переносят пальцем: зажал — взялась, отпустил над кубом —
  * легла. */
function CubePanel(props: { view: CraftView; onView: (v: CraftView) => void }) {
  const { view } = props
  const { busy, note, setNote, fresh, pending, run } = useCraft(view, props.onView)
  const [open, setOpen] = useState<ItemView | null>(null)

  const cube = view.cube
  const canWork = cube.status === 'active' && !cube.away && !busy

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

  if (cube.status !== 'active') return <Locked title="Куб Азата" text={cube.locked} />

  const slots = Array.from({ length: cube.capacity }, (_, i) => cube.items[i] ?? null)
  const liftedId = drag?.item.id

  return (
    <>
      <section className={`cube${zoneClass(drag, 'cube')}`} data-drop="cube">
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
      <section className={`cube-bag${zoneClass(drag, 'bag')}`} data-drop="bag">
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
      {open && <ItemSheet item={open} onClose={() => setOpen(null)} />}
    </>
  )
}

function ItemSheet(props: { item: ItemView; onClose: () => void }) {
  return (
    <Sheet onClose={props.onClose}>
      <ItemText item={props.item} />
      {props.item.count > 1 && <p className="sheet-note">Здесь: {props.item.count} шт.</p>}
    </Sheet>
  )
}

/** Котелок верстака травника: сверху ячейки и кнопки, как у куба; ниже —
  * полки трав и полка отваров, с которых в него кладут, — как в боте; под
  * ними — изученные отвары. Варит на энергии дома. */
function CauldronPanel(props: { view: CraftView; onView: (v: CraftView) => void }) {
  const { view } = props
  const { busy, note, setNote, fresh, pending, run } = useCraft(view, props.onView)
  const [open, setOpen] = useState<ItemView | null>(null)
  const [recipeKind, setRecipeKind] = useState<string | null>(null)

  const pot = view.cauldron
  const canWork = !pot.locked && !pot.away && !busy

  const onDrop = (item: ItemView, from: Zone, to: Zone) => {
    if (from === 'shelf' && to === 'pot') {
      if (pot.items.length >= pot.capacity) return setNote('В котелке нет места.')
      const placed = { ...item, count: 1 }
      const herbs = { ...pot.herbs, items: takeOne(pot.herbs.items, item) }
      const brews = { ...pot.brews, items: takeOne(pot.brews.items, item) }
      run({ ...view, cauldron: { ...pot, items: [...pot.items, placed], herbs, brews } },
        () => api.cauldronPut(item.id), item.id)
    } else if (from === 'pot' && to === 'shelf') {
      run({ ...view, cauldron: { ...pot, items: pot.items.filter((i) => i.id !== item.id) } },
        () => api.cauldronBack(item.id))
    }
  }

  const { drag, handlers, isTap } = useDrag<ItemView>(canWork, onDrop)
  const tapOpen = (item: ItemView) => () => isTap() && setOpen(item)

  if (pot.locked) return <Locked title="Котелок" text={pot.locked} />

  const slots = Array.from({ length: pot.capacity }, (_, i) => pot.items[i] ?? null)
  const liftedId = drag?.item.id
  const recipe = pot.known.find((k) => k.kind === recipeKind) ?? null

  const shelf = (title: string, s: ShelfView, empty: string) => (
    <div className="cauldron-shelf">
      <div className="group-label">
        {title} · {num(s.used)} из {num(s.max)}
      </div>
      {s.items.length === 0 ? (
        <p className="muted-note">{empty}</p>
      ) : (
        <div className="bag-grid">
          {s.items.map((item) => (
            <BagCell
              key={item.id}
              item={item}
              lifted={drag?.from === 'shelf' && item.id === liftedId}
              fresh={fresh.includes(item.id)}
              bind={handlers(item, 'shelf')}
              onOpen={tapOpen(item)}
            />
          ))}
        </div>
      )}
    </div>
  )

  return (
    <>
      <section className={`cube${zoneClass(drag, 'pot')}`} data-drop="pot">
        <div className="cube-grid">
          {slots.map((item, i) => (
            <CubeCell
              key={item ? `${item.id}-${i}` : `empty-${i}`}
              item={item}
              pending={!!item && item.id === pending && i === pot.items.length - 1}
              fresh={!!item && fresh.includes(item.id)}
              lifted={!!item && drag?.from === 'pot' && item.id === liftedId}
              bind={item ? handlers(item, 'pot') : {}}
              onOpen={item ? tapOpen(item) : () => undefined}
            />
          ))}
        </div>
        <div className="cube-side">
          <div>
            <div className="cube-title">Котелок</div>
            <div className="row-sub">дома, на верстаке травника</div>
          </div>
          <Meter icon="✨" label="Энергия" pool={pot.power} tone="energy" />
          <div className="row-sub">
            В котелке {num(pot.items.length)} из {num(pot.capacity)}
          </div>
          <button
            className="cube-activate"
            disabled={!canWork || pot.items.length === 0 || pot.power.current <= 0}
            onClick={() => run(null, api.cauldronCook)}
          >
            🔥 Варить
          </button>
          <button
            className="cube-take"
            disabled={!canWork || pot.items.length === 0}
            onClick={() => run(null, api.cauldronEmpty)}
          >
            Вынуть всё
          </button>
        </div>
        {note && !recipe && <p className="cube-note">{note}</p>}
      </section>
      {pot.away && <section className="card card-warn storage-note">{pot.away}</section>}
      <section className={`cube-bag${zoneClass(drag, 'shelf')}`} data-drop="shelf">
        {!pot.away && (
          <p className="muted-note cube-hint">Зажмите траву или склянку и перетащите в котелок. Обратно — так же.</p>
        )}
        {shelf('Полки трав', pot.herbs, 'На полках трав пусто.')}
        {shelf('Полка отваров', pot.brews, 'Полка отваров пуста.')}
      </section>
      <section className="group cauldron-known">
        <div className="group-label">📖 Изученные</div>
        {pot.known.length === 0 ? (
          <p className="muted-note">
            Изученных отваров пока нет. Сварите что-нибудь в котелке или в кубе Азата — и рецепт появится здесь.
          </p>
        ) : (
          <div className="list">
            {pot.known.map((k) => (
              <button
                key={k.kind}
                className="row row-plain"
                onClick={() => {
                  setNote(null)
                  setRecipeKind(k.kind)
                }}
              >
                <span className="row-body">
                  <span className="row-title">{k.name}</span>
                  <span className="row-sub">
                    {k.can > 0 ? `хватит на ${num(k.can)} шт.` : 'сейчас не хватает трав или энергии'}
                  </span>
                </span>
                <span className="row-chevron" aria-hidden>
                  ›
                </span>
              </button>
            ))}
          </div>
        )}
      </section>
      {drag && (
        <div className={`drag-ghost rarity-${tone(drag.item)}`} style={{ left: drag.x, top: drag.y }} aria-hidden>
          <Icon name={iconOf(drag.item)} size={30} />
        </div>
      )}
      {open && <ItemSheet item={open} onClose={() => setOpen(null)} />}
      {recipe && (
        <RecipeSheet
          recipe={recipe}
          busy={busy}
          blocked={pot.away}
          note={note}
          onBrew={(n) => run(null, () => api.cauldronBrew(recipe.kind, n))}
          onClose={() => {
            setRecipeKind(null)
            setNote(null)
          }}
        />
      )}
    </>
  )
}

/** Изученный отвар — как в боте: из чего, сколько выходит с варки, на сколько
  * хватит; число — и варится с полок. Окно не закрывается после варки: видно
  * итог и сколько ещё хватит. */
function RecipeSheet(props: {
  recipe: KnownBrewView
  busy: boolean
  blocked: string | null
  note: string | null
  onBrew: (n: number) => void
  onClose: () => void
}) {
  const r = props.recipe
  const [count, setCount] = useState('')
  const n = Number(count)
  return (
    <Sheet onClose={props.onClose}>
      <div className="sheet-title">{r.name}</div>
      <div className="item-text">
        {[
          `Рецепт: ${r.herbs.join(', ')}.`,
          r.base && `Основа: «${r.base}» с полки отваров.`,
          `Из одной варки — ${r.portions} шт., энергии уходит 1.`,
          `Трав на полках и энергии хватит на ${num(r.can)} шт.`,
        ]
          .filter(Boolean)
          .join('\n')}
      </div>
      {props.blocked ? (
        <p className="sheet-note">{props.blocked}</p>
      ) : (
        <div className="cauldron-brew">
          <label className="level-field">
            Сколько
            <input
              inputMode="numeric"
              aria-label="Сколько приготовить"
              placeholder={r.can > 0 ? String(r.can) : '0'}
              value={count}
              onChange={(e) => setCount(e.target.value.replace(/\D/g, '').replace(/^0+/, '').slice(0, 4))}
            />
          </label>
          <button className="cube-activate" disabled={props.busy || !(n > 0)} onClick={() => props.onBrew(n)}>
            🔥 Сварить
          </button>
        </div>
      )}
      {props.note && <p className="cube-note">{props.note}</p>}
    </Sheet>
  )
}
