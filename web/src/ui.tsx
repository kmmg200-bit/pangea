import { useEffect, useRef, useState, type ReactNode } from 'react'
import { gameIcons, type GameIcon } from './gameIcons'
import { iconOf, kindOf } from './kinds'
import type { BeltView, ItemView, PoolView } from './types'

/** Иконка game-icons.net. Цвет — от текста (currentColor). Разметка — из
  * своего файла gameIcons.ts, не из ответа бота. */
export function Icon(props: { name: GameIcon; size?: number; className?: string }) {
  return (
    <svg
      className={props.className ? `gi ${props.className}` : 'gi'}
      viewBox="0 0 512 512"
      width={props.size ?? 22}
      height={props.size ?? 22}
      aria-hidden
      dangerouslySetInnerHTML={{ __html: gameIcons[props.name] }}
    />
  )
}

/** Переключатель частей вкладки: «Герой | Снаряжение», «При себе | В городе | Дом». */
export function Segment<T extends string>(props: {
  options: { id: T; label: string }[]
  value: T
  onChange: (id: T) => void
  /** Кнопок много — по три в строку, а не все в одну. */
  rows?: boolean
}) {
  return (
    <div className={props.rows ? 'segment segment-rows' : 'segment'} role="tablist">
      {props.options.map((o) => (
        <button
          key={o.id}
          role="tab"
          aria-selected={o.id === props.value}
          className={o.id === props.value ? 'segment-item segment-on' : 'segment-item'}
          onClick={() => props.onChange(o.id)}
        >
          {o.label}
        </button>
      ))}
    </div>
  )
}

/** Плитка вещи: тёмный квадрат, иконка цветом редкости. Без вещи — серая иконка
  * того, что сюда надевается. */
export function Tile(props: { icon: GameIcon; rarity?: string }) {
  return (
    <span className={props.rarity ? `tile rarity-${props.rarity}` : 'tile tile-empty'} aria-hidden>
      <Icon name={props.icon} />
    </span>
  )
}

/** Цвет вещи: у сюжетного — голубой (`rarity-Story`), у прочего — цвет редкости. */
export const tone = (item: ItemView): string => (item.story ? 'Story' : item.rarity)

export const ItemTile = (props: { item: ItemView }) => <Tile icon={iconOf(props.item)} rarity={tone(props.item)} />

export const num = (n: number) => n.toLocaleString('ru-RU')

/** Слово к числу: «1 рецепт», «3 рецепта», «7 рецептов». */
export function plural(n: number, one: string, few: string, many: string): string {
  const tens = Math.abs(n) % 100
  const last = tens % 10
  if (tens >= 11 && tens <= 14) return many
  if (last === 1) return one
  if (last >= 2 && last <= 4) return few
  return many
}

/** Полоска запаса: что это, сколько из скольких и заполненность. */
export function Meter(props: { icon: string; label: string; pool: PoolView; tone: string }) {
  const { current, max } = props.pool
  const pct = max > 0 ? Math.min(100, Math.max(0, (current / max) * 100)) : 0
  return (
    <div className="meter">
      <div className="meter-top">
        <span>
          {props.icon} {props.label}
        </span>
        <span className="meter-num">
          {num(current)} / {num(max)}
        </span>
      </div>
      <div className="meter-track">
        <span className={`meter-fill tone-${props.tone}`} style={{ width: `${pct}%` }} />
      </div>
    </div>
  )
}

export function Chips<T extends string>(props: {
  options: { id: T; label: ReactNode }[]
  value: T
  onChange: (id: T) => void
}) {
  // Выбранная кнопка не прячется за краем полосы: полоса сама к ней подъезжает.
  const on = useRef<HTMLButtonElement | null>(null)
  useEffect(() => {
    on.current?.scrollIntoView({ block: 'nearest', inline: 'nearest', behavior: 'smooth' })
  }, [props.value])
  return (
    <div className="chips" role="tablist">
      {props.options.map((o) => (
        <button
          key={o.id}
          ref={o.id === props.value ? on : undefined}
          role="tab"
          aria-selected={o.id === props.value}
          className={o.id === props.value ? 'chip chip-on' : 'chip'}
          onClick={() => props.onChange(o.id)}
        >
          {o.label}
        </button>
      ))}
    </div>
  )
}

export function Toolbar(props: {
  query: string
  onQuery: (q: string) => void
  sort: string
  sorts: { id: string; label: string }[]
  onSort: (id: string) => void
}) {
  return (
    <div className="toolbar">
      <input
        className="search"
        type="search"
        placeholder="Поиск по названию"
        value={props.query}
        onChange={(e) => props.onQuery(e.target.value)}
      />
      <select className="sort" value={props.sort} onChange={(e) => props.onSort(e.target.value)} aria-label="Сортировка">
        {props.sorts.map((s) => (
          <option key={s.id} value={s.id}>
            {s.label}
          </option>
        ))}
      </select>
    </div>
  )
}

/** Строка вещи: полоска цвета редкости, значок типа, название и справа — что передали. */
export function ItemRow(props: { item: ItemView; aside?: ReactNode; note?: ReactNode; onOpen: () => void }) {
  const kind = kindOf(props.item)
  return (
    <button className={`row rarity-${tone(props.item)}`} onClick={props.onOpen}>
      <ItemTile item={props.item} />
      <span className="row-body">
        <span className="row-title">{props.item.title}</span>
        <span className="row-sub">
          {kind.label}
          {props.note}
        </span>
      </span>
      {props.item.count > 1 && <span className="badge">×{props.item.count}</span>}
      {props.aside}
    </button>
  )
}

/** Карточка вещи — окном по центру экрана. Закрывается крестиком, тапом мимо и клавишей Esc. */
export function Sheet(props: { onClose: () => void; children: ReactNode }) {
  const { onClose } = props
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => e.key === 'Escape' && onClose()
    window.addEventListener('keydown', onKey)
    document.body.classList.add('locked')
    return () => {
      window.removeEventListener('keydown', onKey)
      document.body.classList.remove('locked')
    }
  }, [onClose])
  return (
    <div className="sheet-backdrop" onClick={props.onClose}>
      <div className="sheet" role="dialog" aria-modal onClick={(e) => e.stopPropagation()}>
        <button className="sheet-close" onClick={props.onClose} aria-label="Закрыть">
          ✕
        </button>
        {props.children}
      </div>
    </div>
  )
}

/** Что снять с полного пояса ради новой вещи: вопрос и ячейки — словами бота. */
export function BeltSwap(props: { ask: string; belt: BeltView; busy: boolean; onPick: (cell: number) => void }) {
  return (
    <div className="buy-confirm">
      <p className="buy-ask">{props.ask}</p>
      <div className="move-list">
        {props.belt.cells.map((c, i) => (
          <button key={i} className="move-to" disabled={props.busy} onClick={() => props.onPick(i)}>
            {c.swap}
          </button>
        ))}
      </div>
    </div>
  )
}

/** Описание вещи — текст из бота как есть, с переносами строк. */
export function ItemText(props: { item: ItemView }) {
  return <div className="item-text">{props.item.text}</div>
}

export function Empty(props: { children: ReactNode }) {
  return <div className="empty">{props.children}</div>
}

export function Spinner() {
  return <div className="spinner" aria-label="Загрузка" />
}

const errors: Record<string, string> = {
  expired: 'Приложение открыто давно. Закройте его и откройте снова.',
  'no-hero': 'Героя пока нет. Начните игру в сообщениях сообщества — и возвращайтесь.',
  'bad-sign': 'Не удалось узнать игрока. Откройте приложение из ВКонтакте.',
  off: 'Приложение пока выключено.',
  offline: 'Нет связи с Пангеей. Проверьте интернет.',
  server: 'В Пангее что-то сломалось. Попробуйте чуть позже.',
}

export function ErrorView(props: { code: string; onRetry: () => void }) {
  const retry = props.code === 'offline' || props.code === 'server'
  return (
    <div className="error">
      <p>{errors[props.code] ?? errors.server}</p>
      {retry && (
        <button className="button" onClick={props.onRetry}>
          Повторить
        </button>
      )}
    </div>
  )
}

/** Всплывающая строка над нижней панелью — что сказал бот. Пропадает сама
  * через пять секунд или по нажатию. Возвращает саму строку (положить в
  * разметку вкладки) и чем её показать. */
export function useToast(): [ReactNode, (text: string) => void] {
  const [toast, setToast] = useState<string | null>(null)
  useEffect(() => {
    if (!toast) return
    const t = window.setTimeout(() => setToast(null), 5000)
    return () => window.clearTimeout(t)
  }, [toast])
  const view = toast && (
    <div className="toast" role="status" onClick={() => setToast(null)}>
      {toast}
    </div>
  )
  return [view, setToast]
}
