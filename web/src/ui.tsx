import { useEffect, useRef, type ReactNode } from 'react'
import { kindOf } from './kinds'
import type { ItemView, PoolView } from './types'

export const num = (n: number) => n.toLocaleString('ru-RU')

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
    <button className={`row rarity-${props.item.rarity}`} onClick={props.onOpen}>
      <span className="row-icon" aria-hidden>
        {kind.icon}
      </span>
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
