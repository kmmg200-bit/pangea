import { useEffect, useRef, useState } from 'react'
import { tap } from './platform'

/** Откуда и куда носят: сумка и куб, полки верстака и котелок. Зона — элемент
  * с `data-drop`. */
export type Zone = 'bag' | 'cube' | 'shelf' | 'pot'

const zones: readonly string[] = ['bag', 'cube', 'shelf', 'pot']

export interface Drag<T> {
  item: T
  from: Zone
  x: number
  y: number
  /** Над какой зоной сейчас палец. */
  over: Zone | null
}

/** Сколько держать палец на вещи, чтобы она «взялась»: короче — это прокрутка
  * или тап. Свайп по сумке и перенос в куб начинаются одинаково — пальцем вверх,
  * — различает их только эта пауза; держим её короткой, чтобы перенос не ждал. */
const HoldMs = 100
/** Насколько можно сдвинуть палец, пока держишь, — дальше это уже прокрутка. */
const SlopPx = 8

const zoneAt = (x: number, y: number): Zone | null => {
  const el = document.elementFromPoint(x, y)?.closest<HTMLElement>('[data-drop]')
  const z = el?.dataset.drop
  return z && zones.includes(z) ? (z as Zone) : null
}

/** Перенос пальцем: зажал вещь — она поднялась и идёт за пальцем, отпустил над
  * другой зоной — `onDrop`. Быстрый свайп — обычная прокрутка, короткое касание —
  * обычный тап. Мышью — то же, только без задержки. Пока `enabled` ложно, вещи
  * не берутся. */
export function useDrag<T>(enabled: boolean, onDrop: (item: T, from: Zone, to: Zone) => void) {
  const [drag, setDrag] = useState<Drag<T> | null>(null)
  const dragRef = useRef<Drag<T> | null>(null)
  const press = useRef<{ item: T; from: Zone; x: number; y: number; timer: number } | null>(null)
  // После переноса браузер может прислать «клик» по вещи под пальцем — его глушим.
  const justDropped = useRef(0)
  const dropRef = useRef(onDrop)
  dropRef.current = onDrop

  const set = (d: Drag<T> | null) => {
    dragRef.current = d
    setDrag(d)
  }

  const cancelPress = () => {
    if (press.current) window.clearTimeout(press.current.timer)
    press.current = null
  }

  const lift = (item: T, from: Zone, x: number, y: number) => {
    cancelPress()
    tap()
    set({ item, from, x, y, over: zoneAt(x, y) })
  }

  const move = (x: number, y: number) => {
    const d = dragRef.current
    if (d) set({ ...d, x, y, over: zoneAt(x, y) })
  }

  const finish = () => {
    const d = dragRef.current
    cancelPress()
    if (!d) return
    set(null)
    justDropped.current = Date.now()
    if (d.over && d.over !== d.from) dropRef.current(d.item, d.from, d.over)
  }

  useEffect(() => {
    // Не пассивный: только так, пока вещь в руке, палец не прокручивает страницу.
    const onTouchMove = (e: TouchEvent) => {
      const t = e.touches[0]
      if (!t) return
      if (dragRef.current) {
        e.preventDefault()
        move(t.clientX, t.clientY)
      } else if (press.current && Math.hypot(t.clientX - press.current.x, t.clientY - press.current.y) > SlopPx) {
        cancelPress()
      }
    }
    const onMouseMove = (e: MouseEvent) => {
      const p = press.current
      if (dragRef.current) move(e.clientX, e.clientY)
      else if (p && Math.hypot(e.clientX - p.x, e.clientY - p.y) > 4) lift(p.item, p.from, e.clientX, e.clientY)
    }
    document.addEventListener('touchmove', onTouchMove, { passive: false })
    document.addEventListener('touchend', finish)
    document.addEventListener('touchcancel', finish)
    document.addEventListener('mousemove', onMouseMove)
    document.addEventListener('mouseup', finish)
    return () => {
      document.removeEventListener('touchmove', onTouchMove)
      document.removeEventListener('touchend', finish)
      document.removeEventListener('touchcancel', finish)
      document.removeEventListener('mousemove', onMouseMove)
      document.removeEventListener('mouseup', finish)
    }
    // Обработчики читают всё через ref — подписываемся один раз.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  /** Что повесить на вещь: зажатие пальцем или мышью. */
  const handlers = (item: T, from: Zone) => ({
    onTouchStart: (e: React.TouchEvent) => {
      const t = e.touches[0]
      if (!enabled || !t || e.touches.length > 1) return
      const x = t.clientX
      const y = t.clientY
      cancelPress()
      press.current = { item, from, x, y, timer: window.setTimeout(() => lift(item, from, x, y), HoldMs) }
    },
    onMouseDown: (e: React.MouseEvent) => {
      if (!enabled || e.button !== 0) return
      cancelPress()
      press.current = { item, from, x: e.clientX, y: e.clientY, timer: 0 }
    },
    onContextMenu: (e: React.MouseEvent) => e.preventDefault(),
  })

  /** Был ли это тап, а не конец переноса. */
  const isTap = () => Date.now() - justDropped.current > 400

  return { drag, handlers, isTap }
}
