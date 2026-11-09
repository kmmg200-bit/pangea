import { useCallback, useEffect, useRef, useState } from 'react'
import { ApiError } from './api'

/** Что прочитано недавно, заново не читаем: при возврате в приложение событий
  * приходит сразу несколько. */
const FRESH_MS = 5_000

export interface Loaded<T> {
  data: T | null
  error: string | null
  loading: boolean
  reload: () => void
  /** Перечитать, если прочитано не только что. Первую загрузку не заменяет. */
  refresh: () => void
  /** Подменить данные свежими — из ответа на действие, без новой загрузки. */
  set: (data: T) => void
}

/** Загрузка с бота: данные, код ошибки (см. ApiError) и «обновить». Прежние
  * данные остаются на экране, пока идёт обновление. */
export function useLoad<T>(load: () => Promise<T>, enabled = true): Loaded<T> {
  const [data, setData] = useState<T | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [loading, setLoading] = useState(false)
  const [tick, setTick] = useState(0)
  const readAt = useRef(0)
  const dropRead = useRef(() => {})

  useEffect(() => {
    if (!enabled) return
    let alive = true
    const drop = () => {
      alive = false
    }
    dropRead.current = drop
    readAt.current = Date.now()
    setLoading(true)
    load()
      .then((d) => {
        if (!alive) return
        setData(d)
        setError(null)
      })
      .catch((e: unknown) => {
        if (alive) setError(e instanceof ApiError ? e.code : 'server')
      })
      .finally(() => {
        if (alive) setLoading(false)
      })
    return drop
  }, [load, enabled, tick])

  const reload = useCallback(() => setTick((t) => t + 1), [])
  const refresh = useCallback(() => {
    if (readAt.current > 0 && Date.now() - readAt.current >= FRESH_MS) setTick((t) => t + 1)
  }, [])
  // Ответ на действие свежее идущей загрузки: опоздавшая не должна его затереть.
  const set = useCallback((fresh: T) => {
    dropRead.current()
    readAt.current = Date.now()
    setData(fresh)
    setLoading(false)
  }, [])
  return { data, error, loading, reload, refresh, set }
}
