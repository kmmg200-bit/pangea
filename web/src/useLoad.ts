import { useCallback, useEffect, useState } from 'react'
import { ApiError } from './api'

export interface Loaded<T> {
  data: T | null
  error: string | null
  loading: boolean
  reload: () => void
}

/** Загрузка с бота: данные, код ошибки (см. ApiError) и «обновить». Прежние
  * данные остаются на экране, пока идёт обновление. */
export function useLoad<T>(load: () => Promise<T>, enabled = true): Loaded<T> {
  const [data, setData] = useState<T | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [loading, setLoading] = useState(false)
  const [tick, setTick] = useState(0)

  useEffect(() => {
    if (!enabled) return
    let alive = true
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
    return () => {
      alive = false
    }
  }, [load, enabled, tick])

  const reload = useCallback(() => setTick((t) => t + 1), [])
  return { data, error, loading, reload }
}
