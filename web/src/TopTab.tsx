import type { TopEntryView, TopKind, TopView } from './types'
import { Empty, ErrorView, num, Segment, Spinner } from './ui'
import type { Loaded } from './useLoad'

export const topKinds: { id: TopKind; label: string }[] = [
  { id: 'level', label: 'Уровень' },
  { id: 'depth', label: 'Глубина' },
  { id: 'kills', label: 'Убийства' },
]

/** Подпись к числу справа — что топ меряет. */
const units: Record<TopKind, string> = { level: 'ур.', depth: 'ур. лабиринта', kills: 'убито' }

function Row(props: { e: TopEntryView; kind: TopKind; pinned?: boolean }) {
  const { e, kind } = props
  const medal = e.place <= 3 ? ` top-place-${e.place}` : ''
  return (
    <div className={`top-row${e.me ? ' top-me' : ''}${props.pinned ? ' top-pinned' : ''}`}>
      <span className={`top-place${medal}`}>{num(e.place)}</span>
      <span className="row-body">
        <span className="row-title">
          {e.profile ? (
            <a className="top-name" href={e.profile} target="_blank" rel="noopener noreferrer">
              {e.name}
            </a>
          ) : (
            <span className="top-name">{e.name}</span>
          )}
          {e.me && <span className="top-you">вы</span>}
        </span>
        <span className="row-sub">
          {e.race}
          {kind !== 'level' && ` · ${num(e.lvl)} ур.`}
        </span>
      </span>
      <span className="top-value">
        <b>{num(e.value)}</b>
        <span>{units[kind]}</span>
      </span>
    </div>
  )
}

function Board(props: { loaded: Loaded<TopView>; kind: TopKind }) {
  const { data, error, reload } = props.loaded
  if (error && !data) return <ErrorView code={error} onRetry={reload} />
  if (!data) return <Spinner />
  if (data.rows.length === 0 && !data.me) return <Empty>В топе пока никого нет.</Empty>
  return (
    <>
      <div className="summary">Героев в топе: {num(data.total)}</div>
      <div className="list">
        {data.rows.map((e) => (
          <Row key={e.place} e={e} kind={props.kind} />
        ))}
      </div>
      {data.me && (
        <>
          <div className="top-gap" aria-hidden>
            ⋯
          </div>
          <Row e={data.me} kind={props.kind} pinned />
        </>
      )}
    </>
  )
}

/** «Топ»: по уровню, глубине лабиринта и убийствам — первые сто и своё место.
  * Загрузки держит App, как и у прочих вкладок: каждый топ грузится, когда
  * его впервые открыли. */
export function TopTab(props: { kind: TopKind; onKind: (k: TopKind) => void; loaded: Loaded<TopView> }) {
  return (
    <>
      <Segment options={topKinds} value={props.kind} onChange={props.onKind} />
      <Board loaded={props.loaded} kind={props.kind} />
    </>
  )
}
