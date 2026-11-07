import type { QuestsView } from './types'
import { Empty } from './ui'

/** «Задания» — тот же журнал, что в боте, по разделам. */
export function QuestsTab(props: { data: QuestsView }) {
  const q = props.data
  const empty = !q.story && q.npcs.length === 0 && q.dailies.length === 0 && q.boards.length === 0
  if (empty)
    return <Empty>Заданий сейчас нет. Горожане Кинэта и доска в Гильдии искателей найдут, чем занять.</Empty>

  return (
    <div className="character">
      {q.story && (
        <>
          <h2 className="section-title">Сюжет</h2>
          <section className="card">
            <div className="card-title">📌 «{q.story.title}»</div>
            <div className="item-text quest-goals">{q.story.objectives}</div>
          </section>
        </>
      )}

      {q.npcs.length > 0 && (
        <>
          <h2 className="section-title">Задания горожан</h2>
          <div className="list">
            {q.npcs.map((n) => (
              <div key={n.title} className="row row-plain row-static">
                <span className="row-icon" aria-hidden>
                  🧭
                </span>
                <span className="row-body">
                  <span className="row-title">«{n.title}»</span>
                  {n.step && <span className="row-sub">{n.step}</span>}
                </span>
              </div>
            ))}
          </div>
        </>
      )}

      {q.dailies.length > 0 && (
        <>
          <h2 className="section-title">Поручения на сегодня</h2>
          <div className="list">
            {q.dailies.map((d) => (
              <div key={d.npc + d.line} className="row row-plain row-static">
                <span className="row-icon" aria-hidden>
                  📅
                </span>
                <span className="row-body">
                  <span className="row-title">{d.npc}</span>
                  <span className="row-sub">{d.line}</span>
                </span>
              </div>
            ))}
          </div>
        </>
      )}

      {q.boards.length > 0 && (
        <>
          <h2 className="section-title">Доска заданий</h2>
          <div className="list">
            {q.boards.map((b) => (
              <div key={b.board + b.what} className="row row-plain row-static">
                <span className="row-icon" aria-hidden>
                  📋
                </span>
                <span className="row-body">
                  <span className="row-sub">Доска {b.board}</span>
                  <span className="row-title">{b.what}</span>
                </span>
                <span className={b.ready ? 'pill pill-ready' : 'pill'}>{b.state}</span>
              </div>
            ))}
          </div>
        </>
      )}
    </div>
  )
}
