import { useState } from 'react'
import type { DoneQuestView, QuestsView } from './types'
import { Empty, Segment } from './ui'

type Part = 'active' | 'done'

const parts: { id: Part; label: string }[] = [
  { id: 'active', label: 'В работе' },
  { id: 'done', label: 'Пройденные' },
]

/** «Задания»: в работе — тот же журнал, что в боте, по разделам; пройденные —
  * что герой уже закончил и как эти задания закончили другие герои. */
export function QuestsTab(props: { data: QuestsView }) {
  const [part, setPart] = useState<Part>('active')
  return (
    <>
      <Segment options={parts} value={part} onChange={setPart} />
      {/* Бот старше приложения — поля нет: пройденных просто не видно. */}
      {part === 'active' ? <Active data={props.data} /> : <Done done={props.data.done ?? []} />}
    </>
  )
}

function Active(props: { data: QuestsView }) {
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

/** Пройденные: сюжет, потом задания горожан — как в журнале. */
function Done(props: { done: DoneQuestView[] }) {
  if (props.done.length === 0) return <Empty>Пройденных заданий пока нет.</Empty>
  const story = props.done.filter((d) => d.story)
  const npcs = props.done.filter((d) => !d.story)
  return (
    <div className="character">
      {story.length > 0 && (
        <>
          <h2 className="section-title">Сюжет</h2>
          <div className="list">
            {story.map((d) => (
              <DoneRow key={d.title} d={d} />
            ))}
          </div>
        </>
      )}

      {npcs.length > 0 && (
        <>
          <h2 className="section-title">Задания горожан</h2>
          <div className="list">
            {npcs.map((d) => (
              <DoneRow key={d.title} d={d} />
            ))}
          </div>
        </>
      )}
    </div>
  )
}

/** Пройденное задание; у развилки — как его закончили другие, свой исход помечен. */
function DoneRow(props: { d: DoneQuestView }) {
  const { d } = props
  return (
    <div className="row row-plain row-static">
      <span className="row-icon" aria-hidden>
        ✅
      </span>
      <span className="row-body">
        <span className="row-title">«{d.title}»</span>
        {d.first && <span className="row-sub">{d.first}</span>}
        {d.stats.map((s) => (
          <span key={s.label} className="row-sub">
            {s.pct} {s.label}
            {s.mine && <span className="top-you">как и вы</span>}
          </span>
        ))}
      </span>
    </div>
  )
}
