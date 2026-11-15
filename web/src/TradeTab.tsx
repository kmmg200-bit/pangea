import { useEffect, useState } from 'react'
import { api } from './api'
import type { HeroNameView, ItemView, TradeActionView, TradeItemView, TradeOfferView, TradeSideView, TradeView } from './types'
import { Empty, ItemRow, ItemText, Sheet, num, useToast } from './ui'

/** Сколько ещё ждать: «2 дн. 5 ч», «3 ч 10 мин», «меньше минуты». */
function left(ms: number): string {
  const min = Math.floor(ms / 60000)
  const d = Math.floor(min / 1440)
  const h = Math.floor((min % 1440) / 60)
  const m = min % 60
  if (d > 0) return `${d} дн. ${h} ч`
  if (h > 0) return `${h} ч ${m} мин`
  return m > 0 ? `${m} мин` : 'меньше минуты'
}

/** Только цифры: суммы пишут числом. */
const digits = (s: string) => s.replace(/\D/g, '').slice(0, 9)

/** Имя для сравнения: без регистра, «ё» как «е». */
const norm = (s: string) => s.trim().toLowerCase().replace(/ё/g, 'е')

/** Подсказки по набранному: сперва имена с такого начала, потом — где оно
  * внутри, не больше шести. Набранное совпало с именем целиком — подсказывать
  * нечего. */
function suggest(heroes: HeroNameView[], typed: string): HeroNameView[] {
  const q = norm(typed)
  if (!q || heroes.some((h) => norm(h.name) === q)) return []
  const starts = heroes.filter((h) => norm(h.name).startsWith(q))
  const inside = heroes.filter((h) => !norm(h.name).startsWith(q) && norm(h.name).includes(q))
  return [...starts, ...inside].slice(0, 6)
}

/** Имена героев — один раз на всё время работы: меняются они редко. Не
  * загрузились — подсказок нет, имя пишут целиком, а в следующий раз попробуем
  * снова. */
let heroNames: Promise<HeroNameView[]> | null = null
function loadHeroNames(): Promise<HeroNameView[]> {
  heroNames ??= api
    .heroNames()
    .then((v) => v.heroes)
    .catch(() => {
      heroNames = null
      return []
    })
  return heroNames
}

/** Своя сторона в работе: номера вещей, суммы — как в полях, записка. `offer` —
  * ответ на это предложение; нет — новое, партнёру `partner`. */
interface Draft {
  offer: number | null
  partner: string
  ids: number[]
  silver: string
  doubloons: string
  note: string
}

/** Сторона: вещи строками, деньги, записка. */
function Side(props: { title: string; side: TradeSideView; onOpen: (i: ItemView) => void }) {
  const { side } = props
  const empty = side.items.length === 0 && side.silver === 0 && side.doubloons === 0
  return (
    <div className="trade-side">
      <div className="sub-title">{props.title}</div>
      {empty && <p className="muted-note">Ничего.</p>}
      {side.items.length > 0 && (
        <div className="list">
          {side.items.map((t) => (
            <ItemRow key={t.ids[0]} item={t.item} onOpen={() => props.onOpen(t.item)} />
          ))}
        </div>
      )}
      {(side.silver > 0 || side.doubloons > 0) && (
        <p className="trade-money">
          {side.silver > 0 && <span>🪙 {num(side.silver)}</span>}
          {side.doubloons > 0 && <span>🟡 {num(side.doubloons)}</span>}
        </p>
      )}
      {side.note && <p className="trade-note">«{side.note}»</p>}
    </div>
  )
}

/** Чей ход коротко: свой — «ваш ход» или, если партнёр согласен, «подтвердите»;
  * партнёра — ждём ответа или, если сами согласились, подтверждения. */
function turnBadge(o: TradeOfferView): string {
  if (o.yourTurn) return o.partnerAgreed ? 'подтвердите' : 'ваш ход'
  return o.youAgreed ? 'ждём подтверждения' : 'ждём ответа'
}

/** Чей ход и что можно сделать — строкой на карточке. */
function turnNote(o: TradeOfferView): string {
  if (o.yourTurn && o.partnerAgreed)
    return 'Партнёр согласен на эти условия. Подтвердите — и обмен состоится. Измените своё — и понадобится новое согласие.'
  if (o.yourTurn) return 'Ход ваш: согласитесь, измените своё или откажитесь. Согласитесь — останется подтверждение партнёра.'
  if (o.youAgreed) return 'Вы согласились — ждём, когда партнёр подтвердит.'
  return 'Ждём ответа партнёра.'
}

/** Строка предложения в списке: с кем, что отдаёте, что получаете, чей ход. */
function OfferRow(props: { offer: TradeOfferView; onOpen: () => void }) {
  const o = props.offer
  return (
    <button className={o.yourTurn ? 'trade-offer trade-yours' : 'trade-offer'} onClick={props.onOpen}>
      <span className="trade-head">
        <span className="row-title">{o.partner}</span>
        <span className={o.yourTurn ? 'badge badge-on' : 'badge'}>{turnBadge(o)}</span>
      </span>
      <span className="row-sub">Вы отдаёте: {o.mine.summary}</span>
      <span className="row-sub">Вы получаете: {o.theirs.summary}</span>
    </button>
  )
}

/** Своя сторона: сверху — что кладёте (нажал — убрал), снизу — что при вас
  * (нажал — положил); суммы и записка между ними. Стопку кладут по одной. */
function Editor(props: {
  data: TradeView
  offer: TradeOfferView | undefined
  draft: Draft
  busy: boolean
  refusal: string | null
  onDraft: (d: Draft) => void
  onSend: () => void
  onClose: () => void
}) {
  const { data, offer, draft, busy } = props
  const [names, setNames] = useState<HeroNameView[]>([])
  useEffect(() => {
    if (!offer) loadHeroNames().then(setNames)
  }, [offer])
  const hints = offer ? [] : suggest(names, draft.partner)
  // Откуда берутся вещи: что уже в закладе и что при герое.
  const pool: TradeItemView[] = [...(offer?.mine.items ?? []), ...data.stock]
  const chosen = pool
    .map((t) => ({ t, ids: t.ids.filter((id) => draft.ids.includes(id)) }))
    .filter((x) => x.ids.length > 0)
  const rest = pool
    .map((t) => ({ t, ids: t.ids.filter((id) => !draft.ids.includes(id)) }))
    .filter((x) => x.ids.length > 0)
  const set = (patch: Partial<Draft>) => props.onDraft({ ...draft, ...patch })
  const fee = (s: string) => {
    const n = Number(s || 0)
    return n > 0 ? Math.ceil((n * data.feePct) / 100) : 0
  }
  return (
    <Sheet onClose={props.onClose}>
      <div className="trade-editor">
        <div className="sheet-title">{offer ? `Ваша сторона · ${offer.partner}` : 'Предложить обмен'}</div>
        {!offer && (
          <input
            className="search"
            placeholder="Имя героя"
            aria-label="Имя героя"
            value={draft.partner}
            onChange={(e) => set({ partner: e.target.value })}
          />
        )}
        {hints.length > 0 && (
          <div className="trade-hints">
            {hints.map((h) => (
              <button key={h.name} className="trade-hint" onClick={() => set({ partner: h.name })}>
                <span>{h.name}</span>
                <span className="lvl">ур. {num(h.lvl)}</span>
              </button>
            ))}
          </div>
        )}
        <div className="sub-title">Вы кладёте</div>
        {chosen.length === 0 ? (
          <p className="muted-note">Пока ничего — нажмите вещь ниже.</p>
        ) : (
          <div className="list">
            {chosen.map(({ t, ids }) => (
              <ItemRow
                key={`in-${t.ids[0]}`}
                item={{ ...t.item, count: ids.length }}
                aside={<span className="trade-mark">−</span>}
                onOpen={() => set({ ids: draft.ids.filter((id) => id !== ids[ids.length - 1]) })}
              />
            ))}
          </div>
        )}
        <div className="trade-fields">
          <label className="level-field">
            🪙
            <input
              inputMode="numeric"
              placeholder="0"
              aria-label="Серебро"
              value={draft.silver}
              onChange={(e) => set({ silver: digits(e.target.value) })}
            />
          </label>
          <label className="level-field">
            🟡
            <input
              inputMode="numeric"
              placeholder="0"
              aria-label="Дублоны"
              value={draft.doubloons}
              onChange={(e) => set({ doubloons: digits(e.target.value) })}
            />
          </label>
        </div>
        {(fee(draft.silver) > 0 || fee(draft.doubloons) > 0) && (
          <p className="sheet-note">
            Сверху — комиссия {data.feePct}%:{fee(draft.silver) > 0 && ` 🪙 ${num(fee(draft.silver))}`}
            {fee(draft.doubloons) > 0 && ` 🟡 ${num(fee(draft.doubloons))}`}. Меньше {num(data.minMoney)} не положить.
          </p>
        )}
        <textarea
          className="trade-text"
          placeholder="Записка: чего хотите взамен"
          maxLength={data.maxNote}
          value={draft.note}
          onChange={(e) => set({ note: e.target.value })}
        />
        {props.refusal && <p className="card card-warn trade-refusal">{props.refusal}</p>}
        <div className="buy-row">
          <button className="buy-yes" disabled={busy || !!data.away} onClick={props.onSend}>
            {offer ? 'Ответить' : 'Отправить'}
          </button>
          <button className="buy-no" onClick={props.onClose}>
            Отмена
          </button>
        </div>
        <p className="sheet-note">
          Положенное уйдёт из сумки, как только вы отправите, и вернётся, если обмен не состоится.
        </p>
        <div className="sub-title">При вас</div>
        {rest.length === 0 ? (
          <p className="muted-note">Больше положить нечего: сюжетные вещи и трофеи не передаются.</p>
        ) : (
          <div className="list">
            {rest.map(({ t, ids }) => (
              <ItemRow
                key={`out-${t.ids[0]}`}
                item={{ ...t.item, count: ids.length }}
                aside={<span className="trade-mark">+</span>}
                onOpen={() => set({ ids: [...draft.ids, ids[0]] })}
              />
            ))}
          </div>
        )}
      </div>
    </Sheet>
  )
}

/** «Обмен»: открытые предложения — свои и чужие, — карточка предложения и
  * своя сторона. Отозвать и отклонить можно откуда угодно, остальное — в
  * городе; бот проверит это сам, под тем же замком, что и ходы в чате. */
export function TradeTab(props: { data: TradeView; onChange: (v: TradeView) => void }) {
  const { data } = props
  const [openId, setOpenId] = useState<number | null>(null)
  const [accepting, setAccepting] = useState(false)
  const [draft, setDraft] = useState<Draft | null>(null)
  const [card, setCard] = useState<ItemView | null>(null)
  const [busy, setBusy] = useState(false)
  const [refusal, setRefusal] = useState<string | null>(null)
  const [toast, setToast] = useToast()
  const offer = data.offers.find((o) => o.id === openId)
  const editing = draft?.offer != null ? data.offers.find((o) => o.id === draft.offer) : undefined

  /** Действие: сделано — закрыть окно и сказать, что вышло; нет — причина там,
    * где игрок сейчас. */
  const run = (act: () => Promise<TradeActionView>, done: () => void) => {
    setBusy(true)
    setRefusal(null)
    act()
      .then((r) => {
        props.onChange(r.trade)
        if (r.done) {
          done()
          if (r.note) setToast(r.note)
        } else setRefusal(r.note ?? 'Не вышло.')
      })
      .catch(() => setToast('Ответ не дошёл. Обновите (⟳) и проверьте обмен.'))
      .finally(() => setBusy(false))
  }

  const send = () => {
    if (!draft) return
    const d = {
      items: draft.ids,
      silver: Number(draft.silver || 0),
      doubloons: Number(draft.doubloons || 0),
      note: draft.note.trim(),
    }
    run(
      () => (draft.offer != null ? api.tradeAnswer(draft.offer, d) : api.tradePropose(draft.partner.trim(), d)),
      () => {
        setDraft(null)
        setOpenId(null)
      },
    )
  }

  const edit = (o: TradeOfferView) => {
    setRefusal(null)
    setDraft({
      offer: o.id,
      partner: o.partner,
      ids: o.mine.items.flatMap((t) => t.ids),
      silver: o.mine.silver ? String(o.mine.silver) : '',
      doubloons: o.mine.doubloons ? String(o.mine.doubloons) : '',
      note: o.mine.note ?? '',
    })
  }

  return (
    <>
      {data.away && <section className="card card-warn storage-note">{data.away}</section>}
      <p className="muted-note">
        Меняйтесь с другими героями вещами и деньгами. Один предлагает, другой соглашается или отвечает своим, а
        подтверждает тот, чьи условия последние. Положенное в обмен ждёт в закладе: отказ, отзыв или срок вернут всё.
        С серебра и дублонов — комиссия {data.feePct}% сверху.
      </p>
      <button
        className="trade-new"
        disabled={!!data.away}
        onClick={() => {
          setRefusal(null)
          setDraft({ offer: null, partner: '', ids: [], silver: '', doubloons: '', note: '' })
        }}
      >
        ➕ Предложить обмен
      </button>
      {data.offers.length === 0 ? (
        <Empty>Открытых предложений нет.</Empty>
      ) : (
        <div className="list trade-list">
          {data.offers.map((o) => (
            <OfferRow
              key={o.id}
              offer={o}
              onOpen={() => {
                setAccepting(false)
                setRefusal(null)
                setOpenId(o.id)
              }}
            />
          ))}
        </div>
      )}
      {offer && !draft && (
        <Sheet onClose={() => setOpenId(null)}>
          <div className="sheet-title">
            Обмен №{offer.id} · {offer.partner}
          </div>
          <Side title="Вы отдаёте" side={offer.mine} onOpen={setCard} />
          <Side title="Вы получаете" side={offer.theirs} onOpen={setCard} />
          <p className="sheet-note">
            {turnNote(offer)} Ждёт ещё {left(offer.leftMs)}.
          </p>
          {refusal && <p className="card card-warn trade-refusal">{refusal}</p>}
          {accepting ? (
            <div className="buy-confirm">
              <p className="buy-ask">
                {offer.partnerAgreed ? 'Подтвердить обмен?' : 'Согласиться на обмен?'} Вы отдаёте: {offer.mine.summary}.
                Вы получаете: {offer.theirs.summary}.
                {!offer.partnerAgreed && ' Обмен состоится, когда партнёр подтвердит.'}
              </p>
              <div className="buy-row">
                <button
                  className="buy-yes"
                  disabled={busy}
                  onClick={() =>
                    run(
                      () => (offer.partnerAgreed ? api.tradeAccept(offer.id) : api.tradeAgree(offer.id)),
                      () => setOpenId(null),
                    )
                  }
                >
                  {offer.partnerAgreed ? 'Меняемся' : 'Соглашаюсь'}
                </button>
                <button className="buy-no" onClick={() => setAccepting(false)}>
                  Назад
                </button>
              </div>
            </div>
          ) : (
            <div className="equip-acts trade-acts">
              {offer.yourTurn && (
                <button className="equip-off trade-accept" disabled={busy || !!data.away} onClick={() => setAccepting(true)}>
                  {offer.partnerAgreed ? 'Подтвердить' : 'Согласиться'}
                </button>
              )}
              {offer.yourTurn && (
                <button className="equip-off" disabled={busy || !!data.away} onClick={() => edit(offer)}>
                  Изменить своё
                </button>
              )}
              <button
                className="equip-off trade-cancel"
                disabled={busy}
                onClick={() =>
                  run(
                    () => api.tradeCancel(offer.id),
                    () => setOpenId(null),
                  )
                }
              >
                {offer.proposed ? 'Отозвать' : 'Отклонить'}
              </button>
            </div>
          )}
        </Sheet>
      )}
      {draft && (
        <Editor
          data={data}
          offer={editing}
          draft={draft}
          busy={busy}
          refusal={refusal}
          onDraft={setDraft}
          onSend={send}
          onClose={() => setDraft(null)}
        />
      )}
      {card && (
        <Sheet onClose={() => setCard(null)}>
          <ItemText item={card} />
        </Sheet>
      )}
      {toast}
    </>
  )
}
