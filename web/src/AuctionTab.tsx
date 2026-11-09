import { useMemo, useState, type ReactNode } from 'react'
import { api } from './api'
import { matches } from './BagTab'
import { Silver } from './icons'
import { gearKinds, groupLabels, presentGroups, type Group } from './kinds'
import type { AuctionView, Currency, LotView } from './types'
import { Chips, Empty, ItemRow, ItemText, num, Sheet, Toolbar } from './ui'

const sorts = [
  { id: 'new', label: 'Сначала новые' },
  { id: 'cheap', label: 'Сначала дешёвые' },
  { id: 'dear', label: 'Сначала дорогие' },
  { id: 'lvl', label: 'По уровню' },
  { id: 'rarity', label: 'По редкости' },
]

const coin: Record<Currency, ReactNode> = { silver: <Silver />, doubloons: '🟡' }

function sortLots(lots: LotView[], sort: string): LotView[] {
  const by = (f: (a: LotView, b: LotView) => number) => [...lots].sort(f)
  switch (sort) {
    case 'cheap':
      return by((a, b) => a.price - b.price)
    case 'dear':
      return by((a, b) => b.price - a.price)
    case 'lvl':
      return by((a, b) => b.item.lvl - a.item.lvl || a.price - b.price)
    case 'rarity':
      return by((a, b) => b.item.rarityRank - a.item.rarityRank || a.price - b.price)
    default:
      return lots
  }
}

/** Что можно вписать в поле уровня: только цифры, не больше четырёх. */
const digits = (v: string) => v.replace(/\D/g, '').slice(0, 4)

function left(lot: LotView): string {
  if (lot.unsold) return ' · не продан'
  if (lot.hoursLeft >= 48) return ` · ещё ${Math.floor(lot.hoursLeft / 24)} дн`
  return lot.hoursLeft > 0 ? ` · ещё ${lot.hoursLeft} ч` : ' · меньше часа'
}

/** Что сказал Рахадим после покупки и купили ли. */
interface Said {
  note: string
  bought: boolean
}

/** Покупка из карточки лота: кнопка с ценой, а после неё — подтверждение, как в
  * боте. Чего не хватает (города, денег, места в сумке), видно сразу; бот всё
  * равно проверит заново — под тем же замком, что и ходы в чате. */
function Buy(props: { lot: LotView; data: AuctionView; onChange: (v: AuctionView, bought: boolean) => void }) {
  const { lot, data } = props
  const [confirm, setConfirm] = useState(false)
  const [busy, setBusy] = useState(false)
  const [said, setSaid] = useState<Said | null>(null)

  const price = (
    <>
      {coin[lot.currency]} {num(lot.price)}
    </>
  )
  const have = lot.currency === 'silver' ? data.silver : data.hero.doubloons
  // Лот ушёл (купили или сняли), пока карточка была открыта, — кнопок больше нет.
  const onSale = data.lots.some((l) => l.id === lot.id)

  const buy = () => {
    setBusy(true)
    api
      .auctionBuy(lot.id)
      .then((r) => {
        setSaid({ note: r.note, bought: r.bought })
        props.onChange(r.auction, r.bought)
      })
      .catch(() =>
        setSaid({ note: 'Ответ не дошёл. Обновите аукцион (⟳) и проверьте, прошла ли покупка.', bought: false }),
      )
      .finally(() => {
        setBusy(false)
        setConfirm(false)
      })
  }

  const note = said && <p className={said.bought ? 'buy-note buy-ok' : 'buy-note'}>{said.note}</p>
  if (said?.bought || !onSale) return note
  if (data.away)
    return (
      <>
        {note}
        <p className="buy-note">{data.away}</p>
      </>
    )
  const why =
    have < lot.price ? (
      <>
        Не хватает: у вас {coin[lot.currency]} {num(have)}
        {lot.currency === 'silver' && ' вместе с ячейкой'}.
      </>
    ) : data.bagFree <= 0 ? (
      'В сумке нет места.'
    ) : null

  return (
    <>
      {note}
      {confirm ? (
        <div className="buy-confirm">
          <p className="buy-ask">
            Купить «{lot.item.title}» за {price}?
          </p>
          <div className="buy-row">
            <button className="buy-yes" onClick={buy} disabled={busy}>
              {busy ? 'Покупаем…' : 'Да, купить'}
            </button>
            <button className="buy-no" onClick={() => setConfirm(false)} disabled={busy}>
              Отмена
            </button>
          </div>
        </div>
      ) : (
        <button className="buy-yes buy-wide" onClick={() => setConfirm(true)} disabled={!!why}>
          Купить за {price}
        </button>
      )}
      {why && <p className="sheet-note">{why}</p>}
    </>
  )
}

export function AuctionTab(props: { data: AuctionView; onChange: (v: AuctionView, bought: boolean) => void }) {
  const [view, setView] = useState<'all' | 'mine'>('all')
  const [group, setGroup] = useState<Group | 'all'>('all')
  // Вид снаряжения: Helmet, Ring… — тип вещи бота.
  const [kind, setKind] = useState('any')
  const [currency, setCurrency] = useState<Currency | 'any'>('any')
  // Границы уровня вещи, включительно; пусто — без границы.
  const [minLvl, setMinLvl] = useState('')
  const [maxLvl, setMaxLvl] = useState('')
  const [query, setQuery] = useState('')
  const [sort, setSort] = useState('new')
  const [open, setOpen] = useState<LotView | null>(null)

  const lots = view === 'all' ? props.data.lots : props.data.mine
  const groups = useMemo(() => presentGroups(lots.map((l) => l.item)), [lots])
  const shown = useMemo(
    () =>
      sortLots(
        lots.filter(
          (l) =>
            matches(l.item, group, query) &&
            (kind === 'any' || l.item.kind === kind) &&
            (currency === 'any' || l.currency === currency) &&
            (minLvl === '' || l.item.lvl >= Number(minLvl)) &&
            (maxLvl === '' || l.item.lvl <= Number(maxLvl)),
        ),
        sort,
      ),
    [lots, group, kind, query, currency, minLvl, maxLvl, sort],
  )

  // Вид снаряжения и группа не должны спорить: вид — это всегда «Снаряжение».
  const pickKind = (k: string) => {
    setKind(k)
    if (k !== 'any') setGroup('gear')
  }
  const pickGroup = (g: Group | 'all') => {
    setGroup(g)
    if (g !== 'gear') setKind('any')
  }

  return (
    <>
      <Chips
        options={[
          { id: 'all' as const, label: `Витрина · ${props.data.lots.length}` },
          { id: 'mine' as const, label: `Мои лоты · ${props.data.mine.length}` },
        ]}
        value={view}
        onChange={(v) => {
          setView(v)
          pickGroup('all')
        }}
      />
      {lots.length === 0 ? (
        <Empty>{view === 'all' ? 'На торгах пока пусто.' : 'Своих лотов у вас нет.'}</Empty>
      ) : (
        <>
          <Toolbar query={query} onQuery={setQuery} sort={sort} sorts={sorts} onSort={setSort} />
          <div className="toolbar">
            {(groups.includes('gear') || kind !== 'any') && (
              <select className="sort sort-wide" value={kind} onChange={(e) => pickKind(e.target.value)} aria-label="Вид снаряжения">
                <option value="any">Всё снаряжение</option>
                {gearKinds.map((k) => (
                  <option key={k.id} value={k.id}>
                    {k.label}
                  </option>
                ))}
              </select>
            )}
            <div className="level-field">
              Ур.
              <input
                inputMode="numeric"
                placeholder="от"
                aria-label="Уровень от"
                value={minLvl}
                onChange={(e) => setMinLvl(digits(e.target.value))}
              />
              –
              <input
                inputMode="numeric"
                placeholder="до"
                aria-label="Уровень до"
                value={maxLvl}
                onChange={(e) => setMaxLvl(digits(e.target.value))}
              />
            </div>
          </div>
          <Chips
            options={[
              { id: 'any' as const, label: 'Любая монета' },
              { id: 'silver' as const, label: <><Silver /> Серебро</> },
              { id: 'doubloons' as const, label: '🟡 Дублоны' },
            ]}
            value={currency}
            onChange={setCurrency}
          />
          {groups.length > 1 && (
            <Chips
              options={[{ id: 'all' as const, label: 'Всё' }, ...groups.map((g) => ({ id: g, label: groupLabels[g] }))]}
              value={group}
              onChange={pickGroup}
            />
          )}
          <div className="list">
            {shown.map((lot) => (
              <ItemRow
                key={lot.id}
                item={lot.item}
                note={left(lot)}
                onOpen={() => setOpen(lot)}
                aside={
                  <span className="price">
                    {lot.mine && view === 'all' && <span className="mine">ваш</span>}
                    <span>
                      {coin[lot.currency]} {lot.price.toLocaleString('ru-RU')}
                    </span>
                  </span>
                }
              />
            ))}
            {shown.length === 0 && <Empty>Ничего не нашлось.</Empty>}
          </div>
        </>
      )}
      {open && (
        <Sheet onClose={() => setOpen(null)}>
          <ItemText item={open.item} />
          <p className="sheet-price">
            Лот № {open.id} · {coin[open.currency]} {open.price.toLocaleString('ru-RU')}
          </p>
          {open.unsold ? (
            <p className="sheet-note">Срок вышел — заберите вещь в Торговом доме.</p>
          ) : open.mine ? (
            <p className="sheet-note">Ваш лот. Снять его можно в Торговом доме.</p>
          ) : (
            <Buy key={open.id} lot={open} data={props.data} onChange={props.onChange} />
          )}
        </Sheet>
      )}
    </>
  )
}
