import { useMemo, useState } from 'react'
import { matches } from './BagTab'
import { groupLabels, presentGroups, type Group } from './kinds'
import type { AuctionView, Currency, LotView } from './types'
import { Chips, Empty, ItemRow, ItemText, Sheet, Toolbar } from './ui'

const sorts = [
  { id: 'new', label: 'Сначала новые' },
  { id: 'cheap', label: 'Сначала дешёвые' },
  { id: 'dear', label: 'Сначала дорогие' },
  { id: 'lvl', label: 'По уровню' },
  { id: 'rarity', label: 'По редкости' },
]

const coin: Record<Currency, string> = { silver: '🪙', doubloons: '🟡' }

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

function left(lot: LotView): string {
  if (lot.unsold) return ' · не продан'
  if (lot.hoursLeft >= 48) return ` · ещё ${Math.floor(lot.hoursLeft / 24)} дн`
  return lot.hoursLeft > 0 ? ` · ещё ${lot.hoursLeft} ч` : ' · меньше часа'
}

export function AuctionTab(props: { data: AuctionView }) {
  const [view, setView] = useState<'all' | 'mine'>('all')
  const [group, setGroup] = useState<Group | 'all'>('all')
  const [currency, setCurrency] = useState<Currency | 'any'>('any')
  const [query, setQuery] = useState('')
  const [sort, setSort] = useState('new')
  const [open, setOpen] = useState<LotView | null>(null)

  const lots = view === 'all' ? props.data.lots : props.data.mine
  const groups = useMemo(() => presentGroups(lots.map((l) => l.item)), [lots])
  const shown = useMemo(
    () =>
      sortLots(
        lots.filter((l) => matches(l.item, group, query) && (currency === 'any' || l.currency === currency)),
        sort,
      ),
    [lots, group, query, currency, sort],
  )

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
          setGroup('all')
        }}
      />
      {lots.length === 0 ? (
        <Empty>{view === 'all' ? 'На торгах пока пусто.' : 'Своих лотов у вас нет.'}</Empty>
      ) : (
        <>
          <Toolbar query={query} onQuery={setQuery} sort={sort} sorts={sorts} onSort={setSort} />
          <Chips
            options={[
              { id: 'any' as const, label: 'Любая монета' },
              { id: 'silver' as const, label: '🪙 Серебро' },
              { id: 'doubloons' as const, label: '🟡 Дублоны' },
            ]}
            value={currency}
            onChange={setCurrency}
          />
          {groups.length > 1 && (
            <Chips
              options={[{ id: 'all' as const, label: 'Всё' }, ...groups.map((g) => ({ id: g, label: groupLabels[g] }))]}
              value={group}
              onChange={setGroup}
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
                    {coin[lot.currency]} {lot.price.toLocaleString('ru-RU')}
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
          <p className="sheet-note">
            {open.unsold
              ? 'Срок вышел — заберите вещь в Торговом доме.'
              : open.mine
                ? 'Ваш лот. Снять его можно в Торговом доме.'
                : 'Купить можно в Торговом доме, в городе.'}
          </p>
        </Sheet>
      )}
    </>
  )
}
