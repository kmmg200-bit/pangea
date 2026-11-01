import { useMemo, useState } from 'react'
import { groupLabels, kindOf, presentGroups, type Group } from './kinds'
import type { InventoryView, ItemView } from './types'
import { Chips, Empty, ItemRow, ItemText, Sheet, Toolbar } from './ui'

const sorts = [
  { id: 'bag', label: 'Как в сумке' },
  { id: 'rarity', label: 'По редкости' },
  { id: 'lvl', label: 'По уровню' },
]

export function sortItems(items: ItemView[], sort: string): ItemView[] {
  if (sort === 'rarity') return [...items].sort((a, b) => b.rarityRank - a.rarityRank || b.lvl - a.lvl)
  if (sort === 'lvl') return [...items].sort((a, b) => b.lvl - a.lvl || b.rarityRank - a.rarityRank)
  return items
}

export function matches(item: ItemView, group: Group | 'all', query: string): boolean {
  return (
    (group === 'all' || kindOf(item).group === group) &&
    (query.trim() === '' || item.title.toLowerCase().includes(query.trim().toLowerCase()))
  )
}

export function BagTab(props: { data: InventoryView }) {
  const [group, setGroup] = useState<Group | 'all'>('all')
  const [query, setQuery] = useState('')
  const [sort, setSort] = useState('bag')
  const [open, setOpen] = useState<ItemView | null>(null)

  const items = props.data.items
  const groups = useMemo(() => presentGroups(items), [items])
  const shown = useMemo(
    () => sortItems(items.filter((i) => matches(i, group, query)), sort),
    [items, group, query, sort],
  )

  return (
    <>
      <div className="summary">
        Занято {props.data.occupied} из {props.data.capacity}
      </div>
      {items.length === 0 ? (
        <Empty>Сумка пуста.</Empty>
      ) : (
        <>
          <Toolbar query={query} onQuery={setQuery} sort={sort} sorts={sorts} onSort={setSort} />
          {groups.length > 1 && (
            <Chips
              options={[{ id: 'all' as const, label: 'Всё' }, ...groups.map((g) => ({ id: g, label: groupLabels[g] }))]}
              value={group}
              onChange={setGroup}
            />
          )}
          <div className="list">
            {shown.map((item) => (
              <ItemRow key={item.id} item={item} onOpen={() => setOpen(item)} />
            ))}
            {shown.length === 0 && <Empty>Ничего не нашлось.</Empty>}
          </div>
        </>
      )}
      {open && (
        <Sheet onClose={() => setOpen(null)}>
          <ItemText item={open} />
          {open.count > 1 && <p className="sheet-note">В сумке: {open.count} шт.</p>}
        </Sheet>
      )}
    </>
  )
}
