import type { ItemView } from './types'

export type Group = 'gear' | 'gems' | 'materials' | 'brews' | 'trophies' | 'other'

export const groupLabels: Record<Group, string> = {
  gear: 'Снаряжение',
  gems: 'Камни и руны',
  materials: 'Материалы',
  brews: 'Отвары',
  trophies: 'Трофеи',
  other: 'Прочее',
}

interface Kind {
  label: string
  icon: string
  group: Group
}

/** Типы вещей бота (ItemType): подпись, значок и группа фильтра. */
const kinds: Record<string, Kind> = {
  Helmet: { label: 'Шлем', icon: '🪖', group: 'gear' },
  ShoulderPads: { label: 'Наплечники', icon: '🎽', group: 'gear' },
  ChestPlate: { label: 'Нагрудник', icon: '🥋', group: 'gear' },
  Bracelets: { label: 'Браслеты', icon: '📿', group: 'gear' },
  Gloves: { label: 'Перчатки', icon: '🧤', group: 'gear' },
  Pants: { label: 'Штаны', icon: '👖', group: 'gear' },
  Leggings: { label: 'Поножи', icon: '👖', group: 'gear' },
  Boots: { label: 'Сапоги', icon: '👢', group: 'gear' },
  Amulet: { label: 'Амулет', icon: '🧿', group: 'gear' },
  Ring: { label: 'Кольцо', icon: '💍', group: 'gear' },
  Belt: { label: 'Пояс', icon: '🎗️', group: 'gear' },
  Flask: { label: 'Фляга', icon: '🧪', group: 'gear' },
  Weapon: { label: 'Оружие', icon: '⚔️', group: 'gear' },
  AdditionalWeapon: { label: 'Доп. оружие', icon: '🗡️', group: 'gear' },
  Shield: { label: 'Щит', icon: '🛡️', group: 'gear' },
  Gem: { label: 'Камень', icon: '💎', group: 'gems' },
  RuneStone: { label: 'Рунный камень', icon: '🪨', group: 'gems' },
  Material: { label: 'Материал', icon: '⛏️', group: 'materials' },
  Brew: { label: 'Отвар', icon: '🍵', group: 'brews' },
  Trophy: { label: 'Трофей', icon: '🦴', group: 'trophies' },
  NamedTrophy: { label: 'Уникальный трофей', icon: '🏆', group: 'trophies' },
  TreasureMap: { label: 'Карта клада', icon: '🗺️', group: 'other' },
  TreasureMapHalf: { label: 'Половина карты', icon: '📜', group: 'other' },
  Rose: { label: 'Роза', icon: '🌹', group: 'other' },
  QuestItem: { label: 'Сюжетный предмет', icon: '⭐', group: 'other' },
}

const unknown: Kind = { label: 'Вещь', icon: '📦', group: 'other' }

export const kindOf = (item: ItemView): Kind => kinds[item.kind] ?? unknown

/** Группы, которые есть среди вещей, — в порядке groupLabels. */
export function presentGroups(items: ItemView[]): Group[] {
  const present = new Set(items.map((i) => kindOf(i).group))
  return (Object.keys(groupLabels) as Group[]).filter((g) => present.has(g))
}
