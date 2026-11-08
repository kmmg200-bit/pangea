import type { GameIcon } from './gameIcons'
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
  icon: GameIcon
  group: Group
}

/** Типы вещей бота (ItemType): подпись, значок и группа фильтра. */
const kinds: Record<string, Kind> = {
  Helmet: { label: 'Шлем', icon: 'visored-helm', group: 'gear' },
  ShoulderPads: { label: 'Наплечники', icon: 'pauldrons', group: 'gear' },
  ChestPlate: { label: 'Нагрудник', icon: 'chest-armor', group: 'gear' },
  Bracelets: { label: 'Браслеты', icon: 'bracer', group: 'gear' },
  Gloves: { label: 'Перчатки', icon: 'gauntlet', group: 'gear' },
  Pants: { label: 'Штаны', icon: 'armored-pants', group: 'gear' },
  Leggings: { label: 'Поножи', icon: 'leg-armor', group: 'gear' },
  Boots: { label: 'Сапоги', icon: 'leather-boot', group: 'gear' },
  Amulet: { label: 'Амулет', icon: 'gem-pendant', group: 'gear' },
  Ring: { label: 'Кольцо', icon: 'diamond-ring', group: 'gear' },
  Belt: { label: 'Пояс', icon: 'belt', group: 'gear' },
  Flask: { label: 'Фляга', icon: 'round-potion', group: 'gear' },
  Weapon: { label: 'Оружие', icon: 'broadsword', group: 'gear' },
  AdditionalWeapon: { label: 'Доп. оружие', icon: 'plain-dagger', group: 'gear' },
  Shield: { label: 'Щит', icon: 'checked-shield', group: 'gear' },
  Gem: { label: 'Камень', icon: 'emerald', group: 'gems' },
  RuneStone: { label: 'Рунный камень', icon: 'rune-stone', group: 'gems' },
  Material: { label: 'Материал', icon: 'ore', group: 'materials' },
  Brew: { label: 'Отвар', icon: 'bubbling-flask', group: 'brews' },
  Trophy: { label: 'Трофей', icon: 'fangs', group: 'trophies' },
  NamedTrophy: { label: 'Уникальный трофей', icon: 'crowned-skull', group: 'trophies' },
  TreasureMap: { label: 'Карта клада', icon: 'treasure-map', group: 'other' },
  TreasureMapHalf: { label: 'Половина карты', icon: 'folded-paper', group: 'other' },
  Rose: { label: 'Роза', icon: 'rose', group: 'other' },
  QuestItem: { label: 'Сюжетный предмет', icon: 'tied-scroll', group: 'other' },
}

const unknown: Kind = { label: 'Вещь', icon: 'swap-bag', group: 'other' }

export const kindOf = (item: ItemView): Kind => kinds[item.kind] ?? unknown

/** Значок вещи: по типу, а внутри типа — по подвиду (лук среди оружия, трава и
  * пыль среди материалов). */
const variantIcons: Record<string, GameIcon> = { ranged: 'bow-arrow', herb: 'herbs-bundle', dust: 'powder-bag' }

export const iconOf = (item: ItemView): GameIcon => (item.variant && variantIcons[item.variant]) || kindOf(item).icon

/** Группы, которые есть среди вещей, — в порядке groupLabels. */
export function presentGroups(items: ItemView[]): Group[] {
  const present = new Set(items.map((i) => kindOf(i).group))
  return (Object.keys(groupLabels) as Group[]).filter((g) => present.has(g))
}
