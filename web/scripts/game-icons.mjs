// Собирает src/gameIcons.ts — только те иконки game-icons.net, что нужны
// приложению, из пакета @iconify-json/game-icons (лицензия CC BY 3.0).
// Новая иконка: добавить имя в список и запустить `npm run icons`.
import { readFileSync, writeFileSync } from 'node:fs'
import { createRequire } from 'node:module'

const names = [
  // Вещи по типам (см. kinds.ts)
  'visored-helm', 'pauldrons', 'chest-armor', 'bracer', 'gauntlet', 'armored-pants', 'leg-armor',
  'leather-boot', 'gem-pendant', 'diamond-ring', 'belt', 'round-potion', 'broadsword', 'bow-arrow',
  'plain-dagger', 'checked-shield', 'fangs', 'crowned-skull', 'treasure-map', 'folded-paper', 'emerald',
  'ore', 'herbs-bundle', 'powder-bag', 'tied-scroll', 'bubbling-flask', 'rune-stone', 'rose', 'swap-bag',
  // Нижняя панель (см. App.tsx)
  'hooded-figure', 'scroll-quill', 'three-friends', 'knapsack', 'gavel',
]

const require = createRequire(import.meta.url)
const set = JSON.parse(readFileSync(require.resolve('@iconify-json/game-icons/icons.json'), 'utf8'))
const size = set.width ?? 512
const lines = names.map((n) => {
  const icon = set.icons[n]
  if (!icon) throw new Error(`Нет иконки ${n} в game-icons`)
  if ((icon.width ?? size) !== 512 || (icon.height ?? size) !== 512) throw new Error(`${n}: не 512×512`)
  return `  '${n}': ${JSON.stringify(icon.body)},`
})

writeFileSync(
  new URL('../src/gameIcons.ts', import.meta.url),
  `// Иконки game-icons.net — Lorc, Delapouite и другие авторы, лицензия CC BY 3.0
// (https://game-icons.net, https://creativecommons.org/licenses/by/3.0/).
// Файл собирает scripts/game-icons.mjs — не править руками. Холст у всех 512×512.

export const gameIcons = {
${lines.join('\n')}
} as const

export type GameIcon = keyof typeof gameIcons
`,
)
console.log(`gameIcons.ts: ${names.length} иконок`)
