// Параметры запуска для проверки без ВК: подписывает их тем же ключом, что и
// ВК, и печатает адрес дев-сервера, открывающий приложение от имени игрока.
//
//   npm run sign -- <VK_APP_SECRET бота> <vk_user_id игрока>
//
// Ключ — тот же, что у локального бота в VK_APP_SECRET; на бою — не нужен.
import crypto from 'node:crypto'
import querystring from 'node:querystring'

const [secret, user] = process.argv.slice(2)
if (!secret || !user) {
  console.error('npm run sign -- <VK_APP_SECRET> <vk_user_id>')
  process.exit(1)
}

const params = {
  vk_app_id: '0',
  vk_language: 'ru',
  vk_platform: 'desktop_web',
  vk_ts: String(Math.floor(Date.now() / 1000)),
  vk_user_id: user,
}
const ordered = Object.fromEntries(Object.entries(params).sort(([a], [b]) => (a < b ? -1 : 1)))
const sign = crypto.createHmac('sha256', secret).update(querystring.stringify(ordered)).digest('base64url')
console.log(`http://localhost:5173/?${querystring.stringify({ ...params, sign })}`)
