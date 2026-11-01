import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// base './' — приложение открывается из любого пути: со своего домена или с
// хостинга ВК. В разработке /api уходит на локального бота (порт 8080), а
// туннель ВК (vk-tunnel) пускается к дев-серверу по своему адресу.
export default defineConfig({
  base: './',
  plugins: [react()],
  server: {
    host: true,
    allowedHosts: ['.vk-apps.com'],
    proxy: { '/api': 'http://localhost:8080' },
  },
})
