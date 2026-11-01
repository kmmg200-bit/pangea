import bridge from '@vkontakte/vk-bridge'

/** Параметры запуска — строка запроса адреса, как её дал ВК. Подписаны ВК:
  * по ним бот узнаёт игрока (см. VkLaunchParams на стороне бота). */
export const launchParams = window.location.search.replace(/^\?/, '')

/** Тема — как в клиенте ВК; вне ВК остаётся системная (prefers-color-scheme). */
export function initPlatform(): void {
  bridge.subscribe((event) => {
    if (event.detail.type !== 'VKWebAppUpdateConfig') return
    const data = event.detail.data as { appearance?: string; scheme?: string }
    const dark = data.appearance
      ? data.appearance === 'dark'
      : data.scheme === 'space_gray' || data.scheme === 'vkcom_dark'
    document.documentElement.dataset.theme = dark ? 'dark' : 'light'
  })
  bridge.send('VKWebAppInit').catch(() => undefined)
}
