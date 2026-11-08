import bridge from '@vkontakte/vk-bridge'

/** Параметры запуска — строка запроса адреса, как её дал ВК. Подписаны ВК:
  * по ним бот узнаёт игрока (см. VkLaunchParams на стороне бота). */
export const launchParams = window.location.search.replace(/^\?/, '')

/** Приложение ВК на телефоне: поверх правого верхнего угла оно рисует свою
  * капсулу «⋯ ✕» — там ничего своего держать нельзя. В мобильном браузере и на
  * компьютере капсулы поверх приложения нет. */
const platform = new URLSearchParams(launchParams).get('vk_platform') ?? ''
export const vkCapsule = platform.startsWith('mobile_') && platform !== 'mobile_web'

/** Тема — как в клиенте ВК; вне ВК остаётся системная (prefers-color-scheme). */
export function initPlatform(): void {
  if (vkCapsule) document.documentElement.dataset.capsule = 'on'
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

/** Лёгкий отклик вибрацией — когда вещь «взялась» пальцем. Вне ВК молчит. */
export function tap(): void {
  bridge.send('VKWebAppTapticImpactOccurred', { style: 'light' }).catch(() => undefined)
}
