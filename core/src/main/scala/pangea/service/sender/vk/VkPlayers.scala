package pangea.service.sender.vk

import pangea.dao.hero.HeroDao
import pangea.engine.Players
import pangea.model.user.User
import pangea.service.sender.{Api, ChatTarget}
import zio.{Task, ZIO}

class VkPlayers(api: Api, heroDao: HeroDao) extends Players {

  /** Имя героя, а у кого его нет (прошёл пролог до имён) — имя из ВК. */
  def getDisplayName(user: User): Task[String] =
    heroDao.getHeroByUserId(user.userId).map(_.flatMap(_.name)).flatMap {
      case Some(name) => ZIO.succeed(name)
      case None       => vkName(user)
    }

  def announce(message: String): Task[Unit] = api.sendToChat(ChatTarget.General, message)

  def announceTrade(message: String): Task[Unit] = api.sendToChat(ChatTarget.Trade, message)

  def announceAt(chat: ChatTarget.Peer, message: String): Task[Unit] = api.sendToChat(chat, message)

  def notify(user: User, message: String): Task[Unit] = api.sendMessage(user, message, Nil, None)

  private def vkName(user: User): Task[String] =
    api.getName(user)
      .flatMap(r => ZIO.fromOption(r.response.headOption).orElseFail(new Throwable(s"ВК не вернул имя игрока ${user.userId.value}")))
      .map(u => s"${u.firstName} ${u.lastName}")
}

object VkPlayers {
  def apply(api: Api, heroDao: HeroDao): VkPlayers = new VkPlayers(api, heroDao)
}
