package pangea.test

import pangea.engine.Players
import pangea.model.user.{User, UserId}
import zio.{Task, ZIO}

class TestPlayers extends Players {
  private var announcements: List[String]        = Nil
  private var tradeAnnouncements: List[String]   = Nil
  private var letters: List[(UserId, String)]    = Nil

  def getDisplayName(user: User): Task[String] = ZIO.succeed("Иван Иванов")

  def announce(message: String): Task[Unit] = ZIO.succeed { announcements = announcements :+ message }

  def announceTrade(message: String): Task[Unit] = ZIO.succeed { tradeAnnouncements = tradeAnnouncements :+ message }

  def notify(user: User, message: String): Task[Unit] =
    ZIO.succeed { letters = letters :+ (user.userId -> message) }

  /** Что улетело в общую беседу Пангеи. */
  def announced: List[String] = announcements

  /** Что улетело в торговую беседу. */
  def announcedTrade: List[String] = tradeAnnouncements

  /** Письма в личку: кому и что. */
  def sentLetters: List[(UserId, String)] = letters
}
