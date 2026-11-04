package server.model

import zio.{Task, ZIO}

/** Кем работает процесс — переменная `ROLE`. Образ один, роли разные:
  *
  *  - `collector` — только приём событий ВК: колбэк (`POST /`) и Long Poll
  *    кладут их в очередь `vk_inbox`. Выкладывается редко: пока выкатывают
  *    бота, ВК всегда отвечает он;
  *  - `bot` — всё остальное: разбор очереди (ходы), таймеры, фоновые циклы,
  *    касса, Mini App, логи;
  *  - `all` — то и другое в одном процессе: так было до разделения, так
  *    удобно и локально. Не задана — `all`. */
sealed abstract class Role(val name: String)

object Role {
  case object All       extends Role("all")
  case object Collector extends Role("collector")
  case object Bot       extends Role("bot")

  val values: List[Role] = List(All, Collector, Bot)

  val EnvName: String = "ROLE"

  /** Роль из окружения. Незнакомая — ошибка на старте: опечатка в роли не
    * должна молча поднять не тот процесс. */
  val fromEnv: Task[Role] =
    ZIO.attempt(Option(System.getenv(EnvName)).map(_.trim).filter(_.nonEmpty).getOrElse(All.name)).flatMap(name =>
      ZIO.fromOption(values.find(_.name == name))
        .orElseFail(new IllegalArgumentException(s"$EnvName=$name: такой роли нет, есть ${values.map(_.name).mkString(", ")}")))
}
