package pangea.service.state.states.hero

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.home.HomeRates
import pangea.model.item.{BrewEffect, BrewKind, BrewRates, ItemSet, PassiveKind}
import pangea.model.rune.Rune
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.stats.StatBoost
import pangea.model.user.User
import pangea.service.state.states.gustavo.BoostStat
import pangea.service.state.{AzatData, ItemMenu, State, UiScene, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Экран «Эффекты» — первой кнопкой «Зелья и бафы» (всё временное, см.
 * [[SkillsState.buffs]]), а за ней — список кнопками ВСЕХ активных умений,
 * пассивок и наборов, которые сейчас действуют на герое: активные — с оружия/нагрудника и клейм на
 * теле ([[Hero.activeSkillSlots]]), пассивные — со всего остального снаряжения
 * и клейм ([[Hero.passives]]), наборы — по порогам. У руны с клейма — пометка
 * «на теле», в описании — понимание. Умение и на вещи, и на теле — две кнопки:
 * с вещи и «на теле».
 * Нажатие на кнопку показывает полное описание; список — только просмотр.
 */
case class SkillsState(heroDao: HeroDao, content: SceneContent) extends State {
  import SkillsState._

  private val branch = new Branch(
    routes = Map(
      // Назад из карточки — на ту же страницу, откуда ушли.
      // Назад из карточки — на ту же страницу: сцену трогать незачем,
      // в ней только номер страницы и есть.
      "SkillsList"     -> Target.Run { (u, _, r) => showList(u, r).as(StateType.Skills) },
      "SkillsPrev"     -> Target.Run { (u, _, r) => navigate(u, r, -1) },
      "SkillsNext"     -> Target.Run { (u, _, r) => navigate(u, r, +1) },
      "BackFromSkills" -> Target.Goto(StateType.HeroStats)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = Set(StateType.HeroStats, StateType.Skills)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    writeScene(user, SkillsScene(page = Some(0))) *> showList(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Список ──────────────────────────────────────────────────────────────────

  private def showList(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- allEntries(user, hero)
      scene <- readScene(user)
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.text("skills.empty"),
               List(content.choice("BackFromSkills", "skills.back"))))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(items, scene.page.getOrElse(0))
             val header  = content.text("skills.header") + (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val btns    = pageItems.zipWithIndex.map { case (e, idx) =>
               Choice(e.buttonId, ItemMenu.truncate(e.label), row = Some(idx))
             }
             val nav = navRow(page, totalPages)
             renderer.show(user, Screen(header, btns ++ nav))
           }
    } yield StateType.Skills

  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- allEntries(user, hero)
      (_, totalPages, _) = ItemMenu.page(items, 0)
      scene <- readScene(user)
      cur    = scene.page.getOrElse(0)
      np     = (cur + delta).max(0).min(totalPages - 1)
      _     <- writeScene(user, scene.copy(page = Some(np)))
      res   <- showList(user, renderer)
    } yield res

  private def navRow(page: Int, totalPages: Int): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice("BackFromSkills", "skills.back").copy(row = Some(row))),
      Option.when(page > 0)(content.choice("SkillsPrev", "common.prev").copy(row = Some(row))),
      Option.when(page < totalPages - 1)(content.choice("SkillsNext", "common.next").copy(row = Some(row)))
    ).flatten
  }

  // ── Детальный экран одного умения ──────────────────────────────────────────

  private def showDetail(user: User, entry: SkillEntry, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      text  = s"${entry.label}\n\n${entry.describe(hero)}"
      _    <- renderer.show(user, Screen(text,
                List(content.choice("SkillsList", "skills.back").copy(color = ChoiceColor.Negative))))
    } yield StateType.Skills

  // ── Fallback: динамические id вида ActiveSkill_<itemId> / PassiveSkill_<kind> ─

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- allEntries(user, hero)
      res   <- parseAction(ua.payload)
                 .flatMap(a => items.find(_.buttonId == a)) match {
                   case Some(entry) => showDetail(user, entry, renderer)
                   case None        => showList(user, renderer)
                 }
    } yield res

  /** Весь список «Эффектов»: зелья и бафы — сроки на «сейчас», благословение
   *  из `azat_data`, — а за ними умения, камни и наборы. */
  private def allEntries(user: User, hero: Hero): Task[List[SkillEntry]] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      azat <- AzatData.read(heroDao, user.userId)
    } yield entries(hero, buffs(content, hero, now, azat.blessingUntil))

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def readScene(user: User): Task[SkillsScene] =
    UiScene.read(heroDao, user.userId, UiScene.Skills, SkillsScene())

  private def writeScene(user: User, scene: SkillsScene): Task[Unit] =
    UiScene.write(heroDao, user.userId, UiScene.Skills, scene)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object SkillsState {

  /** Одно умение/пассивка в списке: активные — с оружия/нагрудника (ключ —
   *  id предмета, т.к. у двух предметов с «одним» скиллом независимые кулдауны),
   *  пассивные — по виду пассивки (дубли уже схлопнуты в `Equipment.passiveKinds`). */
  sealed trait SkillEntry {
    def buttonId: String
    def label: String
    def describe(hero: Hero): String
  }
  object SkillEntry {
    /** `worn` — сколько надетых вещей несут это умение (см. [[times]]);
     *  `twin` — оно же есть и по другую сторону: на теле у вещи, на вещи у
     *  клейма (см. [[twinNote]]). */
    final case class Active(itemId: Long, skill: Skill, worn: Int = 1, twin: Boolean = false) extends SkillEntry {
      private def onBody: Boolean = Rune.isBodySlot(itemId)
      def buttonId: String = s"$ActivePrefix$itemId"
      def label: String    = if (onBody) s"${skill.label} (на теле)" else skill.label + times(worn)
      def describe(hero: Hero): String =
        skill.describe(hero) + (if (onBody) "" else sameSkills(worn)) + twinNote(twin, onBody)
    }
    final case class Passive(kind: PassiveKind, onBody: Boolean, worn: Int = 1, twin: Boolean = false) extends SkillEntry {
      def buttonId: String = s"$PassivePrefix${kind.entryName}" + (if (onBody) BodySuffix else "")
      def label: String    = if (onBody) s"${kind.label} (на теле)" else kind.label + times(worn)
      def describe(hero: Hero): String = {
        val n = hero.runes.understandingOf(Rune.Passive(kind))
        kind.describe + (if (n > 0L) s" Понимание: $n." else "") +
          (if (onBody) "" else sameSkills(worn)) + twinNote(twin, onBody)
      }
    }

    /** Набор снаряжения: в списке — сколько предметов надето («Дикое пламя 6»),
     *  в описании — только те пороги, до которых игрок добрал. Скрытый шаг
     *  малого расового набора чужой расе не виден: и счёт, и пороги у неё
     *  кончаются на девяти (см. `HeroSets.shownPieces`). */
    final case class Set(set: ItemSet, pieces: Int) extends SkillEntry {
      def buttonId: String = s"$SetPrefix${set.entryName}"
      def label: String    = s"🛡 ${set.label} $pieces"
      def describe(hero: Hero): String = {
        val sets   = hero.sets
        val worn   = sets.shownPieces(set)
        val steps  = sets.thresholds(set)
        val header = s"Надето предметов: $worn из ${steps.maxOption.getOrElse(worn)}"
        val lines  = sets.bonuses(set).map { b =>
          val tail = if (b.active) "" else " (пока не действует)"
          s"• ${b.pieces}: ${b.text}$tail"
        }
        val next = steps.find(_ > worn)
          .map(n => s"\n\nСледующий бонус — при $n предметах.")
          .getOrElse("")
        (header +: lines).mkString("\n") + next
      }
    }

    /** Эффекты оружия: камни в гнёздах оружия и пыль на нём — и итог: как
     *  оружие бьёт по броне и по HP и что ещё даёт удару (см. [[weaponFx]]). */
    case object WeaponFx extends SkillEntry {
      def buttonId: String = "Fx_Weapon"
      def label: String    = "⚔ Эффекты оружия"
      def describe(hero: Hero): String = weaponFx(hero)
    }

    /** Эффекты доспехов: камни во всём снаряжении, кроме оружия (щит — здесь),
     *  и их сумма (см. [[armorFx]]). */
    case object ArmorFx extends SkillEntry {
      def buttonId: String = "Fx_Armor"
      def label: String    = "🛡 Эффекты доспехов"
      def describe(hero: Hero): String = armorFx(hero)
    }

    /** Зелья и бафы: всё временное, что сейчас действует на герое, по строке
     *  на каждое (см. [[buffs]]). Строки готовые: в них сроки на «сейчас»
     *  и благословение, которое живёт не в герое, а в `azat_data`. */
    final case class Buffs(label: String, lines: List[String]) extends SkillEntry {
      def buttonId: String = "Fx_Buffs"
      def describe(hero: Hero): String = lines.mkString("\n")
    }
  }

  private val ActivePrefix  = "ActiveSkill_"
  private val PassivePrefix = "PassiveSkill_"
  private val SetPrefix     = "Set_"
  private val BodySuffix    = "_body"

  /** «Тайник ×2»: умение пришло с нескольких надетых вещей. С одной — без
   *  числа, иначе «×1» стоял бы почти у всего списка. */
  private def times(worn: Int): String = if (worn > 1) s" ×$worn" else ""

  /** Умение и на вещи, и на теле: в карточке каждой из двух кнопок — что второе
   *  такое же есть и что они не складываются. */
  private def twinNote(twin: Boolean, onBody: Boolean): String =
    if (!twin) ""
    else if (onBody) "\n\nЭто же умение есть и на надетой вещи. Одинаковые умения не складываются — действует одно."
    else "\n\nЭто же умение выжжено у вас на теле. Одинаковые умения не складываются — действует одно."

  /** В карточке — почему ×2 не вдвое сильнее: одинаковые умения не складываются. */
  private def sameSkills(worn: Int): String =
    if (worn > 1) s"\n\nНадето вещей с этим умением: $worn. Одинаковые умения не складываются — действует одно."
    else ""

  /** Все умения/пассивки/наборы, реально действующие на герое прямо сейчас —
   *  снятые с надетого снаряжения. Активные (оружие/нагрудник) идут первыми,
   *  затем пассивки по алфавиту метки, затем наборы, набравшие хотя бы один
   *  порог — стабильный порядок между перерисовками. Одинаковые вещи схлопнуты
   *  в одну кнопку, а сколько их — видно по «×2»; умение, которое ещё и
   *  выжжено на теле, получает рядом вторую кнопку «на теле». */
  def entries(hero: Hero, buffs: Option[SkillEntry] = None): List[SkillEntry] = buffs.toList ++ skills(hero)

  /** Зелья и бафы — по строке на всё временное, что действует на герое сейчас:
   *  благословение Азата (живёт в `azat_data`, поэтому приходит параметром), сон
   *  дома, зелья Густаво и отвары, зелье застоя Фета и то, что на оружии до
   *  ближайшего боя. Ничего нет — нет и кнопки. Травмы — не бафы: они на
   *  «Персонаже». */
  def buffs(content: SceneContent, hero: Hero, nowMs: Long, blessingUntil: Option[Long]): Option[SkillEntry.Buffs] = {
    def t(key: String, args: (String, String)*): String = content.format(s"skills.buffs.$key", args: _*)
    def left(until: Long): String = pangea.util.TimeLeft.format(until - nowMs)
    val dust     = hero.weaponDust
    val blessing = blessingUntil.filter(_ > nowMs).map(u => t("blessing", "left" -> left(u)))
    val boosts   = hero.statBoosts.active(nowMs).map(b => boostLine(content, b, left(b.until)))
    val frozen   = Option.when(hero.expFrozen)(t("frozen"))
    val layers   = Option.when(dust.layers.nonEmpty)(t("dust", "names" ->
                     dust.layers.groupBy(_.displayName).toList
                       .sortBy { case (n, _) => dust.layers.indexWhere(_.displayName == n) }
                       .map { case (n, xs) => if (xs.sizeIs > 1) s"$n ×${xs.size}" else n }
                       .mkString(", ")))
    val coat     = dust.coat.map {
                     case pangea.model.hero.WeaponCoat.Poison   => t("coatPoison")
                     case pangea.model.hero.WeaponCoat.Bleed    => t("coatBleed")
                     case pangea.model.hero.WeaponCoat.RedVenom => t("coatRedVenom")
                   }
    val mirrors  = Option.when(dust.mirrors > 0)(t("mirrors", "n" -> dust.mirrors.toString))
    val lines    = blessing.toList ++ boosts ++ frozen ++ layers ++ coat ++ mirrors
    Option.when(lines.nonEmpty)(SkillEntry.Buffs(content.text("skills.buffs.label"), lines))
  }

  /** Строка одного бафа из `Hero.statBoosts`. Отвары с тайной — зовы и пузырь
   *  — только с названием и сроком: что они делают, игрок узнаёт сам. Баф, о
   *  котором здесь не знают, — «особый эффект» со сроком, но не пропадает. */
  private def boostLine(content: SceneContent, b: StatBoost, left: String): String = {
    def t(key: String, args: (String, String)*): String = content.format(s"skills.buffs.$key", args: _*)
    def brew(effect: BrewEffect): String = BrewKind.values.find(_.effect == effect).fold("")(_.label)
    b.name match {
      case HomeRates.RestedBoost =>
        t("rested", "pct" -> HomeRates.RestedExpPct.toString, "left" -> left)
      case BrewRates.BubbleBoost =>
        t("untimed", "name" -> brew(BrewEffect.Bubble))
      case BrewRates.GiantBoost =>
        t("untilBattleEnd", "name" -> brew(BrewEffect.Giant), "pct" -> BrewRates.GiantHpPct.toString)
      case BrewRates.WolfCallBoost =>
        t("timed", "name" -> brew(BrewEffect.WolfCall), "left" -> left)
      case BrewRates.RotCallBoost =>
        t("timed", "name" -> brew(BrewEffect.RotCall), "left" -> left)
      case n if n.startsWith(BrewRates.SentenceBoost) =>
        val key  = n.drop(BrewRates.SentenceBoost.length)
        val race = pangea.model.monster.Race.withNameOption(key).fold(key)(_.toString)
        t("sentence", "name" -> brew(BrewEffect.Sentence), "race" -> race, "left" -> left)
      case n =>
        val pct = List(b.buff.str, b.buff.vit, b.buff.agi, b.buff.int).max
        statPotion(n) match {
          case Some((name, stat)) =>
            t("stat", "name" -> name, "stat" -> stat.toLowerCase, "pct" -> pct.toString, "left" -> left)
          case None => t("other", "left" -> left)
        }
    }
  }

  /** Чей это баф к характеристике: название зелья Густаво или отвара — и что
   *  он прибавляет. */
  private def statPotion(name: String): Option[(String, String)] =
    BoostStat.all.find(_.boostName == name).map(bs => bs.potion.capitalize -> bs.label)
      .orElse(BrewKind.values.collectFirst(Function.unlift((k: BrewKind) => k.effect match {
        case BrewEffect.Boost(`name`, _, stat) => Some(k.label -> stat)
        case _                                 => None
      })))

  /** Умения, пассивки, камни и наборы — то, что даёт надетое и выжженное. */
  private def skills(hero: Hero): List[SkillEntry] = {
    val eq            = hero.equipment
    val worn          = eq.passiveKinds
    val bodyPassives  = hero.runes.brandedPassives.toSet
    val bodyActives   = hero.runes.brandedActives.toSet
    // В бою клеймо, повторяющее умение вещи, не нужно (см. Hero.activeSkillSlots),
    // а в списке его видно: оно выжжено, и игрок должен это знать.
    val actives = hero.activeSkillSlots.flatMap { s =>
      if (Rune.isBodySlot(s.itemId)) List(SkillEntry.Active(s.itemId, s.skill))
      else {
        val twin = bodyActives.contains(s.skill)
        SkillEntry.Active(s.itemId, s.skill, wornWith(eq, s.skill), twin) ::
          (if (twin) List(SkillEntry.Active(Rune.bodySlotId(s.skill), s.skill, twin = true)) else Nil)
      }
    }
    val passives = hero.passives.kinds.toList.sortBy(_.label).flatMap { k =>
      val onItem = worn.contains(k)
      val onBody = bodyPassives.contains(k)
      val twin   = onItem && onBody
      Option.when(onItem)(SkillEntry.Passive(k, onBody = false, eq.allItems.count(_.passive.contains(k)), twin)).toList ++
        Option.when(onBody)(SkillEntry.Passive(k, onBody = true, twin = twin)).toList
    }
    actives ++ passives ++
      Option.when(eq.weaponGems.nonEmpty || hero.weaponDust.gems.nonEmpty)(SkillEntry.WeaponFx).toList ++
      Option.when(eq.armorGems.nonEmpty)(SkillEntry.ArmorFx).toList ++
      hero.sets.activeBonuses.map { case (set, _) => SkillEntry.Set(set, hero.sets.shownPieces(set)) }
  }

  private def pct(mult: Double): Long = math.round(mult * 100.0)

  /** Что на оружии и как оно бьёт: каждый камень с его оружейной гранью, пыль,
   *  и итог — доля урона по броне и по HP (стихии, их усиление грейдами и
   *  «Дикое пламя» на пороге 4 — тем же счётом, что в бою), и прочие грани. */
  def weaponFx(hero: Hero): String = {
    val g         = hero.gems
    val gems      = hero.equipment.weaponGems.map(gm => s"💎 ${gm.displayName}: ${gm.weaponEffect}")
    val dust      = hero.weaponDust.layers.flatMap(m => m.gem.map(k =>
                      s"✨ ${m.displayName}: ${pangea.model.item.Gem(k, pangea.model.item.Gem.MinGrade).weaponEffect}"))
    val fireBonus = if (g.hasElement(pangea.model.battle.Element.Fire)) hero.sets.fireDamageBonus else 0.0
    val elements  = pangea.model.battle.Element.values.filter(g.hasElement).map(_.emoji)
    val totals = List(
      Option.when(elements.nonEmpty)(s"• Стихии: ${elements.mkString(" ")}"),
      Some(s"• Урон по броне: ${pct(g.armorDamageMult + fireBonus)}%"),
      Some(s"• Урон по HP: ${pct(g.hpDamageMult + fireBonus)}%"),
      Option.when(g.lightningArmorToHpFrac > 0)(s"• Ещё ${pct(g.lightningArmorToHpFrac)}% урона по броне проходит в HP"),
      Option.when(g.vampirismPct > 0)(s"• Вампиризм: ${g.vampirismPct}%"),
      Option.when(g.energyRegenBonusPct > 0)(s"• Восстановление энергии: +${g.energyRegenBonusPct}%"),
      Option.when(g.accuracyBonusPct > 0)(s"• Точность: +${g.accuracyBonusPct}%"),
      Option.when(g.airEvasionAccuracyBonusPct > 0)(s"• Уклонение и точность: +${g.airEvasionAccuracyBonusPct}%"),
      Option.when(g.weaponPoisonPct > 0)(s"• Яд при уроне по HP: ${g.weaponPoisonPct}%"),
      Option.when(hero.weaponDust.damageMult < 1.0)(s"• Перебор пыли: урон ${pct(hero.weaponDust.damageMult)}%")
    ).flatten
    ((gems ++ dust) ++ ("" :: "Итог:" :: totals)).mkString("\n")
  }

  /** Камни во всём снаряжении, кроме оружия: каждый с его броневой гранью и
   *  сумма по всем. */
  def armorFx(hero: Hero): String = {
    val g    = hero.gems
    val gems = hero.equipment.armorGems.map(gm => s"💎 ${gm.displayName}: ${gm.armorEffect}")
    val totals = List(
      Option.when(g.defenceBonusPct > 0)(s"• Защита: +${g.defenceBonusPct}%"),
      Option.when(g.evasionBonusPct > 0)(s"• Уклонение: +${g.evasionBonusPct}%"),
      Option.when(g.maxHpBonusPct > 0)(s"• Макс. HP: +${g.maxHpBonusPct}% и +${g.flatHp}"),
      Option.when(g.energyBonusPct > 0)(s"• Энергия: +${g.energyBonusPct}%"),
      Option.when(g.hpRegenPerMille > 0)(s"• Восстановление HP: ${g.hpRegenPerMille / 10.0}% за раунд"),
      Option.when(g.gearDropBonusPct > 0)(s"• Шанс экипировки: +${g.gearDropBonusPct}%")
    ).flatten
    (if (totals.isEmpty) gems else gems ++ ("" :: "Итог:" :: totals)).mkString("\n")
  }

  /** Сколько надетых вещей несут активное умение — те же, с которых оно и
   *  берётся: оружие в обеих руках и нагрудник. */
  private def wornWith(eq: Equipment, skill: Skill): Int =
    List(eq.weapon, eq.leftHand, eq.chestPlate).count(_.activeSkill.contains(skill))

  final case class SkillsScene(page: Option[Int] = None)
  object SkillsScene {
    implicit val encoder: Encoder[SkillsScene] = deriveEncoder
    implicit val decoder: Decoder[SkillsScene] = deriveDecoder
  }
}
