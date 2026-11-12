package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{Bleed, SoloPveBattle}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.{Item, ItemSet, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Добитые героем в ход мобов. Моб вне пары истёк ранами от героя в свой
  * собственный ход — это добитый героем, как и за удар: пир «Упыря», напор
  * «Преисподней», «Некромант». Засчитали союзнику — он бил последним — не
  * героя. Раньше всё это видело только тех, кто пал за ход самого героя. */
object KillsInMobTurnSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))

  private val lvl = 10L

  private val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
    ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet,
    ItemType.Ring, ItemType.Ring, ItemType.Belt, ItemType.Weapon)

  /** Набор `set` целиком — двенадцать вещей, двенадцатая в руке оружием. */
  private def wearing(set: ItemSet): Equipment =
    slots.zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (eq, (t, i)) =>
      val it = Item(500L + i, "Предмет", 1L, ItemRarity.Blue, t,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, set = Some(set))
      i match {
        case 0  => eq.copy(helmet = it)
        case 1  => eq.copy(shoulderPads = it)
        case 2  => eq.copy(chestPlate = it)
        case 3  => eq.copy(bracelets = it)
        case 4  => eq.copy(gloves = it)
        case 5  => eq.copy(pants = it)
        case 6  => eq.copy(boots = it)
        case 7  => eq.copy(amulet = it)
        case 8  => eq.copy(firstRing = it)
        case 9  => eq.copy(secondRing = it)
        case 10 => eq.copy(belt = it)
        case _  => eq.copy(weapon = it)
      }
    }

  /** Герой ранен и бьёт слишком слабо, чтобы кого-то добить своим ударом. */
  private def hero(set: ItemSet, race: Race = Race.Human, allies: List[Ally] = Nil): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = lvl,
      race       = race,
      fightStats = FightStats(atk = 1L, hp = 100L, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      equipment  = wearing(set),
      squad      = Squad(heroPos = 1, allies = allies))

  private def monster(hp: Long): Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = 20L, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  /** Напротив героя — крепкий моб, сосед на месте 2 при последнем издыхании и
    * истекает кровью: тик в его же ход его и добьёт. `by` — кто из отряда бил
    * его последним (по имени), никто — значит, раны героя. */
  private def battle(h: Hero, by: SoloPveBattle => Option[String] = _ => None): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(List(monster(100000L), monster(1000L)), h, Nil)
    b.copy(group = b.group.copy(others = b.group.others.map(s =>
      s.copy(currentHp = 1L, effects = s.effects.copy(monsterBleed = Some(Bleed(10))), lastAllyHit = by(b)))))
  }

  /** Герой бьёт того, кто напротив, — и раунд идёт до конца. */
  private def fight(h: Hero, b: SoloPveBattle, rolls: Task[Unit] = ZIO.unit): Task[(Hero, SoloPveBattle, String)] =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      _     <- TestRandom.setSeed(42L) *> rolls
      _     <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
                 .action(testUser, aimed("Attack", 1), r)
      after <- dao.getHeroByUserId(userId).someOrFailException
      b1    <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption)).someOrFailException
      said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (after, b1, said)

  override def spec = suite("Добитые героем в ход мобов")(

    test("«Упырь» 12: сосед истёк кровью в свой ход — пир, как за удар") {
      val h = hero(ItemSet.Ghoul)
      for {
        res <- fight(h, battle(h))
        (after, b1, said) = res
      } yield assertTrue(h.sets.feastsOnKill) &&
              // сосед пал от раны, а не от удара: герой бил того, кто напротив
              assertTrue(b1.group.slain.size == 1 && b1.monsterCurrentHp > 0L) &&
              assertTrue(said.contains("🧛 Пир «Упыря»: +")) &&
              assertTrue(after.fightStats.hp > 100L)
    },

    test("«Упырь» 12: соседа последним бил союзник — истёк кровью в счёт союзнику, пира нет") {
      // Союзник стоит на месте 3: напротив пусто, до соседа героя он не тянется.
      val s = AllyKind.Human.stats(lvl)
      val h = hero(ItemSet.Ghoul, allies = List(Ally(AllyKind.Human, 3, s.hp, s.armor, 0L)))
      for {
        res <- fight(h, battle(h, b => b.group.allies.headOption.map(_.name)))
        (_, b1, said) = res
      } yield assertTrue(b1.group.slain.size == 1) &&
              assertTrue(b1.group.allies.headOption.exists(_.kills == 1)) &&
              assertTrue(!said.contains("🧛 Пир «Упыря»: +"))
    },

    test("«Преисподняя» 12 у демона: сосед истёк кровью в свой ход — напор возвращает энергию") {
      val h = hero(ItemSet.Underworld, race = Race.Demon)
      for {
        res <- fight(h, battle(h))
        (_, b1, said) = res
      } yield assertTrue(h.sets.killEnergyPct > 0L) &&
              assertTrue(b1.group.slain.size == 1) &&
              assertTrue(said.contains("Напор"))
    },

    test("«Некромант» 12: сосед истёк кровью в свой ход — восстаёт и встаёт в строй героя") {
      val h = hero(ItemSet.Necromancer)
      for {
        // Все броски проходят — и шанс восстать (5%) тоже.
        res <- fight(h, battle(h), TestRandom.feedInts(99 :: List.fill(80)(1): _*))
        (_, b1, said) = res
      } yield assertTrue(h.sets.riseChancePct > 0L) &&
              assertTrue(b1.group.slain.size == 1) &&
              assertTrue(said.contains("💀 «Некромант»: ")) &&
              assertTrue(b1.group.allies.exists(_.undead.isDefined))
    }
  )
}
