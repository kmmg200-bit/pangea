package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.{ItemGenerator, MaterialGenerator}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{BattleAlly, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, MaterialKind, QuestItemKind, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.{AllyKind, UndeadForm}
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Пропавшая дружина» — всё, что нужно сюжету в разных сценах: отметить
  * пятисотого убитого и выдать фонарь старшины, узнать, на каком шаге
  * задание, собрать гоблинов у завала, выковать шлем, раздать дары, закрыть
  * задание. Сам ход — [[NpcQuest.LostSquad]] в `npc_quests`; сцены — штольня
  * под Серебряной жилой ([[pangea.service.state.states.lostsquad.SquadMineState]]),
  * Брамбл за столом наёмников и Гуннар Отвес на Стройке. У героя-гнома свои
  * реплики: `key` подставляет вариант `<ключ>Kin`. */
object LostSquadQuest {

  /** Ключ сюжетного боя — подъёма дружины наверх сквозь гоблинов. Как и налёт
    * на мурлоков, награждается как обычный бой: опыт и добыча с каждого
    * павшего по уровню героя. */
  val EscortStory: String = "squadEscort"

  /** Сколько гномов ждёт за завалом. */
  val Survivors: Int = 7

  /** Чем гномы благодарят: каждый — куском одного из двух, поровну. */
  val GiftKinds: List[MaterialKind] = List(MaterialKind.EverburningIron, MaterialKind.MagicStone)

  /** Шлем старшины с фонарём: пурпурный, ровно уровня героя, всегда с точностью. */
  val HelmetName: String       = "Шлем «Двенадцать фонарей»"
  val HelmetRarity: ItemRarity = ItemRarity.Violet

  /** Сколько серебра Гуннар даёт за груз — и кусок железа сверху. */
  val CrateSilver: Long        = 10000L
  val CrateBonus: MaterialKind = MaterialKind.EverburningIron

  /** Гоблины у завала: строй, очередь за ним и последним — их вожак. */
  val EscortFront: Int                  = 6
  val EscortFrontRarities: List[Rarity] = List(Rarity.Uncommon, Rarity.Rare)
  val EscortQueue: Int                  = 6
  val EscortQueueRarities: List[Rarity] = List(Rarity.Rare, Rarity.Mythical)

  /** Вожак мародёров — именной гоблин, собиратель чужого добра. */
  val LeaderName: String = "Хруст Собиратель"

  /** Трое из семерых, кто ещё держит кирку: идут с героем на бой у завала. */
  val EscortGnomes: List[String] = List("Старшина Ульфрик", "Ингрид Крепь", "Торвальд Кайло")

  /** С какой долей HP и брони дружинники входят в бой — полгода в темноте. */
  val EscortStartPct: Long = 75L

  /** Герой той же крови — дружина и Брамбл говорят с ним иначе. */
  def isKin(hero: Hero): Boolean = hero.race == Race.Gnome

  /** Ключ текста с поправкой на кровь героя: гному — `<key>Kin`. */
  def key(hero: Hero, base: String): String = if (isKin(hero)) base + "Kin" else base

  // ── Ход задания ─────────────────────────────────────────────────────────────

  /** Фонарь на руках, штольня ещё не найдена. */
  def lanternOnHands(q: NpcQuests): Boolean = q.onStep(NpcQuest.LostSquad, 1)

  /** Штольня найдена, дружина ждёт решения. */
  def mineFound(q: NpcQuests): Boolean = q.onStep(NpcQuest.LostSquad, 2)

  /** Дружина выведена, дары ещё не приняты. */
  def giftsWait(q: NpcQuests): Boolean = q.onStep(NpcQuest.LostSquad, 3)

  /** Груз забран — нести его Гуннару на Стройку. */
  def crateOnHands(q: NpcQuests): Boolean = q.onStep(NpcQuest.LostSquad, 4)

  /** Под серебряной жилой откроется штольня: фонарь на руках, дружина всё ещё
    * за завалом — или уже наверху, но дары не приняты. Ушёл от неё (хоть по
    * /home) — она ждёт у выхода, и дары не пропадают. */
  def mineOpens(q: NpcQuests): Boolean = lanternOnHands(q) || mineFound(q) || giftsWait(q)

  /** Брамбл уже рассказал, что знал. */
  def bramblTold(q: NpcQuests): Boolean = q.of(NpcQuest.LostSquad).counter > 0L

  /** Дружина брошена — Брамбл этого не забудет. */
  def squadAbandoned(q: NpcQuests): Boolean = q.of(NpcQuest.LostSquad).bonus

  /** Дружина выведена — Брамбл в долгу. */
  def bramblFriend(q: NpcQuests): Boolean = q.isDone(NpcQuest.LostSquad) && !squadAbandoned(q)

  /** Что прораб Гуннар Отвес говорит о дружине — ключ текста по ходу задания.
    * Ящик на руках — замечает его; дружина выведена — благодарит (о том, что
    * вывели, ему сказали сразу, ещё до даров); задание идёт — ждёт своих; груз
    * продан — молчит. Задание не начиналось — None: о дружине ни слова. */
  def foremanLine(q: NpcQuests): Option[String] =
    if (crateOnHands(q)) Some("lostSquad.foreman.crateSeen")
    else if (giftsWait(q) || bramblFriend(q)) Some("lostSquad.foreman.grateful")
    else if (q.isTaken(NpcQuest.LostSquad)) Some("lostSquad.foreman.waiting")
    else if (q.isDone(NpcQuest.LostSquad)) Some("lostSquad.foreman.cold")
    else None

  /** Пока задание идёт, Брамбл всегда за столом — даже если только что ушёл
    * отдыхать, — и идёт даром. Бросил дружину — за даром больше не пойдёт. */
  def bramblCalled(q: NpcQuests): Boolean = q.isTaken(NpcQuest.LostSquad) && !squadAbandoned(q)

  /** Сколько шнапса Брамбл берёт за найм: вывел дружину — всегда одну
    * бутылку, бросил — вдвое против обычного. */
  def bramblBrews(q: NpcQuests, lvl: Long): Long =
    if (bramblFriend(q)) 1L
    else if (squadAbandoned(q)) AllyKind.brewsFor(lvl) * 2L
    else AllyKind.brewsFor(lvl)

  /** Пятисотый убитый — или любой следующий, если герой перевалил за пятьсот
    * раньше: фонарь в сумку, задание на первый шаг, если оно ещё не начато.
    * Брамбл в отряде узнаёт фонарь сразу — его рассказ уже позади. `None` —
    * ничего не началось; иначе — в отряде ли Брамбл. */
  def start(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository,
            userId: UserId, hero: Hero): Task[Option[Boolean]] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      quests <- NpcQuestLog.load(heroDao, userId)
      res    <- if (quests.isTaken(NpcQuest.LostSquad) || quests.isDone(NpcQuest.LostSquad)) ZIO.none
                else {
                  val brambl = hero.squad.has(AllyKind.Gnome)
                  QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.ForemanLantern) *>
                    NpcQuestLog.save(heroDao, userId, quests.take(NpcQuest.LostSquad, now)
                      .update(NpcQuest.LostSquad)(_.copy(counter = if (brambl) 1L else 0L))).as(Some(brambl))
                }
    } yield res

  /** Брамбл рассказал, что знал. */
  def told(heroDao: HeroDao, userId: UserId): Task[Unit] =
    NpcQuestLog.modify(heroDao, userId)(_.update(NpcQuest.LostSquad)(_.copy(counter = 1L))).unit

  /** Кирка провалилась в штольню: дружина найдена. */
  def found(heroDao: HeroDao, userId: UserId): Task[Unit] =
    NpcQuestLog.modify(heroDao, userId)(_.update(NpcQuest.LostSquad)(_.copy(step = 2))).unit

  /** Гоблины перебиты, дружина наверху — дары ещё впереди. */
  def rescued(heroDao: HeroDao, userId: UserId): Task[Unit] =
    NpcQuestLog.modify(heroDao, userId)(_.update(NpcQuest.LostSquad)(_.copy(step = 3))).unit

  /** Груз забран, дружина брошена: ящик в сумку, Брамбл теперь берёт вдвое. */
  def abandon(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository,
              userId: UserId, hero: Hero): Task[Unit] =
    QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.SquadCrate) *>
      NpcQuestLog.modify(heroDao, userId)(_.update(NpcQuest.LostSquad)(_.copy(step = 4, bonus = true))).unit

  /** Закрыть задание: фонарь и ящик уходят из сумки, задание — в выполненные.
    * Возвращает строку для игрока. */
  def finish(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent,
             userId: UserId, hero: Hero): Task[String] =
    QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.ForemanLantern, QuestItemKind.SquadCrate)) *>
      NpcQuestLog.modify(heroDao, userId)(_.finish(NpcQuest.LostSquad)).as(content.text("lostSquad.questDone"))

  // ── Дары дружины ────────────────────────────────────────────────────────────

  /** Шлем старшины: пурпурный, ровно уровня героя. Точность в нём есть всегда:
    * не выпала — встаёт по ставке лёгкой вещи (половина ставки редкости на
    * уровень, см. ItemGenerator). */
  def helmet(hero: Hero, rng: Rng): Item = {
    val raw      = ItemGenerator.createItemOfType(ItemType.Helmet, hero.lvl, HelmetRarity, rng)._1
    val accuracy = if (raw.accuracy > 0L) raw.accuracy else (HelmetRarity.factorR * 0.5 * raw.lvl).toLong.max(1L)
    raw.copy(accuracy = accuracy).withName(s"${HelmetRarity.emoji} $HelmetName")
  }

  /** По куску на каждого выведенного: вечно огненное железо или магический
    * камень, поровну. */
  def gifts(rng: Rng): (List[Item], Rng) =
    (1 to Survivors).foldLeft((List.empty[Item], rng)) { case ((acc, r), _) =>
      val (kind, next) = r.pick(GiftKinds)
      (acc :+ MaterialGenerator.item(kind), next)
    }

  // ── Бой у завала ────────────────────────────────────────────────────────────

  /** Дружинники на бой у завала: гномы-предводители уровня героя, на трёх
    * четвертях HP и брони, с полной энергией. На помощь не зовут — у союзника
    * свои умения ([[pangea.model.squad.AllySkill]]); места им раздаёт бой.
    * После боя уходят со своими наверх и в отряд не встают. */
  def escortAllies(lvl: Long): List[BattleAlly] =
    EscortGnomes.map { name =>
      val stats = MonsterGenerator.generateOfRaceAndRarity(lvl.toInt, Race.Gnome, Rarity.Mythical).fightStats
      BattleAlly(AllyKind.SquadGnome, position = 0,
        hp = stats.hp * EscortStartPct / 100L, armor = stats.armor * EscortStartPct / 100L, energy = stats.energy,
        stats = stats, lvl = lvl, undead = Some(UndeadForm(name, lvl, stats)), home = 0)
    }

  /** Поставить дружинников в строй — на ближайшие к герою свободные места. Мест
    * не хватило — встают, кто влез. */
  def withEscort(battle: SoloPveBattle, lvl: Long): SoloPveBattle =
    escortAllies(lvl).foldLeft(battle) { (b, ally) =>
      b.group.nearestFreeAllyPlace.fold(b)(pos =>
        b.copy(group = b.group.copy(allies = b.group.allies :+ ally.copy(position = pos))))
    }

  // ── Гоблины у завала ────────────────────────────────────────────────────────

  /** Мародёры уровня героя: строй и очередь за ним, последним — вожак. */
  def escortMonsters(lvl: Int, rng: Rng): (List[Monster], List[Monster], Rng) = {
    def roll(n: Int, pool: List[Rarity], r: Rng): (List[Monster], Rng) =
      (1 to n).foldLeft((List.empty[Monster], r)) { case ((acc, rr), _) =>
        val (rarity, next) = rr.pick(pool)
        (acc :+ MonsterGenerator.generateOfRaceAndRarity(lvl, Race.Goblin, rarity), next)
      }
    val (front, r1) = roll(EscortFront, EscortFrontRarities, rng)
    val (queue, r2) = roll(EscortQueue, EscortQueueRarities, r1)
    val leader      = MonsterGenerator.generateOfRaceAndRarity(lvl, Race.Goblin, Rarity.Legendary, Some(LeaderName))
    (front, queue :+ leader, r2)
  }
}
