package pangea.model.hero

import pangea.model.item.{ItemSet, SetBonus}
import pangea.model.monster.Race
import pangea.model.trauma.TraumaRoll

/** Типизированный фасад над наборами надетого снаряжения. Как [[HeroPassives]] и
 *  [[HeroGems]], каждая точка применения спрашивает готовый модификатор, а числа
 *  живут на вариантах [[ItemSet]].
 *
 *  Наборы независимы: бонусы каждого открываются по своему счётчику надетых
 *  предметов, поэтому 12 предметов из шести разных наборов дают шесть двойных
 *  бонусов. Снял предмет — счётчик упал, порог закрылся.
 *
 *  `counts` приходит из `Equipment.setCounts` и уже учитывает только 12 сетовых
 *  слотов (без фляги и доп. оружия). `race` — раса героя: от неё зависит
 *  скрытый шаг малого расового набора (см. [[visible]]). */
final case class HeroSets(counts: Map[ItemSet, Int], race: Option[Race] = None) {

  /** Сколько предметов набора надето. */
  def pieces(set: ItemSet): Int = counts.getOrElse(set, 0)

  /** Есть ли у героя этот бонус набора вообще: скрытый шаг малого расового
   *  набора есть только у героя его расы, и чужому не показывается. */
  def visible(set: ItemSet, b: SetBonus): Boolean =
    !b.ownRaceOnly || set.race.exists(r => race.contains(r))

  /** Открытые бонусы набора — все пороги, до которых добрал игрок. */
  def bonuses(set: ItemSet): List[SetBonus] =
    set.bonuses.filter(b => b.pieces <= pieces(set) && visible(set, b))

  /** Все открытые бонусы по всем наборам, в порядке объявления наборов. */
  def activeBonuses: List[(ItemSet, List[SetBonus])] =
    ItemSet.values.toList.map(s => s -> bonuses(s)).filter(_._2.nonEmpty)

  /** Открыт ли конкретный порог набора и действует ли он (у неактивных бонусов
   *  механики пока нет — см. [[SetBonus.active]]). */
  def has(set: ItemSet, pieces: Int): Boolean =
    this.pieces(set) >= pieces && set.bonusAt(pieces).exists(b => b.active && visible(set, b))

  /** Пороги набора, которые есть у героя: у малого расового чужой расе — 3/6/9. */
  def thresholds(set: ItemSet): List[Int] =
    set.bonuses.filter(visible(set, _)).map(_.pieces)

  /** Сколько вещей набора показывать: выше последнего своего порога счёт не
   *  идёт — иначе «12» выдало бы чужой расе скрытый шаг. */
  def shownPieces(set: ItemSet): Int =
    pieces(set).min(thresholds(set).maxOption.getOrElse(0))

  /** Ставка порога, если он открыт и действует; иначе ноль. */
  private def pct(set: ItemSet, pieces: Int, value: Long): Long =
    if (has(set, pieces)) value else 0L

  // ── Готовые модификаторы ────────────────────────────────────────────────────

  /** +% к защите от «Каменного стража» (порог 2). */
  def defenceBonusPct: Long = pct(ItemSet.StoneGuard, 2, ItemSet.StoneGuard.DefencePct)

  /** +% к атаке: «Дикое пламя» (порог 2) и «Пройдоха» (порог 3). */
  def attackBonusPct: Long =
    pct(ItemSet.WildFlame, 2, ItemSet.WildFlame.AttackPct) + pct(ItemSet.Trickster, 3, ItemSet.Trickster.AttackPct)

  /** +% к уклонению: «Упырь» (порог 2), «Мать Глубин» (порог 3) и «Пустынь»
   *  (порог 12, только каджиту). */
  def evasionBonusPct: Long =
    pct(ItemSet.Ghoul, 2, ItemSet.Ghoul.EvasionPct) + pct(ItemSet.DeepMother, 3, ItemSet.DeepMother.EvasionPct) +
      pct(ItemSet.Desert, 12, ItemSet.Desert.RoadEvasionPct)

  /** +% к точности: «Охотник» (порог 2) и «Тихая роща» (порог 3). */
  def accuracyBonusPct: Long =
    pct(ItemSet.Hunter, 2, ItemSet.Hunter.AccuracyPct) + pct(ItemSet.QuietGrove, 3, ItemSet.QuietGrove.AccuracyPct)

  /** +% к броне: «Крыса» (порог 2) и «Подгорный мастер» (порог 3). В отличие от
   *  прочих прибавок поднимает не итоговый боевой стат, а ПОТОЛОК брони
   *  (`Hero.maxArmor`): иначе прибавка срезалась бы при первой же починке. */
  def armorBonusPct: Long =
    pct(ItemSet.Rat, 2, ItemSet.Rat.ArmorPct) + pct(ItemSet.MountainMaster, 3, ItemSet.MountainMaster.ArmorPct)

  /** Плоская прибавка к макс. HP: +300 за каждый набор, добравший до порога 8. */
  def flatHp: Long = ItemSet.values.count(has(_, 8)) * ItemSet.HpFlatBonus

  /** +% к макс. HP: +10% за каждый набор, добравший до порога 8, и «Орда»
   *  (порог 3). */
  def maxHpBonusPct: Long =
    ItemSet.values.count(has(_, 8)) * ItemSet.HpPctBonus + pct(ItemSet.Horde, 3, ItemSet.Horde.HpPct)

  /** Сдвиг макс. энергии в %: «Охотник» (порог 4) её прибавляет, «Некромант»
   *  (порог 4) отнимает — силы уходят на поддержание мёртвых. Складываются, как
   *  и прочие проценты набора. */
  def energyBonusPct: Long =
    (if (has(ItemSet.Hunter, 4)) ItemSet.Hunter.EnergyPct else 0L) -
      (if (has(ItemSet.Necromancer, 4)) ItemSet.Necromancer.EnergyCutPct else 0L)

  /** +% к интеллекту: «Некромант» (порог 2) — единственная двойка, которая
   *  поднимает не боевой стат, а базовый, — «Преисподняя» и «Первопроходец»
   *  (порог 3). */
  def intBonusPct: Long =
    pct(ItemSet.Necromancer, 2, ItemSet.Necromancer.IntPct) + pct(ItemSet.Underworld, 3, ItemSet.Underworld.IntPct) +
      pct(ItemSet.Pioneer, 3, ItemSet.Pioneer.BaseStatsPct)

  /** +% к силе и к телосложению: «Первопроходец» (порог 3). */
  def strBonusPct: Long = pct(ItemSet.Pioneer, 3, ItemSet.Pioneer.BaseStatsPct)
  def vitBonusPct: Long = pct(ItemSet.Pioneer, 3, ItemSet.Pioneer.BaseStatsPct)

  /** +% к ловкости: «Первопроходец» и «Пустынь» (порог 3). */
  def agiBonusPct: Long =
    pct(ItemSet.Pioneer, 3, ItemSet.Pioneer.BaseStatsPct) + pct(ItemSet.Desert, 3, ItemSet.Desert.AgiPct)

  /** Реген в конце раунда, в % от максимума: HP — «Мать Глубин» (порог 9);
   *  броня — она же и «Подгорный мастер» (порог 6). Складывается с пассивками. */
  def hpRegenPct: Long = pct(ItemSet.DeepMother, 9, ItemSet.DeepMother.RegenPct)
  def armorRegenPct: Long =
    pct(ItemSet.DeepMother, 9, ItemSet.DeepMother.RegenPct) +
      pct(ItemSet.MountainMaster, 6, ItemSet.MountainMaster.ArmorRegenPct)

  // ── «Первопроходец» ─────────────────────────────────────────────────────────

  /** +% к атаке и защите за `rounds` прошедших раундов боя (порог 6): по
   *  проценту за раунд, до потолка. */
  def roundBonusPct(rounds: Int): Long =
    pct(ItemSet.Pioneer, 6, (rounds.toLong * ItemSet.Pioneer.RoundPct).max(0L).min(ItemSet.Pioneer.RoundCapPct))

  /** Сколько держится полученная травма (порог 9 — вдвое меньше). */
  def traumaDurationMs: Long =
    TraumaRoll.DurationMs * (100L - pct(ItemSet.Pioneer, 9, ItemSet.Pioneer.TraumaCutPct)) / 100L

  /** Какая доля отдыха после смерти остаётся (порог 9 — вдвое короче). */
  def deathRestMult: Double = (100L - pct(ItemSet.Pioneer, 9, ItemSet.Pioneer.DeathRestCutPct)) / 100.0

  /** «Второе дыхание» (порог 12, только человеку): раз за бой смерть оставляет
   *  1 HP и снимает яд, кровь и пламя. */
  def secondWind: Boolean = has(ItemSet.Pioneer, 12)

  // ── «Орда» ──────────────────────────────────────────────────────────────────

  /** Сколько HP снимет удар, который снял бы `loss` (порог 6): большой удар —
   *  больше доли от `maxHp` — упрямство держит. */
  def stubbornLoss(loss: Long, maxHp: Long): Long =
    if (has(ItemSet.Horde, 6) && loss * 100L > maxHp * ItemSet.Horde.StubbornHitPct)
      loss * (100L - ItemSet.Horde.StubbornCutPct) / 100L
    else loss

  /** Ниже ли HP черты ярости — половины максимума. */
  private def enraged(hp: Long, maxHp: Long): Boolean = hp * 100L < maxHp * ItemSet.Horde.RageBelowPct

  /** +% к атаке от ран: «Ярость» (порог 9) ниже половины HP и «Орочья кровь»
   *  (порог 12) — за каждый полный шаг недостающего HP. */
  def woundAttackPct(hp: Long, maxHp: Long): Long = {
    val missingPct = if (maxHp <= 0L) 0L else (maxHp - hp).max(0L) * 100L / maxHp
    val blood      = (missingPct / ItemSet.Horde.BloodStepPct * ItemSet.Horde.BloodAttackPct).min(ItemSet.Horde.BloodCapPct)
    (if (enraged(hp, maxHp)) pct(ItemSet.Horde, 9, ItemSet.Horde.RageAttackPct) else 0L) +
      pct(ItemSet.Horde, 12, blood)
  }

  /** Вампиризм «Орочьей крови» (порог 12) — пока HP ниже половины. */
  def woundLifestealPct(hp: Long, maxHp: Long): Long =
    if (enraged(hp, maxHp)) pct(ItemSet.Horde, 12, ItemSet.Horde.BloodLifestealPct) else 0L

  // ── «Мать Глубин» ───────────────────────────────────────────────────────────

  /** Шанс (в %), что попавший обычный удар соскользнёт по чешуе (порог 6), и
   *  на сколько % слабее он тогда пройдёт. */
  def slipChancePct: Long = pct(ItemSet.DeepMother, 6, ItemSet.DeepMother.SlipChancePct)
  def slipCutPct: Long    = ItemSet.DeepMother.SlipCutPct

  /** Сколько мурлоков-собратьев выйдет в начале боя против `foes` врагов
   *  (порог 12, только мурлоку): один, а против многих — на одного больше. */
  def kinCount(foes: Int): Int =
    if (!has(ItemSet.DeepMother, 12)) 0
    else ItemSet.DeepMother.KinCount + (if (foes >= ItemSet.DeepMother.KinManyFoes) 1 else 0)

  // ── «Подгорный мастер» ──────────────────────────────────────────────────────

  /** Держит ли герой на ногах (порог 12, только гному): пока у него есть броня
   *  `armor`, его не оглушить, не сбить с ног, не пригвоздить, и удары не
   *  оставляют травм. Оглушения героя в игре пока нет — правило ждёт его. */
  def steadfast(armor: Long): Boolean = has(ItemSet.MountainMaster, 12) && armor > 0L

  // ── «Тихая роща» ────────────────────────────────────────────────────────────

  /** Бьёт ли первый удар боя из засады — без промаха и сильнее (порог 6). */
  def ambushes: Boolean    = has(ItemSet.QuietGrove, 6)
  def ambushDamagePct: Long = ItemSet.QuietGrove.AmbushDamagePct

  /** Шанс (в %), что удар по HP ослабит врага (порог 9). */
  def weakenOnHitChancePct: Long = pct(ItemSet.QuietGrove, 9, ItemSet.QuietGrove.WeakenChancePct)

  /** На сколько мест дальше бьют лук и арбалет и на сколько % слабее их штраф
   *  вблизи (порог 12, только эльфу). */
  def rangedReachBonus: Int     = if (has(ItemSet.QuietGrove, 12)) ItemSet.QuietGrove.RangedReachBonus else 0
  def closePenaltyCutPct: Long  = pct(ItemSet.QuietGrove, 12, ItemSet.QuietGrove.ClosePenaltyCutPct)

  // ── «Пустынь» ───────────────────────────────────────────────────────────────

  /** Прибавка к шансу прока Воздуха (п.п.) и на сколько раундов дольше держится
   *  попутный ветер (порог 6). */
  def airChanceBonusPct: Long = pct(ItemSet.Desert, 6, ItemSet.Desert.AirChanceBonusPct)
  def airTurnsBonus: Int      = if (has(ItemSet.Desert, 6)) ItemSet.Desert.AirTurnsBonus else 0

  /** На сколько % дороже Ришелье покупает и дешевле продаёт (порог 9). */
  def tradePct: Long = pct(ItemSet.Desert, 9, ItemSet.Desert.TradePct)

  /** На сколько % короче поход по карте клада (порог 12, только каджиту). */
  def mapHuntCutPct: Long = pct(ItemSet.Desert, 12, ItemSet.Desert.MapHuntCutPct)

  // ── «Пройдоха» ──────────────────────────────────────────────────────────────

  /** Шанс (в %), что удар по HP выжжет врагу энергию (порог 6), и сколько — в %
   *  от её потолка. */
  def energyBurnOnHitChancePct: Long = pct(ItemSet.Trickster, 6, ItemSet.Trickster.EnergyBurnChancePct)
  def energyBurnPct: Long            = ItemSet.Trickster.EnergyBurnPct

  /** Сколько % серебра сверху с боёв (порог 9). */
  def battleSilverBonusPct: Long = pct(ItemSet.Trickster, 9, ItemSet.Trickster.SilverBonusPct)

  /** Шанс (в %) лишней вещи с каждого добитого героем врага (порог 12, только
   *  гоблину) и на сколько % реже его окружают при бегстве. */
  def extraGearChancePct: Long = pct(ItemSet.Trickster, 12, ItemSet.Trickster.ExtraGearChancePct)
  def surroundCutPct: Long     = pct(ItemSet.Trickster, 12, ItemSet.Trickster.SurroundCutPct)

  // ── «Преисподняя» ───────────────────────────────────────────────────────────

  /** На сколько % сильнее удары и умения по горящему врагу (порог 6). */
  def burningDamageBonusPct: Long = pct(ItemSet.Underworld, 6, ItemSet.Underworld.BurningDamageBonusPct)

  /** На сколько % дешевле активные умения (порог 9). */
  def skillEnergyCutPct: Long = pct(ItemSet.Underworld, 9, ItemSet.Underworld.SkillEnergyCutPct)

  /** Сколько % максимума энергии возвращает каждый добитый героем враг
   *  (порог 12, только демону). */
  def killEnergyPct: Long = pct(ItemSet.Underworld, 12, ItemSet.Underworld.KillEnergyPct)

  /** Прибавка к шансу прока Холода, в п.п. («Подгорный мастер», порог 9). */
  def coldChanceBonusPct: Long = pct(ItemSet.MountainMaster, 9, ItemSet.MountainMaster.ColdChanceBonusPct)

  /** Множитель вклада ловкости в реген энергии за раунд («Охотник», порог 4). */
  def agiEnergyRegenMult: Long =
    if (has(ItemSet.Hunter, 4)) ItemSet.Hunter.AgiRegenMult else 1L

  // ── «Упырь» ─────────────────────────────────────────────────────────────────

  /** % нанесённого по HP урона, возвращаемого в лечение (порог 4) — как и
   *  вампиризм черепа: удар, поглощённый бронёй, крови не даёт. */
  def lifestealPct: Long = if (has(ItemSet.Ghoul, 4)) ItemSet.Ghoul.LifestealPct else 0L

  /** Шанс (в %), что обычная атака, прошедшая в HP, вызовет кровотечение (порог 6). */
  def bleedOnHitChancePct: Long = if (has(ItemSet.Ghoul, 6)) ItemSet.Ghoul.BleedChancePct else 0L

  /** Сила накладываемого набором кровотечения, в % макс. HP цели. */
  def bleedPct: Int = ItemSet.Ghoul.BleedPct

  /** Лечит ли героя урон от кровотечения врага (порог 10). */
  def healsFromBleed: Boolean = has(ItemSet.Ghoul, 10)

  /** Всегда ли активные умения накладывают кровотечение (порог 12). */
  def skillsAlwaysBleed: Boolean = has(ItemSet.Ghoul, 12)

  /** Восстановление при убийстве врага — «жуткий пир» (порог 12). */
  def feastsOnKill: Boolean = has(ItemSet.Ghoul, 12)
  def feastHpPct: Long      = ItemSet.Ghoul.KillHpRestorePct
  def feastArmorPct: Long   = ItemSet.Ghoul.KillArmorRestorePct

  // ── «Каменный страж» ────────────────────────────────────────────────────────

  /** Множитель ЛЮБОГО стихийного урона по герою (порог 4). Стихийным считается
   *  весь урон элементаля: он и есть чистая стихия — и удар, и всплеск, и смерч,
   *  и шипы, и горение. */
  def elementalDamageTakenMult: Double =
    if (has(ItemSet.StoneGuard, 4)) (100L - ItemSet.StoneGuard.ElementalTakenCutPct) / 100.0 else 1.0

  /** Множитель урона, снимаемого с БРОНИ героя (порог 6): броня тает медленнее.
   *  Урон по HP при этом не меняется — режется именно расход брони. */
  def armorDamageTakenMult: Double =
    if (has(ItemSet.StoneGuard, 6)) (100L - ItemSet.StoneGuard.ArmorDamageCutPct) / 100.0 else 1.0

  /** Сколько брони реально уйдёт, когда она поглотила `absorbed` урона. Броня
   *  прикрывает HP на всю поглощённую величину — порог 6 удешевляет только её
   *  собственный расход. */
  def armorSpent(absorbed: Long): Long =
    if (absorbed <= 0L) absorbed else (absorbed * armorDamageTakenMult).toLong.max(1L)

  /** На сколько п.п. снижен шанс поджечь героя (порог 10). */
  def igniteResistPct: Long =
    if (has(ItemSet.StoneGuard, 10)) ItemSet.StoneGuard.IgniteResistPct else 0L

  /** Спасает ли броня на низком HP (порог 12) и её ставки. */
  def rescuesOnLowHp: Boolean  = has(ItemSet.StoneGuard, 12)
  def lowHpThresholdPct: Long  = ItemSet.StoneGuard.LowHpThresholdPct
  def rescueArmorPct: Long     = ItemSet.StoneGuard.ArmorRestorePct

  // ── «Дикое пламя» ───────────────────────────────────────────────────────────

  /** Прибавка к граням урона огнём, в долях (порог 4). Применяется только если в
   *  оружии есть Огонь — см. `BattleState.splitElementalDamage`. */
  def fireDamageBonus: Double =
    if (has(ItemSet.WildFlame, 4)) ItemSet.WildFlame.FireDamageBonusPct / 100.0 else 0.0

  /** Во сколько раз быстрее растёт горение за раунд (порог 6). */
  def burnGrowthMult: Long =
    if (has(ItemSet.WildFlame, 6)) ItemSet.WildFlame.BurnGrowthMult else 1L

  /** Прибавка к шансу прока Огня, в п.п.: «Дикое пламя» (порог 10) и
   *  «Преисподняя» (порог 6). */
  def igniteChanceBonusPct: Long =
    pct(ItemSet.WildFlame, 10, ItemSet.WildFlame.IgniteChanceBonusPct) +
      pct(ItemSet.Underworld, 6, ItemSet.Underworld.IgniteChanceBonusPct)

  /** Поджигают ли активные умения врага всегда, и режет ли горение его защиту
   *  на столько же процентов, сколько горит (порог 12). */
  def skillsAlwaysIgnite: Boolean = has(ItemSet.WildFlame, 12)

  // ── «Охотник» ───────────────────────────────────────────────────────────────

  /** Шанс (в %) повторить атаку после промаха, не чаще раза за раунд (порог 6). */
  def repeatOnMissChancePct: Long =
    if (has(ItemSet.Hunter, 6)) ItemSet.Hunter.RepeatChancePct else 0L

  /** Шанс (в %) заблокировать урон от атакующего умения противника
   *  («Охотник», порог 10). */
  def blockSkillChancePct: Long =
    if (has(ItemSet.Hunter, 10)) ItemSet.Hunter.BlockSkillChancePct else 0L

  /** Шанс (в %) отвести ОБЫЧНЫЙ удар противника целиком. Пока его не
   *  даёт ни один набор — механика стоит готовой под будущие бонусы
   *  (см. `BattleState.mobStrike`). Ноль — броска нет вовсе. */
  def blockAttackChancePct: Long = 0L

  /** Прибавка к шансу крита всех атакующих умений, в п.п. (порог 12).
   *  У «Удара в слабое место» свой шанс от интеллекта — набор прибавляется
   *  к нему, а не умножает урон второй раз: четверного удара больше нет. */
  def skillCritBonusPct: Long =
    if (has(ItemSet.Hunter, 12)) ItemSet.Hunter.SkillCritBonusPct else 0L

  // ── «Крыса» ─────────────────────────────────────────────────────────────────

  // ── «Некромант» ─────────────────────────────────────────────────────────────

  /** Насколько сильнее ожившие союзники — поднятые с алтаря и восставшие
   *  (порог 4). Ноль — как были. На крысу из набора «Крыса» не действует: она
   *  зверь, а не нежить. */
  def undeadAllyBoostPct: Long =
    if (has(ItemSet.Necromancer, 4)) ItemSet.Necromancer.UndeadBoostPct else 0L

  /** Во сколько раз дольше держится поднятый с алтаря (порог 6). Срок ставится
   *  в момент поднятия: снятый потом набор уже поднятого не торопит. */
  def undeadLastsMult: Long =
    if (has(ItemSet.Necromancer, 6)) ItemSet.Necromancer.UndeadLastsMult else 1L

  /** Выходит ли поднятый в бой целым, сколько бы ни был побит раньше (порог 6). */
  def undeadEntersFresh: Boolean = has(ItemSet.Necromancer, 6)

  /** Сам герой — нежить (порог 10): кровь и яд его не берут, огонь берёт сильнее. */
  def heroIsUndead: Boolean = has(ItemSet.Necromancer, 10)

  /** Множитель урона огнём по герою-нежити. */
  def fireTakenMult: Double =
    if (heroIsUndead) (100L + ItemSet.Necromancer.UndeadFireTakenPct) / 100.0 else 1.0

  /** Во сколько раз быстрее растёт горение на герое-нежити. */
  def heroBurnGrowthMult: Long =
    if (heroIsUndead) ItemSet.Necromancer.UndeadBurnGrowthMult else 1L

  /** Какие ауры держит герой — эффекты, накрывающие ВСЁ поле боя
   *  (см. [[pangea.model.battle.Aura]]). Пока одна: миазмы тьмы с порога 10.
   *  Включить или свернуть их — выбор игрока, кнопкой в бою. */
  def auras: Set[pangea.model.battle.Aura] =
    if (has(ItemSet.Necromancer, 10)) Set(pangea.model.battle.Aura.Miasma) else Set.empty

  /** Держит ли герой хоть одну ауру: по этому на экране боя и появляется кнопка. */
  def spreadsMiasma: Boolean = auras.nonEmpty

  /** Шанс (в %), что добитый героем враг восстанет нежитью (порог 12). */
  def riseChancePct: Long =
    if (has(ItemSet.Necromancer, 12)) ItemSet.Necromancer.RiseChancePct else 0L

  /** Шанс (в %), что обычная атака, прошедшая в HP, отравит врага (порог 4). */
  def poisonOnHitChancePct: Long =
    if (has(ItemSet.Rat, 4)) ItemSet.Rat.PoisonChancePct else 0L

  /** Сила накладываемого набором яда, в % макс. HP цели. */
  def poisonPct: Int = ItemSet.Rat.PoisonPct

  /** Шанс (в %), что в начале раунда к герою выскочит крыса (порог 6). */
  def ratSummonChancePct: Long =
    if (has(ItemSet.Rat, 6)) ItemSet.Rat.SummonChancePct else 0L

  /** Множитель яда и кровотечения НА ГЕРОЕ (порог 10): они снимают вдвое меньше. */
  def heroDotTakenMult: Double =
    if (has(ItemSet.Rat, 10)) (100L - ItemSet.Rat.DotTakenCutPct) / 100.0 else 1.0

  /** На сколько п.п. за раунд слабеет кровотечение на герое (порог 10). Ноль —
   *  рана не затягивается сама, как было до набора. */
  def heroBleedDecayPerRound: Int =
    if (has(ItemSet.Rat, 10)) ItemSet.Rat.BleedDecayPerRound else 0

  /** Приходят ли на зов чумные крысы вместо обычных (порог 12). */
  def summonsPlagueRats: Boolean = has(ItemSet.Rat, 12)

  /** Даётся ли крыса сразу, если в начале боя при герое нет ни одной (порог 12). */
  def startsWithRat: Boolean = has(ItemSet.Rat, 12)
}

object HeroSets {
  val empty: HeroSets = HeroSets(Map.empty)
}
