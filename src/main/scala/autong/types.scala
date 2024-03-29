package autong

import autong.AutoNG.Started
import japgolly.scalajs.react._
import zio.prelude.{Equal, EqualOps}
import zio.{Dequeue, UManaged}

import scala.scalajs.js
import scala.scalajs.js.UndefOr

object AutoNG {
  type Started = Boolean

  implicit val r: Reusability[AutoNG] = Reusability.byRef
}

trait AutoNG {
  def start: RTask[Unit]
  def stop: RTask[Unit]

  def reconfigure(options: js.UndefOr[Options]): RTask[Unit]

  def setOptions(options: Options): RTask[Unit]

  def buyMachines: (BuildMachinesOpts) => RTask[Unit]

  def sendNotification(notif: String): RTask[Unit]

  def subscribeToStarted: UManaged[Dequeue[Started]]
  def subscribeToOptions: UManaged[Dequeue[RequiredOptions]]
  def subscribeToNotifs: UManaged[Dequeue[String]]
}

trait RunningState extends js.Object {
  def started: Boolean
}

object RunningState {

  def apply(started0: Boolean = true): RunningState = new RunningState {
    val started: Boolean = started0
  }

  implicit class RichRunningState(val rs: RunningState) extends AnyVal {
    def copy(started: Boolean = rs.started): RunningState = apply(started0 = started)
  }

  final val default = apply()
}

trait Options extends js.Object {
  def autoDyson: js.UndefOr[Boolean]                  = js.undefined
  def ringCount: js.UndefOr[Int]                      = js.undefined
  def swarmCount: js.UndefOr[Int]                     = js.undefined
  def autoBuySphere: js.UndefOr[Boolean]              = js.undefined
  def buySwarmsAfterSphere: js.UndefOr[Boolean]       = js.undefined
  def taskInterval: js.UndefOr[Int]                   = js.undefined
  def autoEmc: js.UndefOr[Boolean]                    = js.undefined
  def emcOnlyMeteorite: js.UndefOr[Boolean]           = js.undefined
  def emcOnlyWhenFull: js.UndefOr[Boolean]            = js.undefined
  def storageEnabled: js.UndefOr[Boolean]             = js.undefined
  def onlyUpgradeStorageWhenFull: js.UndefOr[Boolean] = js.undefined
  def upgradeStorageForEMC: js.UndefOr[Boolean]       = js.undefined
  def autoScienceEnabled: js.UndefOr[Boolean]         = js.undefined
  def autoTechsEnabled: js.UndefOr[Boolean]           = js.undefined
  def autoSciTechInterval: js.UndefOr[Int]            = js.undefined
  def buyAntimatter: js.UndefOr[Boolean]              = js.undefined
  def buyCommunications: js.UndefOr[Boolean]          = js.undefined
  def buyMilitary: js.UndefOr[Boolean]                = js.undefined
  def buySpaceship: js.UndefOr[Boolean]               = js.undefined
  def buyNanoswarm: js.UndefOr[Boolean]               = js.undefined
  def buyFreeItems: js.UndefOr[Boolean]               = js.undefined
  def bulkBuyMachines: js.UndefOr[Boolean]            = js.undefined
  def bulkBuyOnlyWhenRich: js.UndefOr[Boolean]        = js.undefined
}

object Options {

  implicit val eq: Equal[Options] = Equal.make { (a, b) =>
    a.autoDyson === b.autoDyson &&
    a.ringCount === b.ringCount &&
    a.swarmCount === b.swarmCount &&
    a.autoBuySphere === b.autoBuySphere &&
    a.buySwarmsAfterSphere === b.buySwarmsAfterSphere &&
    a.taskInterval === b.taskInterval &&
    a.autoEmc === b.autoEmc &&
    a.emcOnlyMeteorite === b.emcOnlyMeteorite &&
    a.emcOnlyWhenFull === b.emcOnlyWhenFull &&
    a.storageEnabled === b.storageEnabled &&
    a.onlyUpgradeStorageWhenFull === b.onlyUpgradeStorageWhenFull &&
    a.upgradeStorageForEMC === b.upgradeStorageForEMC &&
    a.autoScienceEnabled === b.autoScienceEnabled &&
    a.autoTechsEnabled === b.autoTechsEnabled &&
    a.autoSciTechInterval === b.autoSciTechInterval &&
    a.buyAntimatter === b.buyAntimatter &&
    a.buyCommunications === b.buyCommunications &&
    a.buyMilitary === b.buyMilitary &&
    a.buySpaceship === b.buySpaceship &&
    a.buyNanoswarm == b.buyNanoswarm &&
    a.buyFreeItems === b.buyFreeItems &&
    a.bulkBuyMachines === b.bulkBuyMachines &&
    a.bulkBuyOnlyWhenRich === b.bulkBuyOnlyWhenRich
  }

  def apply(
      autoDyson0: js.UndefOr[Boolean] = js.undefined,
      ringCount0: js.UndefOr[Int] = js.undefined,
      swarmCount0: js.UndefOr[Int] = js.undefined,
      autoBuySphere0: js.UndefOr[Boolean] = js.undefined,
      buySwarmsAfterSphere0: js.UndefOr[Boolean] = js.undefined,
      taskInterval0: js.UndefOr[Int] = js.undefined,
      autoEmc0: js.UndefOr[Boolean] = js.undefined,
      emcOnlyMeteorite0: js.UndefOr[Boolean] = js.undefined,
      emcOnlyWhenFull0: js.UndefOr[Boolean] = js.undefined,
      storageEnabled0: js.UndefOr[Boolean] = js.undefined,
      onlyUpgradeStorageWhenFull0: js.UndefOr[Boolean] = js.undefined,
      upgradeStorageForEMC0: js.UndefOr[Boolean] = js.undefined,
      autoScienceEnabled0: js.UndefOr[Boolean] = js.undefined,
      autoTechsEnabled0: js.UndefOr[Boolean] = js.undefined,
      autoSciTechInterval0: js.UndefOr[Int] = js.undefined,
      buyAntimatter0: js.UndefOr[Boolean] = js.undefined,
      buyCommunications0: js.UndefOr[Boolean] = js.undefined,
      buyMilitary0: js.UndefOr[Boolean] = js.undefined,
      buySpaceship0: js.UndefOr[Boolean] = js.undefined,
      buyNanoswarm0: js.UndefOr[Boolean] = js.undefined,
      buyFreeItems0: js.UndefOr[Boolean] = js.undefined,
      bulkBuyMachines0: js.UndefOr[Boolean] = js.undefined,
      bulkBuyOnlyWhenRich0: js.UndefOr[Boolean] = js.undefined,
  ): Options = new Options {
    override val autoDyson: js.UndefOr[Boolean]                  = autoDyson0
    override val ringCount: js.UndefOr[Int]                      = ringCount0
    override val swarmCount: js.UndefOr[Int]                     = swarmCount0
    override val autoBuySphere: js.UndefOr[Boolean]              = autoBuySphere0
    override val buySwarmsAfterSphere: js.UndefOr[Boolean]       = buySwarmsAfterSphere0
    override val taskInterval: js.UndefOr[Int]                   = taskInterval0
    override val autoEmc: js.UndefOr[Boolean]                    = autoEmc0
    override val emcOnlyMeteorite: js.UndefOr[Boolean]           = emcOnlyMeteorite0
    override val emcOnlyWhenFull: js.UndefOr[Boolean]            = emcOnlyWhenFull0
    override val storageEnabled: js.UndefOr[Boolean]             = storageEnabled0
    override val onlyUpgradeStorageWhenFull: js.UndefOr[Boolean] = onlyUpgradeStorageWhenFull0
    override val upgradeStorageForEMC: js.UndefOr[Boolean]       = upgradeStorageForEMC0
    override val autoScienceEnabled: js.UndefOr[Boolean]         = autoScienceEnabled0
    override val autoTechsEnabled: js.UndefOr[Boolean]           = autoTechsEnabled0
    override val autoSciTechInterval: js.UndefOr[Int]            = autoSciTechInterval0
    override val buyAntimatter: js.UndefOr[Boolean]              = buyAntimatter0
    override val buyCommunications: js.UndefOr[Boolean]          = buyCommunications0
    override val buyMilitary: js.UndefOr[Boolean]                = buyMilitary0
    override val buySpaceship: js.UndefOr[Boolean]               = buySpaceship0
    override val buyNanoswarm: js.UndefOr[Boolean]               = buyNanoswarm0
    override val buyFreeItems: js.UndefOr[Boolean]               = buyFreeItems0
    override val bulkBuyMachines: js.UndefOr[Boolean]            = bulkBuyMachines0
    override val bulkBuyOnlyWhenRich: js.UndefOr[Boolean]        = bulkBuyOnlyWhenRich0
  }

  implicit class RichOptions(val o: Options) extends AnyVal {

    def copy(
        autoDyson: js.UndefOr[Boolean] = o.autoDyson,
        ringCount: js.UndefOr[Int] = o.ringCount,
        swarmCount: js.UndefOr[Int] = o.swarmCount,
        autoBuySphere: js.UndefOr[Boolean] = o.autoBuySphere,
        buySwarmsAfterSphere: js.UndefOr[Boolean] = o.buySwarmsAfterSphere,
        taskInterval: js.UndefOr[Int] = o.taskInterval,
        autoEmc: js.UndefOr[Boolean] = o.autoEmc,
        emcOnlyMeteorite: js.UndefOr[Boolean] = o.emcOnlyMeteorite,
        emcOnlyWhenFull: js.UndefOr[Boolean] = o.emcOnlyWhenFull,
        storageEnabled: js.UndefOr[Boolean] = o.storageEnabled,
        onlyUpgradeStorageWhenFull: js.UndefOr[Boolean] = o.onlyUpgradeStorageWhenFull,
        upgradeStorageForEMC: js.UndefOr[Boolean] = o.upgradeStorageForEMC,
        autoScienceEnabled: js.UndefOr[Boolean] = o.autoScienceEnabled,
        autoTechsEnabled: js.UndefOr[Boolean] = o.autoTechsEnabled,
        autoSciTechInterval: js.UndefOr[Int] = o.autoSciTechInterval,
        buyAntimatter: js.UndefOr[Boolean] = o.buyAntimatter,
        buyCommunications: js.UndefOr[Boolean] = o.buyCommunications,
        buyMilitary: js.UndefOr[Boolean] = o.buyMilitary,
        buySpaceship: js.UndefOr[Boolean] = o.buySpaceship,
        buyNanoswarm: js.UndefOr[Boolean] = o.buyNanoswarm,
        buyFreeItems: js.UndefOr[Boolean] = o.buyFreeItems,
        bulkBuyMachines: js.UndefOr[Boolean] = o.bulkBuyMachines,
        bulkBuyOnlyWhenRich: js.UndefOr[Boolean] = o.bulkBuyOnlyWhenRich,
    ): Options = Options(
      autoDyson,
      ringCount,
      swarmCount,
      autoBuySphere,
      buySwarmsAfterSphere,
      taskInterval,
      autoEmc,
      emcOnlyMeteorite,
      emcOnlyWhenFull,
      storageEnabled,
      onlyUpgradeStorageWhenFull,
      upgradeStorageForEMC,
      autoScienceEnabled,
      autoTechsEnabled,
      autoSciTechInterval,
      buyAntimatter,
      buyCommunications,
      buyMilitary,
      buySpaceship,
      buyNanoswarm,
      buyFreeItems,
      bulkBuyMachines,
      bulkBuyOnlyWhenRich,
    )

  }

  final val default =
    RequiredOptions(
      autoDyson = true,
      ringCount = 5,
      swarmCount = 6,
      autoBuySphere = true,
      buySwarmsAfterSphere = true,
      taskInterval = 500,
      autoEmc = true,
      emcOnlyMeteorite = false,
      emcOnlyWhenFull = true,
      storageEnabled = true,
      onlyUpgradeStorageWhenFull = true,
      upgradeStorageForEMC = true,
      autoScienceEnabled = true,
      autoTechsEnabled = true,
      autoSciTechInterval = 60000,
      buyAntimatter = false,
      buyCommunications = false,
      buyMilitary = false,
      buySpaceship = false,
      buyNanoswarm = false,
      buyFreeItems = true,
      bulkBuyMachines = true,
      bulkBuyOnlyWhenRich = true,
    )

  def setDefaults(opts: js.UndefOr[Options]): RequiredOptions =
    opts.fold(default)(o =>
      RequiredOptions(
        o.autoDyson.getOrElse(default.autoDyson),
        o.ringCount.getOrElse(default.ringCount),
        o.swarmCount.getOrElse(default.swarmCount),
        o.autoBuySphere.getOrElse(default.autoBuySphere),
        o.buySwarmsAfterSphere.getOrElse(default.buySwarmsAfterSphere),
        o.taskInterval.getOrElse(default.taskInterval),
        o.autoEmc.getOrElse(default.autoEmc),
        o.emcOnlyMeteorite.getOrElse(default.emcOnlyMeteorite),
        o.emcOnlyWhenFull.getOrElse(default.emcOnlyWhenFull),
        o.storageEnabled.getOrElse(default.storageEnabled),
        o.onlyUpgradeStorageWhenFull.getOrElse(default.onlyUpgradeStorageWhenFull),
        o.upgradeStorageForEMC.getOrElse(default.upgradeStorageForEMC),
        o.autoScienceEnabled.getOrElse(default.autoScienceEnabled),
        o.autoTechsEnabled.getOrElse(default.autoTechsEnabled),
        o.autoSciTechInterval.getOrElse(default.autoSciTechInterval),
        o.buyAntimatter.getOrElse(default.buyAntimatter),
        o.buyCommunications.getOrElse(default.buyCommunications),
        o.buyMilitary.getOrElse(default.buyMilitary),
        o.buySpaceship.getOrElse(default.buySpaceship),
        o.buyNanoswarm.getOrElse(default.buyNanoswarm),
        o.buyFreeItems.getOrElse(default.buyFreeItems),
        o.bulkBuyMachines.getOrElse(default.bulkBuyMachines),
        o.bulkBuyOnlyWhenRich.getOrElse(default.bulkBuyOnlyWhenRich),
      )
    )

  implicit val r: Reusability[Options] = RequiredOptions.r.contramap(o => setDefaults(o))
}

object RequiredOptions {
  implicit val r: Reusability[RequiredOptions] = Reusability.by_==
}

case class RequiredOptions(
    autoDyson: Boolean,
    ringCount: Int,
    swarmCount: Int,
    autoBuySphere: Boolean,
    buySwarmsAfterSphere: Boolean,
    taskInterval: Int,
    autoEmc: Boolean,
    emcOnlyMeteorite: Boolean,
    emcOnlyWhenFull: Boolean,
    storageEnabled: Boolean,
    onlyUpgradeStorageWhenFull: Boolean,
    upgradeStorageForEMC: Boolean,
    autoScienceEnabled: Boolean,
    autoTechsEnabled: Boolean,
    autoSciTechInterval: Int,
    buyAntimatter: Boolean,
    buyCommunications: Boolean,
    buyMilitary: Boolean,
    buySpaceship: Boolean,
    buyNanoswarm: Boolean,
    buyFreeItems: Boolean,
    bulkBuyMachines: Boolean,
    bulkBuyOnlyWhenRich: Boolean,
) {

  def combineWith(n: Options): RequiredOptions = RequiredOptions(
    n.autoDyson getOrElse autoDyson,
    n.ringCount getOrElse ringCount,
    n.swarmCount getOrElse swarmCount,
    n.autoBuySphere getOrElse autoBuySphere,
    n.buySwarmsAfterSphere getOrElse buySwarmsAfterSphere,
    n.taskInterval getOrElse taskInterval,
    n.autoEmc getOrElse autoEmc,
    n.emcOnlyMeteorite getOrElse emcOnlyMeteorite,
    n.emcOnlyWhenFull getOrElse emcOnlyWhenFull,
    n.storageEnabled getOrElse storageEnabled,
    n.onlyUpgradeStorageWhenFull getOrElse onlyUpgradeStorageWhenFull,
    n.upgradeStorageForEMC getOrElse upgradeStorageForEMC,
    n.autoScienceEnabled getOrElse autoScienceEnabled,
    n.autoTechsEnabled getOrElse autoTechsEnabled,
    n.autoSciTechInterval getOrElse autoSciTechInterval,
    n.buyAntimatter getOrElse buyAntimatter,
    n.buyCommunications getOrElse buyCommunications,
    n.buyMilitary getOrElse buyMilitary,
    n.buySpaceship getOrElse buySpaceship,
    n.buyNanoswarm getOrElse buyNanoswarm,
    n.buyFreeItems getOrElse buyFreeItems,
    n.bulkBuyMachines getOrElse bulkBuyMachines,
    n.bulkBuyOnlyWhenRich getOrElse bulkBuyOnlyWhenRich,
  )

  def toOptions: Options = Options(
    autoDyson,
    ringCount,
    swarmCount,
    autoBuySphere,
    buySwarmsAfterSphere,
    taskInterval,
    autoEmc,
    emcOnlyMeteorite,
    emcOnlyWhenFull,
    storageEnabled,
    onlyUpgradeStorageWhenFull,
    upgradeStorageForEMC,
    autoScienceEnabled,
    autoTechsEnabled,
    autoSciTechInterval,
    buyAntimatter,
    buyCommunications,
    buyMilitary,
    buySpaceship,
    buyNanoswarm,
    buyFreeItems,
    bulkBuyMachines,
    bulkBuyOnlyWhenRich,
  )

  def toJson: String = js.JSON.stringify(toOptions)

}

case class BuildMachinesOpts(
    leaveUnbuilt: js.UndefOr[Boolean] = js.undefined,
    limitTo: js.UndefOr[Set[String]] = js.undefined,
)
