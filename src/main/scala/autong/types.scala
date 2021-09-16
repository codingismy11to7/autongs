package autong

import autong.AutoNG.{NotifListener, OptionsListener, StartListener}
import japgolly.scalajs.react._
import zio.prelude.{Equal, EqualOps}

import scala.scalajs.js

object AutoNG {
  type Started         = Boolean
  type StartListener   = (Started) => RPure[Unit]
  type OptionsListener = (RequiredOptions) => RPure[Unit]
  type NotifListener   = (String) => RPure[Unit]

  implicit val r: Reusability[AutoNG] = Reusability.byRef
}

trait AutoNG {
  def start: RTask[Unit]
  def stop: RTask[Unit]

  def reconfigure(options: js.UndefOr[Options]): RTask[Unit]

  def setOptions(options: Options): RTask[Unit]

  def emc: (js.UndefOr[EMCOptions]) => RTask[Unit]
  def buyMachines: (BuildMachinesOpts) => RTask[Unit]

  def sendNotification(notif: String): RTask[Unit]

  def addStartListener(listener: StartListener): RPure[Unit]
  def removeStartListener(listener: StartListener): RPure[Unit]

  def addOptionsListener(listener: OptionsListener): RPure[Unit]
  def removeOptionsListener(listener: OptionsListener): RPure[Unit]

  def addNotifListener(listener: NotifListener): RPure[Unit]
  def removeNotifListener(listener: NotifListener): RPure[Unit]
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
  def ringCount: js.UndefOr[Int]                      = js.undefined
  def swarmCount: js.UndefOr[Int]                     = js.undefined
  def autoBuySphere: js.UndefOr[Boolean]              = js.undefined
  def buySwarmsAfterSphere: js.UndefOr[Boolean]       = js.undefined
  def taskInterval: js.UndefOr[Int]                   = js.undefined
  def autoEmc: js.UndefOr[Boolean]                    = js.undefined
  def emcOnlyMeteorite: js.UndefOr[Boolean]           = js.undefined
  def emcAllPages: js.UndefOr[Boolean]                = js.undefined
  def storageEnabled: js.UndefOr[Boolean]             = js.undefined
  def onlyUpgradeStorageWhenFull: js.UndefOr[Boolean] = js.undefined
  def autoScienceEnabled: js.UndefOr[Boolean]         = js.undefined
  def autoTechsEnabled: js.UndefOr[Boolean]           = js.undefined
  def autoSciTechInterval: js.UndefOr[Int]            = js.undefined
  def buyCommunications: js.UndefOr[Boolean]          = js.undefined
  def buyMilitary: js.UndefOr[Boolean]                = js.undefined
}

object Options {

  implicit val eq: Equal[Options] = Equal.make { (a, b) =>
    a.ringCount === b.ringCount &&
    a.swarmCount === b.swarmCount &&
    a.autoBuySphere === b.autoBuySphere &&
    a.buySwarmsAfterSphere === b.buySwarmsAfterSphere &&
    a.taskInterval === b.taskInterval &&
    a.autoEmc === b.autoEmc &&
    a.emcOnlyMeteorite === b.emcOnlyMeteorite &&
    a.emcAllPages === b.emcAllPages &&
    a.storageEnabled === b.storageEnabled &&
    a.onlyUpgradeStorageWhenFull === b.onlyUpgradeStorageWhenFull &&
    a.autoScienceEnabled === b.autoScienceEnabled &&
    a.autoTechsEnabled === b.autoTechsEnabled &&
    a.autoSciTechInterval === b.autoSciTechInterval &&
    a.buyCommunications === b.buyCommunications &&
    a.buyMilitary === b.buyMilitary
  }

  def apply(
      ringCount0: js.UndefOr[Int] = js.undefined,
      swarmCount0: js.UndefOr[Int] = js.undefined,
      autoBuySphere0: js.UndefOr[Boolean] = js.undefined,
      buySwarmsAfterSphere0: js.UndefOr[Boolean] = js.undefined,
      taskInterval0: js.UndefOr[Int] = js.undefined,
      autoEmc0: js.UndefOr[Boolean] = js.undefined,
      emcOnlyMeteorite0: js.UndefOr[Boolean] = js.undefined,
      emcAllPages0: js.UndefOr[Boolean] = js.undefined,
      storageEnabled0: js.UndefOr[Boolean] = js.undefined,
      onlyUpgradeStorageWhenFull0: js.UndefOr[Boolean] = js.undefined,
      autoScienceEnabled0: js.UndefOr[Boolean] = js.undefined,
      autoTechsEnabled0: js.UndefOr[Boolean] = js.undefined,
      autoSciTechInterval0: js.UndefOr[Int] = js.undefined,
      buyCommunications0: js.UndefOr[Boolean] = js.undefined,
      buyMilitary0: js.UndefOr[Boolean] = js.undefined,
  ): Options = new Options {
    override val ringCount: js.UndefOr[Int]                      = ringCount0
    override val swarmCount: js.UndefOr[Int]                     = swarmCount0
    override val autoBuySphere: js.UndefOr[Boolean]              = autoBuySphere0
    override val buySwarmsAfterSphere: js.UndefOr[Boolean]       = buySwarmsAfterSphere0
    override val taskInterval: js.UndefOr[Int]                   = taskInterval0
    override val autoEmc: js.UndefOr[Boolean]                    = autoEmc0
    override val emcOnlyMeteorite: js.UndefOr[Boolean]           = emcOnlyMeteorite0
    override val emcAllPages: js.UndefOr[Boolean]                = emcAllPages0
    override val storageEnabled: js.UndefOr[Boolean]             = storageEnabled0
    override val onlyUpgradeStorageWhenFull: js.UndefOr[Boolean] = onlyUpgradeStorageWhenFull0
    override val autoScienceEnabled: js.UndefOr[Boolean]         = autoScienceEnabled0
    override val autoTechsEnabled: js.UndefOr[Boolean]           = autoTechsEnabled0
    override val autoSciTechInterval: js.UndefOr[Int]            = autoSciTechInterval0
    override val buyCommunications: js.UndefOr[Boolean]          = buyCommunications0
    override val buyMilitary: js.UndefOr[Boolean]                = buyMilitary0
  }

  implicit class RichOptions(val o: Options) extends AnyVal {

    def copy(
        ringCount: js.UndefOr[Int] = o.ringCount,
        swarmCount: js.UndefOr[Int] = o.swarmCount,
        autoBuySphere: js.UndefOr[Boolean] = o.autoBuySphere,
        buySwarmsAfterSphere: js.UndefOr[Boolean] = o.buySwarmsAfterSphere,
        taskInterval: js.UndefOr[Int] = o.taskInterval,
        autoEmc: js.UndefOr[Boolean] = o.autoEmc,
        emcOnlyMeteorite: js.UndefOr[Boolean] = o.emcOnlyMeteorite,
        emcAllPages: js.UndefOr[Boolean] = o.emcAllPages,
        storageEnabled: js.UndefOr[Boolean] = o.storageEnabled,
        onlyUpgradeStorageWhenFull: js.UndefOr[Boolean] = o.onlyUpgradeStorageWhenFull,
        autoScienceEnabled: js.UndefOr[Boolean] = o.autoScienceEnabled,
        autoTechsEnabled: js.UndefOr[Boolean] = o.autoTechsEnabled,
        autoSciTechInterval: js.UndefOr[Int] = o.autoSciTechInterval,
        buyCommunications: js.UndefOr[Boolean] = o.buyCommunications,
        buyMilitary: js.UndefOr[Boolean] = o.buyMilitary,
    ): Options = Options(
      ringCount,
      swarmCount,
      autoBuySphere,
      buySwarmsAfterSphere,
      taskInterval,
      autoEmc,
      emcOnlyMeteorite,
      emcAllPages,
      storageEnabled,
      onlyUpgradeStorageWhenFull,
      autoScienceEnabled,
      autoTechsEnabled,
      autoSciTechInterval,
      buyCommunications,
      buyMilitary,
    )

  }

  final val default =
    RequiredOptions(
      ringCount = 5,
      swarmCount = 5,
      autoBuySphere = false,
      buySwarmsAfterSphere = false,
      taskInterval = 5000,
      autoEmc = false,
      emcOnlyMeteorite = false,
      emcAllPages = false,
      storageEnabled = true,
      onlyUpgradeStorageWhenFull = true,
      autoScienceEnabled = true,
      autoTechsEnabled = true,
      autoSciTechInterval = 60000,
      buyCommunications = false,
      buyMilitary = false,
    )

  def setDefaults(opts: js.UndefOr[Options]): RequiredOptions =
    opts.fold(default)(o =>
      RequiredOptions(
        o.ringCount.getOrElse(default.ringCount),
        o.swarmCount.getOrElse(default.swarmCount),
        o.autoBuySphere.getOrElse(default.autoBuySphere),
        o.buySwarmsAfterSphere.getOrElse(default.buySwarmsAfterSphere),
        o.taskInterval.getOrElse(default.taskInterval),
        o.autoEmc.getOrElse(default.autoEmc),
        o.emcOnlyMeteorite.getOrElse(default.emcOnlyMeteorite),
        o.emcAllPages.getOrElse(default.emcAllPages),
        o.storageEnabled.getOrElse(default.storageEnabled),
        o.onlyUpgradeStorageWhenFull.getOrElse(default.onlyUpgradeStorageWhenFull),
        o.autoScienceEnabled.getOrElse(default.autoScienceEnabled),
        o.autoTechsEnabled.getOrElse(default.autoTechsEnabled),
        o.autoSciTechInterval.getOrElse(default.autoSciTechInterval),
        o.buyCommunications.getOrElse(default.buyCommunications),
        o.buyMilitary.getOrElse(default.buyMilitary),
      )
    )

  implicit val r: Reusability[Options] = RequiredOptions.r.contramap(o => setDefaults(o))
}

object RequiredOptions {
  implicit val r: Reusability[RequiredOptions] = Reusability.by_==
}

case class RequiredOptions(
    ringCount: Int,
    swarmCount: Int,
    autoBuySphere: Boolean,
    buySwarmsAfterSphere: Boolean,
    taskInterval: Int,
    autoEmc: Boolean,
    emcOnlyMeteorite: Boolean,
    emcAllPages: Boolean,
    storageEnabled: Boolean,
    onlyUpgradeStorageWhenFull: Boolean,
    autoScienceEnabled: Boolean,
    autoTechsEnabled: Boolean,
    autoSciTechInterval: Int,
    buyCommunications: Boolean,
    buyMilitary: Boolean,
) {

  def combineWith(n: Options): RequiredOptions = RequiredOptions(
    n.ringCount getOrElse ringCount,
    n.swarmCount getOrElse swarmCount,
    n.autoBuySphere getOrElse autoBuySphere,
    n.buySwarmsAfterSphere getOrElse buySwarmsAfterSphere,
    n.taskInterval getOrElse taskInterval,
    n.autoEmc getOrElse autoEmc,
    n.emcOnlyMeteorite getOrElse emcOnlyMeteorite,
    n.emcAllPages getOrElse emcAllPages,
    n.storageEnabled getOrElse storageEnabled,
    n.onlyUpgradeStorageWhenFull getOrElse onlyUpgradeStorageWhenFull,
    n.autoScienceEnabled getOrElse autoScienceEnabled,
    n.autoTechsEnabled getOrElse autoTechsEnabled,
    n.autoSciTechInterval getOrElse autoSciTechInterval,
    n.buyCommunications getOrElse buyCommunications,
    n.buyMilitary getOrElse buyMilitary,
  )

  def toOptions: Options = Options(
    ringCount,
    swarmCount,
    autoBuySphere,
    buySwarmsAfterSphere,
    taskInterval,
    autoEmc,
    emcOnlyMeteorite,
    emcAllPages,
    storageEnabled,
    onlyUpgradeStorageWhenFull,
    autoScienceEnabled,
    autoTechsEnabled,
    autoSciTechInterval,
    buyCommunications,
    buyMilitary,
  )

  def toJson: String = js.JSON.stringify(toOptions)

}

case class EMCOptions(taskInterval: js.UndefOr[Int] = js.undefined)

case class BuildMachinesOpts(
    leaveUnbuilt: js.UndefOr[Boolean] = js.undefined,
    limitTo: js.UndefOr[Set[String]] = js.undefined,
)
