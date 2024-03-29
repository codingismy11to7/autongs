package autong

import autong.AutoNG.Started
import autong.Bootstrap.bootstrapUi
import autong.Buying._
import autong.Dyson.buildDyson
import autong.Nav.navToPage
import autong.Notifications.sendNotification
import autong.Science.{buildAllScience, navAndBuildAllScience}
import autong.Storage.{load, store}
import autong.Technologies.navAndBoostUnlockAllTechs
import autong.UIInterface._
import japgolly.scalajs.react._
import zio.ZIO.ifM
import zio._
import zio.clock.{instant, Clock}
import zio.duration.durationInt

import scala.scalajs.js

object AutoNGMain extends zio.App {

  private[autong] case class RetVal(lastScienceTime: Option[Long] = None)

  private val energyRe = """([+]?)([\d.]+)(.?)""".r

  private val currentEnergyPerSec: RIO[Has[UIInterface], Double] =
    findSideNav("Energy").flatMap(_.navButton).flatMap(_.amountStored).optional.map {
      _.flatMap {
        case energyRe(plus, num, units) =>
          val value    = Utils.getValueWithUnits(num.toDouble, units)
          val positive = plus == "+"
          value.map(v => if (positive) v else 0 - v)

        case _ => None
      }.getOrElse(0)
    }

  private def currentEnergyGreaterThan(amt: Long) = currentEnergyPerSec.map(_ > amt)

  private def currPageIs(name: String) = currentPageName.optional.map(_ contains name)

  private def clickEMCButtonIfWanted(emcOnlyWhenFull: Boolean)(row: CostRow) = {
    val doClick = row.emcButton.flatMap(_.click.asSomeError)

    if (!emcOnlyWhenFull) doClick
    else {
      val nav = row.rowName.flatMap {
        case "Meteorite" => findSideNav("Plasma")
        case _           => findSideNav("Batteries")
      }
      val isFull = for {
        n      <- nav
        nb     <- n.navButton
        isFull <- nb.isFull
      } yield isFull

      doClick.whenM(isFull)
    }
  }

  private def clickEMC(onlyMeteorite: Boolean, emcOnlyWhenFull: Boolean)(card: Card) = {
    val costRows = card.costRows
      .flatMap(ZIO.foreach(_)(r => r.timeRemainingSeconds.optional.map(_ -> r).asSomeError))
      .map(_.sortBy(_._1.getOrElse(0)).map(_._2).reverse)

    val rows = if (onlyMeteorite) costRows.flatMap(ZIO.filter(_)(_.rowName.map(_ == "Meteorite"))) else costRows

    rows.flatMap(ZIO.foreach_(_)(r => clickEMCButtonIfWanted(emcOnlyWhenFull)(r).optional.asSomeError))
  }

  private def upgradeAllStorage(onlyUpgradeWhwenFull: Boolean) =
    sideNavEntries.flatMap(upgradeStorage(onlyUpgradeWhwenFull))

  private[autong] def doWork(opts: RequiredOptions, lastScienceTime: Long) = {
    val empty = ZIO.succeed(Option.empty[RetVal])

    val runAutoSciTechIfNeeded = (currPage: Option[String], elapsed: Long, currTime: Long) =>
      if (elapsed < opts.autoSciTechInterval) empty
      else
        currPage match {
          case None => empty
          case Some(currPage) =>
            for {
              _ <- navAndBuildAllScience.when(opts.autoScienceEnabled)
              _ <- navAndBoostUnlockAllTechs.when(opts.autoTechsEnabled)
              _ <- navToPage(currPage)
            } yield Some(RetVal(Some(currTime)))
        }

    val runAutoScienceAndTech: RIO[Clock with Has[UIInterface], Option[RetVal]] =
      if (!(opts.autoScienceEnabled || opts.autoTechsEnabled)) empty
      else
        for {
          currTime <- instant.map(_.toEpochMilli)
          elapsed = currTime - lastScienceTime
          currPage <- currentPageName.optional
          r        <- runAutoSciTechIfNeeded(currPage, elapsed, currTime)
        } yield r

    val doStorage = upgradeAllStorage(opts.onlyUpgradeStorageWhenFull).when(opts.storageEnabled)
    val doEMC     = emcPage(opts.emcOnlyMeteorite, opts.emcOnlyWhenFull).optional.when(opts.autoEmc)

    val doNeededStorageUpgrades = buyNeededStorageUpgrades.when(opts.upgradeStorageForEMC)

    def buildAllMachinesFor(pageName: String, when: Boolean) = (retVal: Option[RetVal]) =>
      ifM(ZIO.succeed(when) && currPageIs(pageName))(
        doNeededStorageUpgrades *> buildAllMachines(BuildMachinesOpts(false)) *> runAutoScienceAndTech,
        ZIO.succeed(retVal),
      )

    val isSciencePage = currPageIs("Science")
    val isDysonPage   = currPageIs("Dyson")

    val onBulkBuy: OnBulkBuy[Has[Notifications]] =
      (in, am, ab) => sendNotification(s"Bought $ab to reach $am on $in")
    val buyFree       = buildFreeItems(onBulkBuy).when(opts.buyFreeItems).unlessM(isSciencePage || isDysonPage)
    val shouldBuyBulk = if (opts.bulkBuyOnlyWhenRich) currentEnergyGreaterThan(2_000_000) else ZIO.succeed(true)
    val buyBulk      = buildBulkMachines(onBulkBuy).when(opts.bulkBuyMachines).whenM(shouldBuyBulk).unlessM(isDysonPage)
    val doScience    = buildAllScience.whenM(isSciencePage && ZIO.succeed(opts.autoScienceEnabled))
    val doAntimatter = buildAllMachinesFor("Antimatter", opts.buyAntimatter)
    val doMilitary   = buildAllMachinesFor("Military", opts.buyMilitary)
    val doSpaceship  = buildAllMachinesFor("Spaceship", opts.buySpaceship)
    val doNanoswarms = buildAllMachinesFor("Nanoswarm", opts.buyNanoswarm)

    val doComms =
      ifM(ZIO.succeed(opts.buyCommunications) && currPageIs("Communication"))(
        doNeededStorageUpgrades *> currentPageCards.optional.flatMap {
          case None => ZIO.unit

          case Some(cards) =>
            val buyBtn = (name: String) =>
              (for {
                cOpt <- ZIOfind(cards)(c => c.name.map(_ == name))
                c    <- ZIO.fromOption(cOpt)
                bbs  <- c.buyButtons
                bb   <- ZIO.fromOption(bbs.lastOption)
              } yield bb).optional

            for {
              ab  <- buyBtn("Astronomical Breakthrough")
              irs <- buyBtn("Interstellar Radar Scanner")
              b = ab orElse irs
              _ <- b.fold[Task[Unit]](ZIO.unit)(_.click)
            } yield {}
        } *> runAutoScienceAndTech,
        empty,
      )

    def doDyson(retVal: Option[RetVal]) = ifM(ZIO.succeed(opts.autoDyson) && isDysonPage)(
      buildDyson(opts).optional *> runAutoScienceAndTech,
      ZIO succeed retVal,
    )

    doStorage *> doEMC *> buyFree *> buyBulk *> doScience *>
      (doComms >>= doMilitary >>= doSpaceship >>= doAntimatter >>= doDyson >>= doNanoswarms)
  }

  private def emcPage(onlyMeteorite: Boolean, onlyWhenFull: Boolean) =
    currentPageCards.map(_.reverse).flatMap { cards =>
      ZIO.foreach_(cards)(card => clickEMC(onlyMeteorite, onlyWhenFull)(card).optional.asSomeError)
    }

  private def currPageName[R, E](f: (String) => ZIO[R, E, Unit]) =
    currentPageName.optional.flatMap {
      case None     => sendNotification("Couldn't find a current page name")
      case Some(pn) => f(pn)
    }

  private def doBuyMachines(options: RequiredOptions) =
    (buildOpts: BuildMachinesOpts) =>
      currPageName { pageName =>
        lazy val runLoop: RIO[ZEnv with Has[UIInterface] with Has[Notifications], Unit] =
          ifM(currPageIs(pageName))(
            buildAllMachines(buildOpts) *> runLoop,
            sendNotification(s"Stopped auto-buying on $pageName"),
          )
            .delay(options.taskInterval.millis)

        sendNotification(s"Auto-buying while on $pageName") *> runLoop.forkDaemon.unit
      }

  private case class ANGState(
      runningState: RunningState,
      options: RequiredOptions,
      lastScienceTime: Long = 0,
      workFiber: Option[Fiber.Runtime[Throwable, Unit]] = None,
  )

  private def saveToLocalStorage(key: String)(obj: js.Object) = for {
    _ <- RT
    _ <- store(key, obj)
  } yield {}

  private def localStorageObject[T](key: String) = for {
    objOpt <- load(key)
  } yield objOpt.map(_.asInstanceOf[T])

  private val savedRunningState = localStorageObject[RunningState]("autoNGsRunningState")

  private val savedRunningStateOrDefault = savedRunningState.map(_ getOrElse RunningState.default)

  private val savedOptions = localStorageObject[Options]("autoNGsOptions")

  private val savedOptionsOrDefault = savedOptions.map(_ getOrElse Options())

  private val stateRef = for {
    rs   <- savedRunningStateOrDefault
    opts <- savedOptionsOrDefault
    ref  <- ZRef.make(ANGState(rs, Options setDefaults opts))
  } yield ref

  implicit private class RichZIO[R, E, V](val z: ZIO[R, E, V]) extends AnyVal {

    def psl[ROut <: Has[_]](
        layer: URLayer[ZEnv, ROut]
    )(implicit ev1: ZEnv with ROut <:< R, tagged: Tag[ROut]): ZIO[ZEnv, E, V] =
      z.provideSomeLayer[ZEnv](layer)

  }

  private case class Notifs(stateRef: Ref[ANGState], notifsHub: Hub[String]) {

    val sendNotif: (String) => URIO[ZEnv, Unit] = notif => notifsHub.publish(notif).unit

    val notifLayer: ZLayer[ZEnv, Nothing, Has[Notifications]] =
      ZLayer.fromFunctionM(env => ZIO.succeed((notif: String) => sendNotif(notif).provide(env)))

  }

  private def createControllerAndStart(
      stateRef: Ref[ANGState],
      optsHub: Hub[RequiredOptions],
      startedHub: Hub[Started],
      notifs: Notifs,
  ) = {
    val workFiber = stateRef.foldAll(
      identity,
      identity,
      identity,
      (w: Option[Fiber.Runtime[Throwable, Unit]]) => s => Right(s.copy(workFiber = w)),
      s => Right(s.workFiber),
    )
    val startedZ  = workFiber.map(_.isDefined).get
    val currOptsZ = stateRef.map(_.options).get
    val runStateZ = stateRef.foldAll(
      identity,
      identity,
      identity,
      (rs: RunningState) => st => Right(st.copy(runningState = rs)),
      s => Right(s.runningState),
    )

    val fireStartListeners  = startedZ.flatMap(startedHub.publish).unit
    val fireOptionListeners = currOptsZ.flatMap(optsHub.publish).unit
    val updateRunningState  = runStateZ.get flatMap saveToLocalStorage("autoNGsRunningState")
    val setStarted          = (started: Boolean) => runStateZ.update(_.copy(started = started)) *> updateRunningState

    val handleRetVal = (r: Option[RetVal]) =>
      ZIO.succeed(r.flatMap(_.lastScienceTime)) >>= {
        case None      => RT
        case Some(lst) => stateRef.update(_.copy(lastScienceTime = lst))
      }

    lazy val work = (stateRef.get >>= { state =>
      (doWork(state.options, state.lastScienceTime) >>= handleRetVal).uninterruptible
        .catchAll(t => ZIO(t.printStackTrace()))
        .delay(state.options.taskInterval.millis)
    }).forever
    val startWorking = work.forkDaemon.map(Some(_)) >>= workFiber.set
    val stopWorking  = workFiber.modify(wf => wf -> None).flatMap(ZIO.fromOption(_)).flatMap(_.interrupt).optional.unit

    val doStart     = startWorking *> fireStartListeners
    val startZ      = (setStarted(true) *> doStart).unlessM(startedZ)
    val stopZ       = (setStarted(false) *> stopWorking *> fireStartListeners).whenM(startedZ)
    val saveOptions = currOptsZ.map(_.toOptions) >>= saveToLocalStorage("autoNGsOptions")

    val reconfigureZ = (opts: js.UndefOr[Options]) =>
      stateRef.update(_.copy(options = Options.setDefaults(opts))) *> saveOptions *> fireOptionListeners

    val setOptionsZ = (opts: Options) =>
      stateRef.update(s => s.copy(options = s.options.combineWith(opts))) *> saveOptions *> fireOptionListeners

    val buyMachinesZ = (o: BuildMachinesOpts) => currOptsZ >>= (doBuyMachines(_)(o).forkDaemon.unit)

    val createANG = for {
      hasSt     <- ZIO.environment[Has[Storage]]
      hasUi     <- ZIO.environment[Has[UIInterface]]
      hasNotifs <- ZIO.environment[Has[Notifications]]
      comb = ZLayer.succeedMany(hasSt ++ hasUi ++ hasNotifs)
    } yield new AutoNG {
      override val start: RTask[Unit]                                     = startZ.psl(comb)
      override val stop: RTask[Unit]                                      = stopZ.psl(comb)
      override def reconfigure(options: js.UndefOr[Options]): RTask[Unit] = reconfigureZ(options).psl(comb)
      override def setOptions(options: Options): RTask[Unit]              = setOptionsZ(options).psl(comb)
      override def buyMachines: (BuildMachinesOpts) => RTask[Unit]        = o => buyMachinesZ(o).psl(comb)
      override def sendNotification(notif: String): RTask[Unit]           = notifs.sendNotif(notif)
      override val subscribeToStarted: UManaged[Dequeue[Started]] =
        startedHub.subscribe <* fireStartListeners.toManaged_
      override val subscribeToOptions: UManaged[Dequeue[RequiredOptions]] =
        optsHub.subscribe <* fireOptionListeners.toManaged_
      override val subscribeToNotifs: UManaged[Dequeue[String]] = notifs.notifsHub.subscribe
    }

    for {
      _   <- doStart.whenM(runStateZ.map(_.started).get)
      ang <- createANG
    } yield ang
  }

  private val program = for {
    sr        <- stateRef
    notifsHub <- Hub.sliding[String](8)
    notifs     = Notifs(sr, notifsHub)
    notifLayer = notifs.notifLayer
    optsHub    <- Hub.sliding[RequiredOptions](8)
    startedHub <- Hub.sliding[Started](4)
    cont <- createControllerAndStart(sr, optsHub, startedHub, notifs)
      .provideSomeLayer[Has[UIInterface] with Has[Storage] with ZEnv](notifLayer)
    _ <- bootstrapUi(cont)
    _ <- cont.sendNotification("Loaded AutoNG").delay(50.millis).forkDaemon
  } yield {}

  private val layers = Storage.live ++ UIInterface.live

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.provideCustomLayer(layers).exitCode
}
