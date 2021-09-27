package autong

import autong.AutoNG.Started
import autong.Bootstrap.bootstrapUi
import autong.Buying.{buildAllMachines, buildBulkMachines, buildFreeItems, OnBulkBuy}
import autong.Dyson.buildDyson
import autong.Nav.navToPage
import autong.Notifications.sendNotification
import autong.Science.{buildAllScience, navAndBuildAllScience}
import autong.Storage.{load, store}
import autong.Technologies.navAndBoostUnlockAllTechs
import autong.UIInterface._
import japgolly.scalajs.react._
import zio.Clock.instant
import zio.ZIO.ifZIO
import zio._

import scala.scalajs.js

object AutoNGMain extends ZIOAppDefault {

  private[autong] case class RetVal(lastScienceTime: Option[Long] = None)

  private def currPageIs(name: String) = currentPageName.unsome.map(_ contains name)

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

      doClick.whenZIO(isFull)
    }
  }

  private def clickEMC(onlyMeteorite: Boolean, emcOnlyWhenFull: Boolean)(card: Card) = {
    val rows =
      if (onlyMeteorite) card.costRows.flatMap(ZIO.filter(_)(_.rowName.map(_ == "Meteorite"))) else card.costRows

    rows.flatMap(ZIO.foreachDiscard(_)(r => clickEMCButtonIfWanted(emcOnlyWhenFull)(r).unsome.asSomeError))
  }

  private def upgradeStorage(onlyUpgradeWhenFull: Boolean) =
    sideNavEntries.flatMap { es =>
      val btnsToClick = ZIO.collect(es) { e =>
        val isFull = e.navButton.flatMap(_.isFull)

        val buttonAndName = for {
          upg  <- e.upgradeButton
          nav  <- e.navButton
          name <- nav.name
        } yield upg -> name

        ifZIO(!ZIO.fromOption(Some(onlyUpgradeWhenFull)) || isFull)(buttonAndName, ZIO.fail(None))
      }
      btnsToClick.flatMap(ZIO.foreachDiscard(_) { case (b, name) =>
        (b.click *> sendNotification(s"Upgraded $name Storage")).unlessZIO(b.disabled)
      })
    }

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

    val runAutoScienceAndTech: RIO[Has[Clock] with Has[UIInterface], Option[RetVal]] =
      if (!(opts.autoScienceEnabled || opts.autoTechsEnabled)) empty
      else
        for {
          currTime <- instant.map(_.toEpochMilli)
          elapsed = currTime - lastScienceTime
          currPage <- currentPageName.unsome
          r        <- runAutoSciTechIfNeeded(currPage, elapsed, currTime)
        } yield r

    val doStorage = upgradeStorage(opts.onlyUpgradeStorageWhenFull).when(opts.storageEnabled)
    val doEMC     = emcPage(opts.emcOnlyMeteorite, opts.emcOnlyWhenFull).unsome.when(opts.autoEmc)

    def buildAllMachinesFor(pageName: String, when: Boolean) = (retVal: Option[RetVal]) =>
      ifZIO(ZIO.succeed(when) && currPageIs(pageName))(
        buildAllMachines(BuildMachinesOpts(false)) *> runAutoScienceAndTech,
        ZIO.succeed(retVal),
      )

    val isSciencePage = currPageIs("Science")
    val isDysonPage   = currPageIs("Dyson")

    val onBulkBuy: OnBulkBuy[Has[Notifications]] =
      (in, am, ab) => sendNotification(s"Bought $ab to reach $am on $in")
    val buyFree      = buildFreeItems(onBulkBuy).when(opts.buyFreeItems).unlessZIO(isSciencePage || isDysonPage)
    val buyBulk      = buildBulkMachines(onBulkBuy).when(opts.bulkBuyMachines).unlessZIO(isDysonPage)
    val doScience    = buildAllScience.whenZIO(isSciencePage && ZIO.succeed(opts.autoScienceEnabled))
    val doAntimatter = buildAllMachinesFor("Antimatter", opts.buyAntimatter)
    val doMilitary   = buildAllMachinesFor("Military", opts.buyMilitary)
    val doSpaceship  = buildAllMachinesFor("Spaceship", opts.buySpaceship)

    val doComms =
      ifZIO(ZIO.succeed(opts.buyCommunications) && currPageIs("Communication"))(
        currentPageCards.unsome.flatMap {
          case None => ZIO.unit

          case Some(cards) =>
            val buyBtn = (name: String) =>
              (for {
                cOpt <- ZIOfind(cards)(c => c.name.map(_ == name))
                c    <- ZIO.fromOption(cOpt)
                bbs  <- c.buyButtons
                bb   <- ZIO.fromOption(bbs.lastOption)
              } yield bb).unsome

            for {
              ab  <- buyBtn("Astronomical Breakthrough")
              irs <- buyBtn("Interstellar Radar Scanner")
              b = ab orElse irs
              _ <- b.fold[Task[Unit]](ZIO.unit)(_.click)
            } yield {}
        } *> runAutoScienceAndTech,
        empty,
      )

    def doDyson(retVal: Option[RetVal]) = ifZIO(ZIO.succeed(opts.autoDyson) && isDysonPage)(
      buildDyson(opts).unsome *> runAutoScienceAndTech,
      ZIO succeed retVal,
    )

    doStorage *> doEMC *> buyFree *> buyBulk *> doScience *>
      (doComms flatMap doMilitary flatMap doSpaceship flatMap doAntimatter flatMap doDyson)
  }

  private def emcPage(onlyMeteorite: Boolean, onlyWhenFull: Boolean) =
    currentPageCards.map(_.reverse).flatMap { cards =>
      ZIO.foreachDiscard(cards)(card => clickEMC(onlyMeteorite, onlyWhenFull)(card).unsome.asSomeError)
    }

  private def currPageName[R, E](f: (String) => ZIO[R, E, Unit]) =
    currentPageName.unsome.flatMap {
      case None     => sendNotification("Couldn't find a current page name")
      case Some(pn) => f(pn)
    }

  private def doBuyMachines(options: RequiredOptions) =
    (buildOpts: BuildMachinesOpts) =>
      currPageName { pageName =>
        lazy val runLoop: RIO[ZEnv with Has[UIInterface] with Has[Notifications], Unit] =
          ifZIO(currPageIs(pageName))(
            buildAllMachines(buildOpts) *> runLoop,
            sendNotification(s"Stopped auto-buying on $pageName"),
          )
            .delay(options.taskInterval.millis)

        sendNotification(s"Auto-buying while on $pageName") *> runLoop.forkDaemon.as {}
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
      ZLayer.fromFunction(env => (notif: String) => sendNotif(notif).provide(env))

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
      ZIO.succeed(r.flatMap(_.lastScienceTime)).flatMap {
        case None      => RT
        case Some(lst) => stateRef.update(_.copy(lastScienceTime = lst))
      }

    lazy val work = stateRef.get.flatMap { state =>
      (doWork(state.options, state.lastScienceTime) flatMap handleRetVal).uninterruptible
        .catchAll(t => ZIO(t.printStackTrace()))
        .delay(state.options.taskInterval.millis)
    }.forever
    val startWorking = work.forkDaemon.map(Some(_)) flatMap workFiber.set
    val stopWorking  = workFiber.modify(wf => wf -> None).flatMap(ZIO.fromOption(_)).flatMap(_.interrupt).unsome.unit

    val doStart     = startWorking *> fireStartListeners
    val startZ      = (setStarted(true) *> doStart).unlessZIO(startedZ).unit
    val stopZ       = (setStarted(false) *> stopWorking *> fireStartListeners).whenZIO(startedZ).unit
    val saveOptions = currOptsZ.map(_.toOptions) flatMap saveToLocalStorage("autoNGsOptions")

    val reconfigureZ = (opts: js.UndefOr[Options]) =>
      stateRef.update(_.copy(options = Options.setDefaults(opts))) *> saveOptions *> fireOptionListeners

    val setOptionsZ = (opts: Options) =>
      stateRef.update(s => s.copy(options = s.options.combineWith(opts))) *> saveOptions *> fireOptionListeners

    val buyMachinesZ = (o: BuildMachinesOpts) => currOptsZ flatMap (doBuyMachines(_)(o).forkDaemon.unit)

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
        startedHub.subscribe <* fireStartListeners.toManaged
      override val subscribeToOptions: UManaged[Dequeue[RequiredOptions]] =
        optsHub.subscribe <* fireOptionListeners.toManaged
      override val subscribeToNotifs: UManaged[Dequeue[String]] = notifs.notifsHub.subscribe
    }

    for {
      _   <- doStart.whenZIO(runStateZ.map(_.started).get)
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
    ret <- bootstrapUi(cont)
  } yield ret

  private val layers = Storage.live ++ UIInterface.live

  override def run: ZIO[ZEnv with Has[ZIOAppArgs], Any, Any] = program.provideCustomLayer(layers)
}
