package autong

import autong.AutoNG.Started
import autong.Bootstrap.bootstrapUi
import autong.Buying.{buildAllMachines, buildBulkMachines, buildFreeItems, OnBulkBuy}
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

  private def currPageIs(name: String) = currentPageName.optional.map(_ contains name)

  private def clickBuy(name: String) = (card: Card) =>
    for {
      b <- card.buyButton(name)
      _ <- b.click.asSomeError
    } yield {}

  private val click250   = clickBuy("= 250")
  private val buySphere  = clickBuy("= 250 + Sphere")
  private val clickRing  = clickBuy("= 50 + Ring")
  private val clickSwarm = clickBuy("= 100 + Swarm")

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
    val rows =
      if (onlyMeteorite) card.costRows.flatMap(ZIO.filter(_)(_.rowName.map(_ == "Meteorite"))) else card.costRows

    rows.flatMap(ZIO.foreach_(_)(r => clickEMCButtonIfWanted(emcOnlyWhenFull)(r).optional.asSomeError))
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

        ifM(!ZIO.fromOption(Some(onlyUpgradeWhenFull)) || isFull)(buttonAndName, ZIO.fail(None))
      }
      btnsToClick.flatMap(ZIO.foreach_(_) { case (b, name) =>
        (b.click *> sendNotification(s"Upgraded $name Storage")).unlessM(b.disabled)
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

    val runAutoScienceAndTech: RIO[Clock with Has[UIInterface], Option[RetVal]] =
      if (!(opts.autoScienceEnabled || opts.autoTechsEnabled)) empty
      else
        for {
          currTime <- instant.map(_.toEpochMilli)
          elapsed = currTime - lastScienceTime
          currPage <- currentPageName.optional
          r        <- runAutoSciTechIfNeeded(currPage, elapsed, currTime)
        } yield r

    val doStorage = upgradeStorage(opts.onlyUpgradeStorageWhenFull).when(opts.storageEnabled)

    ifM(currPageIs("Dyson"))(
      {
        val doDyson =
          currentPageCards
            .map(cards => List(cards.headOption, cards.lift(1), cards.lift(2), cards.lift(3)).map(ZIO.fromOption(_)))
            .flatMap {
              case segment :: ring :: swarm :: sphere :: Nil =>
                val doEMC =
                  segment.flatMap(clickEMC(opts.emcOnlyMeteorite, opts.emcOnlyWhenFull)).optional.when(opts.autoEmc)

                val sphereBuyButtons = sphere.flatMap(_.buyButtons)

                // if we have a sphere card but it has no buy buttons, we've bought it already
                val spherePurchased =
                  sphere.optional.map(_.isDefined) && sphereBuyButtons.optional.map(_.forall(_.isEmpty))

                (doEMC *> ifM((RPure(opts.buySwarmsAfterSphere) && spherePurchased).asSomeError)(
                  swarm >>= clickSwarm,
                  ifM(ring.flatMap(_.count).map(_ < opts.ringCount))(
                    ring >>= clickRing,
                    ifM(swarm.flatMap(_.count).map(_ < opts.swarmCount))(
                      swarm >>= clickSwarm,
                      ifM(ZIO.succeed(opts.autoBuySphere).asSomeError)(
                        sphere >>= buySphere,
                        (segment >>= click250).unlessM(spherePurchased.asSomeError),
                      ),
                    ),
                  ),
                ).optional).asSomeError

              case _ => ZIO.unit // compiler doesn't know we always have a list of 4 items
            }

        doDyson.optional *> doStorage *> runAutoScienceAndTech
      }, {
        def buildAllMachinesFor(pageName: String, when: Boolean) = (retVal: Option[RetVal]) =>
          ifM(ZIO.succeed(when) && currPageIs(pageName))(
            buildAllMachines(BuildMachinesOpts(false)) *> runAutoScienceAndTech,
            ZIO.succeed(retVal),
          )

        val onBulkBuy: OnBulkBuy[Has[Notifications]] =
          (in, am, ab) => sendNotification(s"Bought $ab to reach $am on $in")
        val doEmc = emcPage(opts.emcOnlyMeteorite, opts.emcOnlyWhenFull).optional.when(opts.autoEmc && opts.emcAllPages)
        val buyFree      = buildFreeItems(onBulkBuy).when(opts.buyFreeItems).unlessM(currPageIs("Science"))
        val buyBulk      = buildBulkMachines(onBulkBuy).when(opts.bulkBuyMachines)
        val doScience    = buildAllScience.whenM(currPageIs("Science") && RPure(opts.autoScienceEnabled))
        val doAntimatter = buildAllMachinesFor("Antimatter", opts.buyAntimatter)
        val doMilitary   = buildAllMachinesFor("Military", opts.buyMilitary)
        val doSpaceship  = buildAllMachinesFor("Spaceship", opts.buySpaceship)

        val doComms =
          ifM(ZIO.succeed(opts.buyCommunications) && currPageIs("Communication"))(
            currentPageCards.optional.flatMap {
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

        doStorage *> doEmc *> buyFree *> buyBulk *> doScience *> (doComms >>= doMilitary >>= doSpaceship >>= doAntimatter)
      },
    )

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

  private def doEmc(options: js.UndefOr[EMCOptions]) =
    currPageName { pageName =>
      lazy val runLoop: RIO[ZEnv with Has[UIInterface] with Has[Notifications], Unit] =
        ifM(currPageIs(pageName))(
          emcPage(onlyMeteorite = false, onlyWhenFull = false).optional *> runLoop,
          sendNotification(s"Stopped Auto-EMCing on $pageName"),
        )
          .delay(options.flatMap(_.taskInterval).getOrElse(5000).millis)

      sendNotification(s"Auto-EMCing while on $pageName") *> runLoop.forkDaemon.as {}
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

    val startEmc     = (o: js.UndefOr[EMCOptions]) => doEmc(o).forkDaemon.unit
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
      override def emc: (js.UndefOr[EMCOptions]) => RTask[Unit]           = o => startEmc(o).psl(comb)
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
    ret <- bootstrapUi(cont)
  } yield ret

  private val layers = Storage.live ++ UIInterface.live

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.provideCustomLayer(layers).exitCode
}
