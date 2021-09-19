package autong

import autong.AutoNG.{NotifListener, OptionsListener, StartListener}
import autong.Bootstrap.bootstrapUi
import autong.Buying.{buildAllMachines, buildFreeItems}
import autong.Nav.navToPage
import autong.Science.{buildAllScience, navAndBuildAllScience}
import autong.Selectors.{
  currentPageCards,
  currentPageName,
  ringBuyButtons,
  segmentBuyButtons,
  segmentSections,
  sideNavEntries,
  swarmBuyButtons,
  Card,
  Section,
}
import autong.Technologies.navAndBoostUnlockAllTechs
import japgolly.scalajs.react._
import org.scalajs.dom
import zio.clock.instant
import zio.duration.durationInt
import zio.{ExitCode, Fiber, URIO, ZIO, ZRef}

import scala.scalajs.js

object AutoNGMain extends zio.App {

  private case class RetVal(lastScienceTime: Option[Long] = None)

  private def currPageIs(name: String) = currentPageName.optional.map(_ contains name)

  private def click250(segment: Card) =
    segmentSections(segment).flatMap { case (_, buttons) => segmentBuyButtons(buttons) }.map { case (_, _, eq250, _) =>
      eq250.click()
    }

  private def buySphere(sphere: Card) =
    sphere.buyButtons.optional.map(_.filter(_.size == 2).foreach(_.head.click())).asSomeError

  private def clickRing(ring: Card) =
    ring.buyButtons.flatMap(ringBuyButtons).map { case (eq50PlusRing, _) => eq50PlusRing.click() }

  private def clickSwarm(swarm: Card) =
    swarm.buyButtons.flatMap(swarmBuyButtons).map { case (eq100PlusSwarm, _) => eq100PlusSwarm.click() }

  private def clickEMC(onlyMeteorite: Boolean) = (costs: Section) => {
    val rows =
      if (onlyMeteorite) costs.costRows.flatMap(ZIO.filter(_)(_.rowName.map(_ == "Meteorite"))) else costs.costRows

    rows.flatMap(ZIO.foreach_(_)(_.emcButton.map(_.click()).optional.asSomeError))
  }

  private def upgradeStorage(onlyUpgradeWhenFull: Boolean) =
    sideNavEntries.flatMap(ZIO.foreach_(_) { e =>
      val nb     = e.navButton
      val stored = nb.flatMap(_.amountStored).optional
      val total  = nb.flatMap(_.totalStorage).optional

      val storedAndEqualSame = for {
        s <- stored
        t <- total
      } yield s == t

      e.upgradeButton.optional
        .map(_.foreach(b => if (!b.classList.contains("disabled")) b.click()))
        .whenM(!RPure(onlyUpgradeWhenFull) || (stored.map(_.isDefined) && storedAndEqualSame))
    })

  private def doWork(opts: RequiredOptions, lastScienceTime: Long) = {
    val empty = RTask(Option.empty[RetVal])

    val runAutoSciTechIfNeeded = (currPage: Option[String], elapsed: Long, currTime: Long) =>
      currPage.fold(empty) { currPage =>
        if (elapsed < opts.autoSciTechInterval) empty
        else
          for {
            _ <- navAndBuildAllScience.when(opts.autoScienceEnabled)
            _ <- navAndBoostUnlockAllTechs.when(opts.autoTechsEnabled)
            _ <- navToPage(currPage)
          } yield Some(RetVal(Some(currTime)))
      }

    val runAutoScienceAndTech: RTask[Option[RetVal]] =
      if (!(opts.autoScienceEnabled || opts.autoTechsEnabled)) empty
      else
        for {
          currTime <- instant.map(_.toEpochMilli)
          elapsed = currTime - lastScienceTime
          currPage <- currentPageName.optional
          r        <- runAutoSciTechIfNeeded(currPage, elapsed, currTime)
        } yield r

    val doStorage = upgradeStorage(opts.onlyUpgradeStorageWhenFull).when(opts.storageEnabled)

    ZIO.ifM(currPageIs("Dyson"))(
      {
        val doDyson =
          currentPageCards
            .map(cards => List(cards.headOption, cards.lift(1), cards.lift(2), cards.lift(3)).map(ZIO.fromOption(_)))
            .flatMap {
              case segment :: ring :: swarm :: sphere :: Nil =>
                val doEMC =
                  segment.flatMap(_.costs).flatMap(clickEMC(opts.emcOnlyMeteorite)).optional.when(opts.autoEmc)

                val sphereBuyButtons = sphere.flatMap(_.buyButtons)

                // if we have a sphere card but it has no buy buttons, we've bought it already
                val spherePurchased =
                  sphere.optional.map(_.isDefined) && sphereBuyButtons.optional.map(_.forall(_.isEmpty))

                (doEMC *> spherePurchased.flatMap { spherePurchased =>
                  ZIO
                    .ifM((RPure(opts.buySwarmsAfterSphere) && RPure(spherePurchased)).asSomeError)(
                      swarm >>= clickSwarm,
                      ring.flatMap(_.count).flatMap { ringCount =>
                        if (ringCount < opts.ringCount) ring >>= clickRing
                        else
                          swarm.flatMap(_.count).flatMap { swarmCount =>
                            if (swarmCount < opts.swarmCount) swarm >>= clickSwarm
                            else if (opts.autoBuySphere) sphere >>= buySphere
                            else (segment >>= click250).unless(spherePurchased)
                          }
                      },
                    )
                    .optional
                }).asSomeError

              case _ => ZIO.unit // compiler doesn't know we always have a list of 4 items
            }

        doDyson.optional *> doStorage *> runAutoScienceAndTech
      }, {
        val doEmc     = emcPage(opts.emcOnlyMeteorite).optional.when(opts.autoEmc && opts.emcAllPages)
        val buyFree   = buildFreeItems.when(opts.buyFreeItems).unlessM(currPageIs("Science"))
        val doScience = buildAllScience.whenM(currPageIs("Science") && RPure(opts.autoScienceEnabled))
        val doMilitary = ZIO.ifM(RPure(opts.buyMilitary) && currPageIs("Military"))(
          buildAllMachines(BuildMachinesOpts(false)) *> runAutoScienceAndTech,
          empty,
        )
        val doComms = (retVal: Option[RetVal]) =>
          ZIO.ifM(RPure(opts.buyCommunications) && currPageIs("Communication"))(
            currentPageCards.flatMap { cards =>
              val buyBtn = (name: String) =>
                ZIOfind(cards)(c => c.name.map(_ == name))
                  .flatMap(ZIO.fromOption(_))
                  .flatMap(_.buyButtons)
                  .map(_.lastOption)

              for {
                ab  <- buyBtn("Astronomical Breakthrough")
                irs <- buyBtn("Interstellar Radar Scanner")
              } yield (ab orElse irs).foreach(_.click())
            }.optional *> runAutoScienceAndTech,
            RPure(retVal),
          )

        doStorage *> doEmc *> buyFree *> doScience *> (doMilitary >>= doComms)
      },
    )

  }

  private def emcPage(onlyMeteorite: Boolean) =
    currentPageCards.map(_.reverse).flatMap { cards =>
      ZIO.foreach_(cards) { card =>
        val rows =
          if (onlyMeteorite) card.costs.flatMap(_.costRows).flatMap(ZIO.filter(_)(r => r.rowName.map(_ == "Meteorite")))
          else card.costs.flatMap(_.costRows)

        rows.flatMap(ZIO.foreach_(_)(row => row.emcButton.map(_.click())))
      }
    }

  private def currPageName(sendNotification: (String) => RTask[Unit])(f: (String) => RTask[Unit]) =
    currentPageName.optional.flatMap(_.fold(sendNotification("Couldn't find a current page name"))(f))

  private def doEmc(sendNotification: (String) => RTask[Unit]) = (options: js.UndefOr[EMCOptions]) =>
    currPageName(sendNotification) { pageName =>
      lazy val runLoop: RTask[Unit] =
        ZIO
          .ifM(currPageIs(pageName))(
            emcPage(false).optional *> runLoop,
            sendNotification(s"Stopped Auto-EMCing on $pageName"),
          )
          .delay(options.flatMap(_.taskInterval).getOrElse(5000).millis)

      sendNotification(s"Auto-EMCing while on $pageName") *> runLoop.forkDaemon.as {}
    }

  private def doBuyMachines(sendNotification: (String) => RTask[Unit], options: RequiredOptions) =
    (buildOpts: BuildMachinesOpts) =>
      currPageName(sendNotification) { pageName =>
        lazy val runLoop: RTask[Unit] =
          ZIO
            .ifM(currPageIs(pageName))(
              buildAllMachines(buildOpts) *> runLoop,
              sendNotification(s"Stopped auto-buying on $pageName"),
            )
            .delay(options.taskInterval.millis)

        sendNotification(s"Auto-buying while on $pageName") *> runLoop.forkDaemon.as {}
      }

  private case class ANGState(
      runningState: RunningState,
      options: RequiredOptions,
      startListeners: List[StartListener] = Nil,
      optionListeners: List[OptionsListener] = Nil,
      notifListeners: List[NotifListener] = Nil,
      lastScienceTime: Long = 0,
      workFiberX: Option[Fiber.Runtime[Throwable, Unit]] = None,
  )

  private def saveToLocalStorage(key: String)(obj: js.Object) = for {
    _ <- RT
    json = js.JSON.stringify(obj)
    _ <- RPure(dom.window.localStorage.setItem(key, json)).option
  } yield {}

  private def localStorageObject[T](key: String) = (for {
    json <- ZIO.fromOption(Option(dom.window.localStorage.getItem(key)))
    obj = js.JSON.parse(json).asInstanceOf[T]
  } yield obj).optional

  private val savedRunningState = localStorageObject[RunningState]("autoNGsRunningState")

  private val savedRunningStateOrDefault = savedRunningState.map(_ getOrElse RunningState.default)

  private val savedOptions = localStorageObject[Options]("autoNGsOptions")

  private val savedOptionsOrDefault = savedOptions.map(_ getOrElse Options())

  private val stateRef = for {
    rs   <- savedRunningStateOrDefault
    opts <- savedOptionsOrDefault
    ref  <- ZRef.make(ANGState(rs, Options setDefaults opts))
  } yield ref

  private def createControllerAndStart(stateRef: zio.Ref[ANGState]) = {
    val workFiber = stateRef.foldAll(
      identity,
      identity,
      identity,
      (w: Option[Fiber.Runtime[Throwable, Unit]]) => s => Right(s.copy(workFiberX = w)),
      s => Right(s.workFiberX),
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
    val startLists = stateRef.foldAll(
      identity,
      identity,
      identity,
      (li: List[StartListener]) => s => Right(s.copy(startListeners = li)),
      s => Right(s.startListeners),
    )
    val optsLists = stateRef.foldAll(
      identity,
      identity,
      identity,
      (li: List[OptionsListener]) => s => Right(s.copy(optionListeners = li)),
      s => Right(s.optionListeners),
    )
    val notifLists = stateRef.foldAll(
      identity,
      identity,
      identity,
      (li: List[NotifListener]) => s => Right(s.copy(notifListeners = li)),
      s => Right(s.notifListeners),
    )

    val addStartListenerZ    = (li: StartListener) => startLists.update(li :: _) *> (startedZ >>= (li(_)))
    val removeStartListenerZ = (li: StartListener) => startLists.update(_.filterNot(_ == li))
    val fireStartListeners   = startedZ >>= (st => startLists.get >>= (ZIO.foreach_(_)(_(st))))
    val addOptionsListenerZ  = (li: OptionsListener) => optsLists.update(li :: _) *> (currOptsZ >>= (li(_)))
    val removeOptsListenerZ  = (li: OptionsListener) => optsLists.update(_.filterNot(_ == li))
    val fireOptionListeners  = currOptsZ >>= (o => optsLists.get >>= (ZIO.foreach_(_)(_(o))))
    val addNotifListenerZ    = (li: NotifListener) => notifLists.update(li :: _)
    val removeNotifListenerZ = (li: NotifListener) => notifLists.update(_.filterNot(_ == li))
    val sendNotificationZ    = (notif: String) => notifLists.get >>= (ZIO.foreach_(_)(_(notif)))

    val updateRunningState = runStateZ.get >>= saveToLocalStorage("autoNGsRunningState")
    val setStarted         = (started: Boolean) => runStateZ.update(_.copy(started = started)) *> updateRunningState

    val handleRetVal = (r: Option[RetVal]) =>
      ZIO.succeed(r.flatMap(_.lastScienceTime)) >>= {
        case None      => RT
        case Some(lst) => stateRef.update(_.copy(lastScienceTime = lst))
      }

    lazy val work = (stateRef.get >>= { state =>
      (doWork(state.options, state.lastScienceTime) >>= handleRetVal).uninterruptible
        .delay(state.options.taskInterval.millis)
    }).forever
    val startWorking = work.forkDaemon.map(Some(_)) >>= workFiber.set
    val stopWorking  = workFiber.modify(wf => wf -> None).flatMap(ZIO.fromOption(_)).flatMap(_.interrupt).optional.unit

    val doStart     = fireStartListeners *> startWorking
    val startZ      = (setStarted(true) *> doStart).unlessM(startedZ)
    val stopZ       = (setStarted(false) *> fireStartListeners *> stopWorking).whenM(startedZ)
    val saveOptions = currOptsZ.map(_.toOptions) >>= saveToLocalStorage("autoNGsOptions")

    val reconfigureZ = (opts: js.UndefOr[Options]) =>
      stateRef.update(_.copy(options = Options.setDefaults(opts))) *> saveOptions *> fireOptionListeners

    val setOptionsZ = (opts: Options) =>
      stateRef.update(s => s.copy(options = s.options.combineWith(opts))) *> saveOptions *> fireOptionListeners

    val startEmc     = (o: js.UndefOr[EMCOptions]) => doEmc(sendNotificationZ)(o).forkDaemon.unit
    val buyMachinesZ = (o: BuildMachinesOpts) => currOptsZ >>= (doBuyMachines(sendNotificationZ, _)(o).forkDaemon.unit)

    doStart
      .whenM(runStateZ.map(_.started).get)
      .as(new AutoNG {
        override def start: RTask[Unit]                                      = startZ
        override def stop: RTask[Unit]                                       = stopZ
        override def reconfigure(options: js.UndefOr[Options]): RTask[Unit]  = reconfigureZ(options)
        override def setOptions(options: Options): RTask[Unit]               = setOptionsZ(options)
        override def emc: (js.UndefOr[EMCOptions]) => RTask[Unit]            = startEmc
        override def buyMachines: (BuildMachinesOpts) => RTask[Unit]         = buyMachinesZ
        override def sendNotification(notif: String): RTask[Unit]            = sendNotificationZ(notif)
        override def addStartListener(li: StartListener): RPure[Unit]        = addStartListenerZ(li).purify
        override def removeStartListener(li: StartListener): RPure[Unit]     = removeStartListenerZ(li).purify
        override def addOptionsListener(li: OptionsListener): RPure[Unit]    = addOptionsListenerZ(li).purify
        override def removeOptionsListener(li: OptionsListener): RPure[Unit] = removeOptsListenerZ(li).purify
        override def addNotifListener(li: NotifListener): RPure[Unit]        = addNotifListenerZ(li).purify
        override def removeNotifListener(li: NotifListener): RPure[Unit]     = removeNotifListenerZ(li).purify
      })
  }

  private val program = for {
    sr   <- stateRef
    cont <- createControllerAndStart(sr)
    ret  <- bootstrapUi(cont)
  } yield ret

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.exitCode
}
