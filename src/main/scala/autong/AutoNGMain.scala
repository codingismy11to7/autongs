package autong

import autong.AutoNG.{NotifListener, OptionsListener, StartListener}
import autong.Bootstrap.bootstrapUi
import autong.Buying.buildAllMachines
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
import zio.prelude.fx.ZPure
import zio.{ExitCode, Fiber, URIO, ZIO, ZRef}

import scala.scalajs.js

object AutoNGMain extends zio.App {

  private case class RetVal(lastScienceTime: js.UndefOr[Long] = js.undefined)

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
    val empty = RTask[js.UndefOr[RetVal]](js.undefined)

    val runAutoSciTechIfNeeded = (currPage: Option[String], elapsed: Long, currTime: Long) =>
      currPage.fold(empty) { currPage =>
        if (elapsed < opts.autoSciTechInterval) empty
        else
          for {
            _ <- navAndBuildAllScience.when(opts.autoScienceEnabled)
            _ <- navAndBoostUnlockAllTechs.when(opts.autoTechsEnabled)
            _ <- navToPage(currPage)
          } yield js.defined(RetVal(currTime))
      }

    val runAutoScienceAndTech: RTask[js.UndefOr[RetVal]] =
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
                            else if (!spherePurchased) segment >>= click250
                            else ZIO.unit
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
        val doScience = buildAllScience.whenM(currPageIs("Science") && RPure(opts.autoScienceEnabled))
        val doMilitary = ZIO.ifM(RPure(opts.buyMilitary) && currPageIs("Military"))(
          buildAllMachines(BuildMachinesOpts(false)) *> runAutoScienceAndTech,
          empty,
        )
        val doComms = (retVal: js.UndefOr[RetVal]) =>
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

        doStorage *> doEmc *> doScience *> (doMilitary >>= doComms)
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
      workFiber: Option[Fiber.Runtime[Throwable, Unit]] = None,
  )

  private def saveToLocalStorage(key: String, obj: js.Object) = for {
    _ <- RT
    json = js.JSON.stringify(obj)
    _ <- RPure(dom.window.localStorage.setItem(key, json)).option
  } yield {}

  private def localStorageObject[T](key: String) = for {
    jsonOpt <- ZIO(Option(dom.window.localStorage.getItem(key))).orElseSucceed(None)
    obj = jsonOpt.map(j => js.JSON.parse(j).asInstanceOf[T])
  } yield obj

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
    val startedZ                          = stateRef.map(_.workFiber.isDefined).get
    val currOptsZ: RTask[RequiredOptions] = stateRef.map(_.options).get
    val runStateZ                         = stateRef.map(_.runningState)

    val addStartListenerZ = (li: StartListener) =>
      stateRef.update(s => s.copy(startListeners = li :: s.startListeners)) *> (startedZ >>= (li(_)))
    val removeStartListenerZ = (li: StartListener) =>
      stateRef.update(s => s.copy(startListeners = s.startListeners.filterNot(_ == li)))
    val addOptionsListenerZ = (li: OptionsListener) =>
      stateRef.update(s => s.copy(optionListeners = li :: s.optionListeners)) *> (currOptsZ >>= (li(_)))
    val removeOptsListenerZ = (li: OptionsListener) =>
      stateRef.update(s => s.copy(optionListeners = s.optionListeners.filterNot(_ == li)))
    val addNotifListenerZ = (li: NotifListener) => stateRef.update(s => s.copy(notifListeners = li :: s.notifListeners))
    val removeNotifListenerZ = (li: NotifListener) =>
      stateRef.update(s => s.copy(notifListeners = s.notifListeners.filterNot(_ == li)))
    val sendNotificationZ = (notif: String) =>
      stateRef.map(_.notifListeners).get >>= (lis => ZPure.forEach(lis)(_(notif)).unit)

    val updateRunningState = (rs: RunningState) => saveToLocalStorage("autoNGsRunningState", rs)
    val setStarted = (started: Boolean) =>
      stateRef.update(s =>
        s.copy(runningState = s.runningState.copy(started = started))
      ) *> (runStateZ.get >>= updateRunningState)

    val handleRetVal = (r: js.UndefOr[RetVal]) =>
      RTask(r.toOption.flatMap(_.lastScienceTime.toOption)) >>= {
        case None      => RT
        case Some(lst) => stateRef.update(_.copy(lastScienceTime = lst))
      }

    lazy val scheduleNext: RTask[Unit] = stateRef.get >>= { state =>
      ((doWork(state.options, state.lastScienceTime) >>= handleRetVal).uninterruptible *> scheduleNext)
        .delay(state.options.taskInterval.millis)
    }
    val startWorking = scheduleNext.forkDaemon >>= { fiber => stateRef.update(_.copy(workFiber = Some(fiber))) }
    val stopWorking = stateRef
      .modify(state => state.workFiber -> state.copy(workFiber = None))
      .flatMap(ZIO.fromOption(_))
      .flatMap(_.interrupt)
      .optional
      .unit

    val fireStartListeners  = stateRef.get >>= (st => ZPure.forEach(st.startListeners)(_(st.runningState.started)))
    val doStart             = fireStartListeners *> startWorking
    val startZ              = (setStarted(true) *> doStart).unlessM(startedZ)
    val stopZ               = (setStarted(false) *> fireStartListeners *> stopWorking).whenM(startedZ)
    val fireOptionListeners = stateRef.get >>= (st => ZPure.forEach(st.optionListeners)(_(st.options)).unit)
    val saveOptions         = currOptsZ.map(_.toOptions) >>= (saveToLocalStorage("autoNGsOptions", _))

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
