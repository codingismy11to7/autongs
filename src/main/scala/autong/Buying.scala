package autong

import autong.Notifications.sendNotification
import autong.UIInterface.{
  currentPageCardsWithBuyButtons,
  sideNavEntries,
  BulkBuyButton,
  Card,
  ProductionRow,
  SideNavEntry,
}
import zio._

object Buying {

  private def buildAllFromCard(card: Card, numToLeaveUnbuilt: Int) = {
    val currMax = card.maxCanBuild.optional

    card.lastBuyButton.optional.flatMap {
      case None => ZIO.unit

      case Some(btn) =>
        def loop(max: Option[BigDecimal]): Task[Unit] =
          (btn.click *> ZIO.yieldNow *> (currMax >>= loop).when(max.isDefined))
            .when(max.forall(_ > numToLeaveUnbuilt))

        currMax >>= loop
    }

  }

  def upgradeStorage(onlyUpgradeWhenFull: Boolean)(toUpgrade: Seq[SideNavEntry]) = {
    val btnsToClick = ZIO.collect(toUpgrade) { e =>
      val isFull = e.navButton.flatMap(_.isFull)

      val buttonAndName = for {
        upg  <- e.upgradeButton
        nav  <- e.navButton
        name <- nav.name
      } yield upg -> name

      ZIO.ifM(!ZIO.fromOption(Some(onlyUpgradeWhenFull)) || isFull)(buttonAndName, ZIO.fail(None))
    }
    btnsToClick.flatMap(ZIO.foreach_(_) { case (b, name) =>
      (b.click *> sendNotification(s"Upgraded $name Storage")).unlessM(b.disabled)
    })
  }

  val buyNeededStorageUpgrades: RIO[Has[UIInterface] with Has[Notifications], Unit] =
    currentPageCardsWithBuyButtons.optional.flatMap {
      case None => ZIO.unit

      case Some(cards) =>
        def collectNotEnoughStorages(card: Card) = for {
          crs <- card.costRows
          names <- ZIO
            .collect(crs) { costRow =>
              ZIO.ifM(costRow.isNotEnoughStorage.asSomeError)(costRow.rowName, ZIO.fail(None))
            }
            .asSomeError
        } yield names.toSet

        ZIO.foreach(cards.toSet)(collectNotEnoughStorages).map(_.flatten).optional.flatMap {
          case Some(wantedRowNames) if wantedRowNames.nonEmpty =>
            for {
              allSideNavs <- sideNavEntries
              wantedSideNavs <- ZIO.filter(allSideNavs) { e =>
                e.navButton.flatMap(_.name).optional.map(_.exists(wantedRowNames.contains))
              }
              _ <- upgradeStorage(onlyUpgradeWhenFull = false)(wantedSideNavs)
            } yield {}

          case _ => ZIO.unit
        }
    }

  def buildAllMachines(o: BuildMachinesOpts = BuildMachinesOpts(true)): RIO[Has[UIInterface], Unit] =
    currentPageCardsWithBuyButtons.optional.flatMap {
      case None => ZIO.unit

      case Some(cards) =>
        def machinePassesFilter(card: Card) =
          ZIO
            .fromOption(o.limitTo.toOption)
            .foldM(_ => ZIO.succeed(true), toBuild => card.name.fold(_ => false, toBuild.contains))

        def buildIfWanted(card: Card, numToLeaveUnbuilt: Int) =
          buildAllFromCard(card, if (o.leaveUnbuilt getOrElse true) numToLeaveUnbuilt else 0)
            .whenM(machinePassesFilter(card))

        def loop(rem: Vector[Card] = cards.reverse, numToLeaveUnbuilt: Int = 1): Task[Unit] =
          ZIO
            .fromOption(rem.headOption)
            .foldM(_ => Task.unit, buildIfWanted(_, numToLeaveUnbuilt) *> loop(rem.tail, 1 + numToLeaveUnbuilt))

        loop()
    }

  private case class BulkBuyInfo(
      name: String,
      bbb: BulkBuyButton,
      currentAmount: Int,
      maxCanBuild: Int,
  ) {

    final lazy val canBulkBuyWithOneRemaining = bbb.buyAmount < (currentAmount + maxCanBuild)
  }

  type ItemName       = String
  type AmountClicked  = Int
  type ActuallyBought = Int
  type OnBulkBuy[R]   = (ItemName, AmountClicked, ActuallyBought) => RIO[R, Unit]

  private def bulkBuyItems[R](items: Iterable[BulkBuyInfo], onBuy: OnBulkBuy[R]) =
    ZIO.foreach_(items.filter(_.canBulkBuyWithOneRemaining))(i =>
      onBuy(i.name, i.bbb.buyAmount, i.bbb.buyAmount - i.currentAmount) *> i.bbb.click
    )

  private def bulkBuyMachines[R](
      onBulkBuy: OnBulkBuy[R]
  )(furtherFilter: (Card) => IO[Option[Throwable], Any] = _ => IO.unit) =
    currentPageCardsWithBuyButtons.optional.flatMap {
      case None => UIO.unit

      case Some(cards) =>
        val infos = ZIO.collect(cards) { c =>
          for {
            n    <- c.name
            bb   <- c.firstBulkBuyButton
            curr <- c.count
            max  <- c.maxCanBuild
            _    <- furtherFilter(c)
          } yield BulkBuyInfo(n, bb, curr.toInt, max.toInt)
        }

        for {
          items <- infos
          _     <- bulkBuyItems(items, onBulkBuy)
        } yield {}
    }

  def buildBulkMachines[R](onBulkBuy: OnBulkBuy[R]): ZIO[R with Has[UIInterface], Throwable, Unit] =
    bulkBuyMachines(onBulkBuy)()

  def buildFreeItems[R](onBulkBuy: OnBulkBuy[R]): ZIO[R with Has[UIInterface], Throwable, Unit] = {
    // if any of them start with '-' instead of '+' then they aren't all free
    def allFree(rows: Vector[ProductionRow]) = ZIO.foreach(rows)(_.inputOrOutput).map(_.forall(_ startsWith "+"))
    def onlyAllowAllFree(rows: Vector[ProductionRow]) = allFree(rows).optional.map(_.filter(identity)).some

    bulkBuyMachines(onBulkBuy) { c =>
      for {
        prs <- c.productionRows
        _   <- onlyAllowAllFree(prs)
      } yield {}
    }

  }

}
