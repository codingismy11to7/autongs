package autong

import autong.UIInterface.{currentPageCardsWithBuyButtons, BulkBuyButton, Card, ProductionRow}
import zio._

object Buying {

  private def buildAllFromCard(card: Card, numToLeaveUnbuilt: Int) = {
    val currMax = card.maxCanBuild.optional

    card.lastBuyButton.optional.flatMap {
      case None => ZIO.unit

      case Some(btn) =>
        def loop(max: Option[Int]): Task[Unit] =
          (btn.click *> ZIO.yieldNow *> (currMax >>= loop).when(max.isDefined))
            .when(max.forall(_ > numToLeaveUnbuilt))

        currMax >>= loop
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
  type OnBulkBuy      = (ItemName, AmountClicked, ActuallyBought) => RIO[ZEnv, Unit]

  private def bulkBuyItems(items: Iterable[BulkBuyInfo], onBuy: OnBulkBuy) =
    ZIO.foreach_(items.filter(_.canBulkBuyWithOneRemaining))(i =>
      onBuy(i.name, i.bbb.buyAmount, i.bbb.buyAmount - i.currentAmount) *> i.bbb.click
    )

  private def bulkBuyMachines(
      onBulkBuy: OnBulkBuy
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
          } yield BulkBuyInfo(n, bb, curr, max)
        }

        for {
          items <- infos
          _     <- bulkBuyItems(items, onBulkBuy)
        } yield {}
    }

  def buildBulkMachines(onBulkBuy: OnBulkBuy): RIO[Has[UIInterface] with ZEnv, Unit] = bulkBuyMachines(onBulkBuy)()

  def buildFreeItems(onBulkBuy: OnBulkBuy): RIO[Has[UIInterface] with ZEnv, Unit] = {
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
