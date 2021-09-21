package autong

import autong.UIInterface.{currentPageCardsWithBuyButtons, BulkBuyButton, Card, ProductionRow}
import zio.{Has, RIO, Task, ZIO}

object Buying {

  private def buildAllFromCard(card: Card, numToLeaveUnbuilt: Int) = {
    val currMax = card.costs.flatMap(_.max).optional

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

  private case class FreeBuyInfo(
      name: String,
      bbb: BulkBuyButton,
      currentAmount: Int,
      maxCanBuild: Int,
  )

  val buildFreeItems: RIO[Has[UIInterface], Unit] = {
    // if any of them start with '-' instead of '+' then they aren't all free
    def allFree(rows: Vector[ProductionRow]) = ZIO.foreach(rows)(_.inputOrOutput).map(_.forall(_ startsWith "+"))
    def onlyAllowAllFree(rows: Vector[ProductionRow]) = allFree(rows).optional.map(_.filter(identity)).some

    def canClickBuy(info: FreeBuyInfo) = info.bbb.buyAmount < (info.currentAmount + info.maxCanBuild)

    currentPageCardsWithBuyButtons.optional.flatMap {
      case None => ZIO.unit

      case Some(cards) =>
        val freeItems =
          ZIO.collect(cards) { c =>
            for {
              n     <- c.name
              bb    <- c.firstBulkBuyButton
              curr  <- c.count
              costs <- c.costs
              max   <- costs.max
              prod  <- c.production
              prs   <- prod.productionRows
              _     <- onlyAllowAllFree(prs)
            } yield FreeBuyInfo(n, bb, curr, max)
          }

        for {
          items <- freeItems
          canClick = items.filter(canClickBuy)
          btns     = canClick.map(_.bbb)
          _ <- ZIO.foreach_(btns)(_.click)
        } yield {}
    }
  }

}
