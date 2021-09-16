package autong

import autong.Selectors.{currentPageCardsWithBuyButtons, Card}
import japgolly.scalajs.react.{RT, RTask}
import zio.ZIO

object Buying {

  private def buildAllFromCard(card: Card, numToLeaveUnbuilt: Int) = {
    val currMax = card.costs.flatMap(_.max).optional

    card.buyButtons.optional.flatMap {
      case None => RT

      case Some(btns) =>
        def loop(max: Option[Int]): RTask[Unit] =
          (RT.as(btns.lastOption.foreach(_.click())) *> ZIO.yieldNow *> (currMax >>= loop)
            .when(max.isDefined)).when(max.forall(_ > numToLeaveUnbuilt))

        currMax >>= loop
    }

  }

  def buildAllMachines(o: BuildMachinesOpts = BuildMachinesOpts(true)): RTask[Unit] =
    currentPageCardsWithBuyButtons.optional.flatMap {
      case None => RT

      case Some(cards) =>
        val machinePassesFilter = (card: Card) =>
          o.limitTo.fold[RTask[Boolean]](ZIO.succeed(true)) { toBuild =>
            card.name.optional.map(_.fold(false)(toBuild.contains))
          }

        val buildIfWanted = (card: Card, numToLeaveUnbuilt: Int) =>
          buildAllFromCard(card, if (o.leaveUnbuilt getOrElse true) numToLeaveUnbuilt else 0)
            .whenM(machinePassesFilter(card))

          def loop(rem: Vector[Card] = cards.reverse, numToLeaveUnbuilt: Int = 1): RTask[Unit] =
            rem.headOption.fold(RT) { curr =>
              buildIfWanted(curr, numToLeaveUnbuilt) *> loop(rem.tail, 1 + numToLeaveUnbuilt)
            }

          loop()
    }

}
