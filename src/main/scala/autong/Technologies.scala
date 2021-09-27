package autong

import autong.Nav.navToPage
import autong.UIInterface.{currentPageCards, Card}
import zio._

object Technologies {

  private val boostUnlockFromCard = (card: Card) =>
    card.buyButtons.flatMap(ZIO.foreachDiscard(_)(_.click.asSomeError)).unsome

  private val boostUnlockAllTechnologies =
    currentPageCards.flatMap(ZIO.foreachDiscard(_)(c => boostUnlockFromCard(c).asSomeError)).unsome

  val navAndBoostUnlockAllTechs: RIO[Has[UIInterface] with Has[Clock], Unit] =
    navToPage("Technologies") *> boostUnlockAllTechnologies.unit

}
