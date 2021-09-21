package autong

import autong.Nav.navToPage
import autong.UIInterface.{currentPageCards, Card}
import zio.clock.Clock
import zio.{Has, RIO, ZIO}

object Technologies {

  private val boostUnlockFromCard = (card: Card) =>
    card.buyButtons.flatMap(ZIO.foreach_(_)(_.click.asSomeError)).optional

  private val boostUnlockAllTechnologies =
    currentPageCards.flatMap(ZIO.foreach_(_)(c => boostUnlockFromCard(c).asSomeError)).optional

  val navAndBoostUnlockAllTechs: RIO[Has[UIInterface] with Clock, Unit] =
    navToPage("Technologies") *> boostUnlockAllTechnologies.unit

}
