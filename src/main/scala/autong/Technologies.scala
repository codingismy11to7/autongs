package autong

import autong.Nav.navToPage
import autong.Selectors.{currentPageCards, Card}
import zio.{RIO, ZIO}
import zio.clock.Clock

object Technologies {

  private val boostUnlockFromCard = (card: Card) =>
    card.buyButtons.flatMap(ZIO.foreach_(_)(_.click.asSomeError)).optional

  private val boostUnlockAllTechnologies =
    currentPageCards.flatMap(ZIO.foreach_(_)(c => boostUnlockFromCard(c).asSomeError)).optional

  val navAndBoostUnlockAllTechs: RIO[Clock, Unit] = navToPage("Technologies") *> boostUnlockAllTechnologies.unit
}
