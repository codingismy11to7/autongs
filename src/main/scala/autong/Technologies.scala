package autong

import autong.Nav.navToPage
import autong.Selectors.{currentPageCards, Card}
import zio.{RIO, ZIO}
import zio.clock.Clock

object Technologies {

  private val boostUnlockFromCard = (card: Card) =>
    card.buyButtons.optional.map(_.foreach(_.foreach(_.click()))).asSomeError

  private val boostUnlockAllTechnologies = currentPageCards.flatMap(ZIO.foreach_(_)(boostUnlockFromCard)).optional.unit

  val navAndBoostUnlockAllTechs: RIO[Clock, Unit] = navToPage("Technologies") *> boostUnlockAllTechnologies
}
