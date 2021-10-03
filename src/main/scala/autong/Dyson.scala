package autong

import autong.UIInterface.{currentPageCards, Card}
import zio.{Has, ZIO}
import zio.ZIO.ifM

object Dyson {

  private def clickBuy(name: String) = (card: Card) =>
    for {
      b <- card.buyButton(name)
      _ <- b.click.asSomeError
    } yield {}

  private val click250   = clickBuy("= 250")
  private val buySphere  = clickBuy("= 250 + Sphere")
  private val clickRing  = clickBuy("= 50 + Ring")
  private val clickSwarm = clickBuy("= 100 + Swarm")

  def buildDyson(opts: RequiredOptions): ZIO[Has[UIInterface], Option[Throwable], Unit] =
    currentPageCards
      .map(cards => List(cards.headOption, cards.lift(1), cards.lift(2), cards.lift(3)).map(ZIO.fromOption(_)))
      .flatMap {
        case segment :: ring :: swarm :: sphere :: Nil =>
          val sphereBuyButtons = sphere.flatMap(_.buyButtons)

          // if we have a sphere card but it has no buy buttons, we've bought it already
          val spherePurchased =
            sphere.optional.map(_.isDefined) && sphereBuyButtons.optional.map(_.forall(_.isEmpty))

          ifM((ZIO.succeed(opts.buySwarmsAfterSphere) && spherePurchased).asSomeError)(
            swarm >>= clickSwarm,
            ifM(ring.flatMap(_.count).map(_ < opts.ringCount))(
              ring >>= clickRing,
              ifM(swarm.flatMap(_.count).map(_ < opts.swarmCount))(
                swarm >>= clickSwarm,
                ifM(ZIO.succeed(opts.autoBuySphere).asSomeError)(
                  sphere >>= buySphere,
                  (segment >>= click250).unlessM(spherePurchased.asSomeError),
                ),
              ),
            ),
          ).optional.asSomeError.unit

        case _ => ZIO.unit // compiler doesn't know we always have a list of 4 items
      }

}
