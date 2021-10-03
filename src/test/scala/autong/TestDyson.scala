package autong

import autong.AutoNGMain.doWork
import autong.TestUIInterface._
import autong.UIInterface.Card
import zio.UIO
import zio.test._
import zio.test.Assertion._

import scala.scalajs.js

object TestDyson extends DefaultRunnableSpec {

  private case class ClickCounts(
      eq50: Int = 0,
      eq100: Int = 0,
      eq250: Int = 0,
      plusOne: Int = 0,
      eq50PlusRing: Int = 0,
      ringBuild: Int = 0,
      eq100PlusSwarm: Int = 0,
      swarmBuild: Int = 0,
      eq250PlusSphere: Int = 0,
      sphereBuild: Int = 0,
  )

  private case class DysonCards(
      cards: Vector[Card],
      eq50: TestClickCountButton,
      eq100: TestClickCountButton,
      eq250: TestClickCountButton,
      plusOne: TestClickCountButton,
      eq50PlusRing: TestClickCountButton,
      ringBuild: TestClickCountButton,
      eq100PlusSwarm: TestClickCountButton,
      swarmBuild: TestClickCountButton,
      eq250PlusSphere: TestClickCountButton,
      sphereBuild: TestClickCountButton,
  ) {

    val toClickCounts: UIO[ClickCounts] = for {
      eq50C            <- eq50.clicks
      eq100C           <- eq100.clicks
      eq250C           <- eq250.clicks
      plusOneC         <- plusOne.clicks
      eq50PlusRingC    <- eq50PlusRing.clicks
      ringBuildC       <- ringBuild.clicks
      eq100PlusSwarmC  <- eq100PlusSwarm.clicks
      swarmBuildC      <- swarmBuild.clicks
      eq250PlusSphereC <- eq250PlusSphere.clicks
      sphereBuildC     <- sphereBuild.clicks
    } yield ClickCounts(
      eq50C,
      eq100C,
      eq250C,
      plusOneC,
      eq50PlusRingC,
      ringBuildC,
      eq100PlusSwarmC,
      swarmBuildC,
      eq250PlusSphereC,
      sphereBuildC,
    )

  }

  private def createCards(
      ringCount: Int,
      swarmCount: Int,
      spherePurchased: Boolean,
      ringOnly: Boolean,
  ) = for {
    eq50            <- TestClickCountButton.make("= 50")
    eq100           <- TestClickCountButton.make("= 100")
    eq250           <- TestClickCountButton.make("= 250")
    plusOne         <- TestClickCountButton.make("+ 1")
    eq50PlusRing    <- TestClickCountButton.make("= 50 + Ring")
    ringBuild       <- TestClickCountButton.make("Build")
    eq100PlusSwarm  <- TestClickCountButton.make("= 100 + Swarm")
    swarmBuild      <- TestClickCountButton.make("Build")
    eq250PlusSphere <- TestClickCountButton.make("= 250 + Sphere")
    sphereBuild     <- TestClickCountButton.make("Build")
  } yield {
    val baseCards = Vector(
      TestCard(
        "Segment",
        25,
        Vector(TestSection("Costs"), TestBuyButtons(Vector(eq50, eq100, eq250, plusOne))),
        150,
        Vector("Titanium", "Gold", "Silicon", "Meteorite", "Ice").map(TestCostRow(_)),
      ),
      TestCard(
        "Ring",
        ringCount,
        Vector(
          TestSection("Production"),
          TestSection("Costs"),
          TestBuyButtons(Vector(eq50PlusRing, ringBuild)),
        ),
        js.undefined,
        Vector("Segment", "Fuel").map(TestCostRow(_)),
        Vector(TestProdRow("Energy", "+14.11K")),
      ),
    )

    def swarmAndSphere = Vector(
      TestCard(
        "Swarm",
        swarmCount,
        Vector(
          TestSection("Production"),
          TestSection("Costs"),
          TestBuyButtons(Vector(eq100PlusSwarm, swarmBuild)),
        ),
        js.undefined,
        Vector("Segment", "Fuel").map(TestCostRow(_)),
        Vector(TestProdRow("Energy", "+70.53K")),
      ),
      if (spherePurchased) TestCard("Sphere")
      else
        TestCard(
          "Sphere",
          js.undefined,
          Vector(
            TestSection("Production"),
            TestSection("Costs"),
            TestBuyButtons(Vector(eq250PlusSphere, sphereBuild)),
          ),
          js.undefined,
          Vector("Segment", "Fuel").map(TestCostRow(_)),
          Vector(TestProdRow("Energy", "+2.821M")),
        ),
    )

    val cards = if (ringOnly) baseCards else baseCards ++ swarmAndSphere

    DysonCards(
      cards,
      eq50,
      eq100,
      eq250,
      plusOne,
      eq50PlusRing,
      ringBuild,
      eq100PlusSwarm,
      swarmBuild,
      eq250PlusSphere,
      sphereBuild,
    )
  }

  private def runWork(
      opts: Options,
      ringCount: Int = 5,
      swarmCount: Int = 6,
      spherePurchased: Boolean = true,
      ringOnly: Boolean = false,
  ) = for {
    dysonCards <- createCards(ringCount, swarmCount, spherePurchased, ringOnly)
    page  = createPage("Dyson", dysonCards.cards)
    layer = create(page) ++ TestNotifications.empty
    _      <- doWork(Options setDefaults opts, 0).provideCustomLayer(layer)
    counts <- dysonCards.toClickCounts
  } yield counts

  private def opts(
      buySwarmsAfterSphere: Boolean = false,
      ringCount: Int = 5,
      swarmCount: Int = 6,
      autoBuySphere: Boolean = false,
      autoDyson: Boolean = true,
  ) =
    Options(
      autoDyson0 = autoDyson,
      buySwarmsAfterSphere0 = buySwarmsAfterSphere,
      ringCount0 = ringCount,
      swarmCount0 = swarmCount,
      autoBuySphere0 = autoBuySphere,
    )

  final val spec = suite("Dyson Automation")(
    suite("should buy the right things")(
      testM("when done and only ring unlocked") {
        runWork(opts(), ringOnly = true).map(assert(_)(equalTo(ClickCounts())))
      },
      testM("when not done and only ring unlocked") {
        runWork(opts(), ringOnly = true, ringCount = 4).map(assert(_)(equalTo(ClickCounts(eq50PlusRing = 1))))
      },
      testM("when not done and only ring unlocked and auto-dyson disabled") {
        runWork(opts(autoDyson = false), ringOnly = true, ringCount = 4).map(assert(_)(equalTo(ClickCounts())))
      },
      testM("when done and not buying swarms after sphere") {
        runWork(opts()).map(assert(_)(equalTo(ClickCounts())))
      },
      testM("when done and buying swarms after sphere") {
        runWork(opts(buySwarmsAfterSphere = true)).map(assert(_)(equalTo(ClickCounts(eq100PlusSwarm = 1))))
      },
      testM("when done and buying swarms after sphere and auto-dyson disabled") {
        runWork(opts(buySwarmsAfterSphere = true, autoDyson = false)).map(assert(_)(equalTo(ClickCounts())))
      },
      testM("when sphere unpurchased and not auto-purchasing") {
        runWork(opts(), spherePurchased = false).map(assert(_)(equalTo(ClickCounts(eq250 = 1))))
      },
      testM("when sphere unpurchased and not auto-purchasing and auto-dyson disabled") {
        runWork(opts(autoDyson = false), spherePurchased = false).map(assert(_)(equalTo(ClickCounts())))
      },
      testM("when sphere unpurchased and auto-purchasing") {
        runWork(opts(autoBuySphere = true), spherePurchased = false)
          .map(assert(_)(equalTo(ClickCounts(eq250PlusSphere = 1))))
      },
      testM("when sphere unpurchased and auto-purchasing and auto-dyson disabled") {
        runWork(opts(autoBuySphere = true, autoDyson = false), spherePurchased = false)
          .map(assert(_)(equalTo(ClickCounts())))
      },
      testM("when not done with rings or swarms or sphere") {
        runWork(opts(), ringCount = 1, swarmCount = 0, spherePurchased = false).map(
          assert(_)(equalTo(ClickCounts(eq50PlusRing = 1)))
        )
      },
      testM("when not done with rings or swarms or sphere and auto-dyson disabled") {
        runWork(opts(autoDyson = false), ringCount = 1, swarmCount = 0, spherePurchased = false).map(
          assert(_)(equalTo(ClickCounts()))
        )
      },
      testM("when not done with swarms or sphere") {
        runWork(opts(), swarmCount = 1, spherePurchased = false).map(
          assert(_)(equalTo(ClickCounts(eq100PlusSwarm = 1)))
        )
      },
      testM("when not done with swarms or sphere and auto-dyson disabled") {
        runWork(opts(autoDyson = false), swarmCount = 1, spherePurchased = false).map(
          assert(_)(equalTo(ClickCounts()))
        )
      },
    )
  )

}
