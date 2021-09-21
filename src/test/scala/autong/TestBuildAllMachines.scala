package autong

import autong.Buying.buildAllMachines
import autong.TestUIInterface._
import zio.ZIO
import zio.test.Assertion._
import zio.test._

object TestBuildAllMachines extends DefaultRunnableSpec {

  private def noMax(opts: BuildMachinesOpts) = for {
    m1Build <- TestClickCountButton.make("Build")
    m2Build <- TestClickCountButton.make("Boost")
    m3Build <- TestClickCountButton.make("+ 1")
    m4Build <- TestClickCountButton.make("Build")
    c1    = TestCard("Machine1", 5, Vector(TestSection(_buyButtons = Vector(m1Build))))
    c2    = TestCard("Machine2", 5, Vector(TestSection(_buyButtons = Vector(m2Build))))
    c3    = TestCard("Machine3", 5, Vector(TestSection(_buyButtons = Vector(m3Build))))
    c4    = TestCard("", 5, Vector(TestSection(_buyButtons = Vector(m4Build))))
    cards = Vector(c1, c2, c3, c4)
    page  = createPage("Machines", cards)
    layer = TestUIInterface.create(page)
    _       <- buildAllMachines(opts).provideCustomLayer(layer)
    m1Count <- m1Build.clicks
    m2Count <- m2Build.clicks
    m3Count <- m3Build.clicks
    m4Count <- m4Build.clicks
  } yield (m1Count, m2Count, m3Count, m4Count)

  private def max(opts: BuildMachinesOpts) = for {
    page <- createDynamicPage(
      "Titanium",
      Vector.empty,
      Vector(
        DynCard("Explorer", 75, 40),
        DynCard("Lunarite Drill", 75, 38),
        DynCard("Penta-Drill", 75, 10),
        DynCard("Drill of Titans", 73, 3),
      ),
    )
    layer = TestUIInterface.create(page)
    _     <- buildAllMachines(opts).provideCustomLayer(layer)
    cards <- page.cards.optional.map(_.getOrElse(Vector.empty))
    dcs   <- ZIO.collect(cards)(_.toDynCard)
  } yield dcs

  final val spec = suite("Build All Machines")(
    suite("should work without max on machines")(
      testM("with try not to buy true") {
        noMax(BuildMachinesOpts(true)).map { case (m1Count, m2Count, m3Count, m4Count) =>
          assert(m1Count)(equalTo(1)) &&
            assert(m2Count)(equalTo(1)) &&
            assert(m3Count)(equalTo(1)) &&
            assert(m4Count)(equalTo(1))
        }
      },
      testM("with try not to buy false") {
        noMax(BuildMachinesOpts(false)).map { case (m1Count, m2Count, m3Count, m4Count) =>
          assert(m1Count)(equalTo(1)) &&
            assert(m2Count)(equalTo(1)) &&
            assert(m3Count)(equalTo(1)) &&
            assert(m4Count)(equalTo(1))
        }
      },
      testM("with a filter") {
        noMax(BuildMachinesOpts(true, Set("Machine1", "Machine3", "Machine4"))).map {
          case (m1Count, m2Count, m3Count, m4Count) =>
            assert(m1Count)(equalTo(1)) &&
              assert(m2Count)(equalTo(0)) &&
              assert(m3Count)(equalTo(1)) &&
              assert(m4Count)(equalTo(0))
        }
      },
    ),
    suite("should work with max on machines")(
      testM("with try not to buy true") {
        max(BuildMachinesOpts(true)).map(dcs =>
          assert(dcs)(
            equalTo(
              Vector(
                DynCard("Explorer", 111, 4),
                DynCard("Lunarite Drill", 110, 3),
                DynCard("Penta-Drill", 83, 2),
                DynCard("Drill of Titans", 75, 1),
              )
            )
          )
        )
      },
      testM("with try not to buy false") {
        max(BuildMachinesOpts(false)).map(dcs =>
          assert(dcs)(
            equalTo(
              Vector(
                DynCard("Explorer", 115),
                DynCard("Lunarite Drill", 113),
                DynCard("Penta-Drill", 85),
                DynCard("Drill of Titans", 76),
              )
            )
          )
        )
      },
      testM("with a filter") {
        max(BuildMachinesOpts(true, Set("Explorer", "Penta-Drill"))).map(dcs =>
          assert(dcs)(
            equalTo(
              Vector(
                DynCard("Explorer", 111, 4),
                DynCard("Lunarite Drill", 75, 38),
                DynCard("Penta-Drill", 83, 2),
                DynCard("Drill of Titans", 73, 3),
              )
            )
          )
        )
      },
    ),
  )

}
