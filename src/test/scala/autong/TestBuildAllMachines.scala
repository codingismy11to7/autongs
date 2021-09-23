package autong

import autong.Buying.{buildAllMachines, buildBulkMachines, buildFreeItems}
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
    c1    = TestCard("Machine1", 5, Vector(TestBuyButtons(Vector(m1Build))))
    c2    = TestCard("Machine2", 5, Vector(TestBuyButtons(Vector(m2Build))))
    c3    = TestCard("Machine3", 5, Vector(TestBuyButtons(Vector(m3Build))))
    c4    = TestCard("", 5, Vector(TestBuyButtons(Vector(m4Build))))
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

  private val buyFreeItemCards = Vector(
    DynCard("Free1", 3, 2),
    DynCard("Free2", 3, 3),
    DynCard("Free3", 175, 100),
    DynCard("Free4", 24, 80),
    DynCard("NonFree", 1, 100, productionRows = Vector(TestProdRow("Energy", "-1"))),
  ).map(c => c.copy(productionRows = TestProdRow("Resource", "+10") +: c.productionRows.getOrElse(Vector.empty)))

  private def testBuildFree(runTwice: Boolean = false) = for {
    gain20  <- TestClickCountButton.make("Gain 20")
    upgrade <- TestClickCountButton.make("Upgrade")
    page <- createDynamicPage(
      "Resource",
      Vector(
        TestCard("Overview", _sections = Vector(TestBuyButtons(Vector(gain20)))),
        TestCard("Upgrade Storage", 20, _sections = Vector(TestBuyButtons(Vector(upgrade)))),
      ),
      buyFreeItemCards,
    )
    layer = TestUIInterface.create(page)
    run   = buildFreeItems.provideCustomLayer(layer)
    _             <- run
    _             <- run.when(runTwice)
    cards         <- page.cards.optional.map(_.getOrElse(Vector.empty))
    dcs           <- ZIO.collect(cards)(_.toDynCard)
    gain20Clicks  <- gain20.clicks
    upgradeClicks <- upgrade.clicks
  } yield (gain20Clicks, upgradeClicks, dcs)

  private val bulkBuyMachinesCards = Vector(
    DynCard("Small Pump", 150, 5),
    DynCard("Pumpjack", 16, 113),
    DynCard("Oil Field", 1, 71),
    DynCard("Offshore Rig", 0, 57),
    DynCard("Fossilator 9000", 1, 22),
  )

  private def testBulkBuyMachines(runTwice: Boolean = false) = for {
    gain20  <- TestClickCountButton.make("Gain 20")
    upgrade <- TestClickCountButton.make("Upgrade")
    page <- createDynamicPage(
      "Oil",
      Vector(
        TestCard("Overview", _sections = Vector(TestSection("Total"), TestBuyButtons(Vector(gain20)))),
        TestCard(
          "Upgrade Storage",
          19,
          _sections = Vector(TestSection("Costs"), TestBuyButtons(Vector(upgrade))),
        ),
      ),
      bulkBuyMachinesCards,
    )
    layer = TestUIInterface.create(page)
    run   = buildBulkMachines.provideCustomLayer(layer)
    _             <- run
    _             <- run.when(runTwice)
    cards         <- page.cards.optional.map(_.getOrElse(Vector.empty))
    dcs           <- ZIO.collect(cards)(_.toDynCard)
    gain20Clicks  <- gain20.clicks
    upgradeClicks <- upgrade.clicks
  } yield (gain20Clicks, upgradeClicks, dcs)

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
    suite("buy free items")(
      testM("should work") {
        testBuildFree().map { case (gain20Clicks, upgradeClicks, dcs) =>
          assert(gain20Clicks)(equalTo(0)) &&
            assert(upgradeClicks)(equalTo(0)) &&
            assert(dcs(0))(equalTo(buyFreeItemCards(0))) &&
            assert(dcs(1))(equalTo(buyFreeItemCards(1).copy(count = 5, max = 1))) &&
            assert(dcs(2))(equalTo(buyFreeItemCards(2).copy(count = 250, max = 25))) &&
            assert(dcs(3))(equalTo(buyFreeItemCards(3).copy(count = 25, max = 79))) &&
            assert(dcs(4))(equalTo(buyFreeItemCards(4)))
        }
      },
      testM("should work when run again") {
        testBuildFree(true).map { case (gain20Clicks, upgradeClicks, dcs) =>
          assert(gain20Clicks)(equalTo(0)) &&
            assert(upgradeClicks)(equalTo(0)) &&
            assert(dcs(0))(equalTo(buyFreeItemCards(0))) &&
            assert(dcs(1))(equalTo(buyFreeItemCards(1).copy(count = 5, max = 1))) &&
            assert(dcs(2))(equalTo(buyFreeItemCards(2).copy(count = 250, max = 25))) &&
            assert(dcs(3))(equalTo(buyFreeItemCards(3).copy(count = 75, max = 29))) &&
            assert(dcs(4))(equalTo(buyFreeItemCards(4)))
        }
      },
    ),
    suite("bulk buy items")(
      testM("should work") {
        testBulkBuyMachines().map { case (gain20Clicks, upgradeClicks, dcs) =>
          assert(gain20Clicks)(equalTo(0)) &&
            assert(upgradeClicks)(equalTo(0)) &&
            assert(dcs(0))(equalTo(bulkBuyMachinesCards(0))) &&
            assert(dcs(1))(equalTo(bulkBuyMachinesCards(1).copy(count = 25, max = 104))) &&
            assert(dcs(2))(equalTo(bulkBuyMachinesCards(2).copy(count = 5, max = 67))) &&
            assert(dcs(3))(equalTo(bulkBuyMachinesCards(3).copy(count = 5, max = 52))) &&
            assert(dcs(4))(equalTo(bulkBuyMachinesCards(4).copy(count = 5, max = 18)))
        }
      },
      testM("should work when run again") {
        testBulkBuyMachines(true).map { case (gain20Clicks, upgradeClicks, dcs) =>
          assert(gain20Clicks)(equalTo(0)) &&
            assert(upgradeClicks)(equalTo(0)) &&
            assert(dcs(0))(equalTo(bulkBuyMachinesCards(0))) &&
            assert(dcs(1))(equalTo(bulkBuyMachinesCards(1).copy(count = 75, max = 54))) &&
            assert(dcs(2))(equalTo(bulkBuyMachinesCards(2).copy(count = 25, max = 47))) &&
            assert(dcs(3))(equalTo(bulkBuyMachinesCards(3).copy(count = 25, max = 32))) &&
            assert(dcs(4))(equalTo(bulkBuyMachinesCards(4).copy(count = 5, max = 18)))
        }

      },
    ),
  )

}
