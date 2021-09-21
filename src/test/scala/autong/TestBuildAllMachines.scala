package autong

import autong.Buying.buildAllMachines
import autong.TestUIInterface.{createPage, TestButton, TestCard, TestSection}
import zio.test._
import zio.test.Assertion._

object TestBuildAllMachines extends DefaultRunnableSpec {

  private def noMax(opts: BuildMachinesOpts) = for {
    m1Build <- TestButton.make("Build")
    m2Build <- TestButton.make("Boost")
    m3Build <- TestButton.make("+ 1")
    m4Build <- TestButton.make("Build")
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
    )
  )

}
