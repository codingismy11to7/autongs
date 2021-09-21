package autong

import autong.AutoNGMain.doWork
import autong.TestUIInterface.{createPage, TestButton, TestSideNavButton, TestSideNavEntry}
import zio.test.Assertion._
import zio.test._

object TestAutoStorage extends DefaultRunnableSpec {

  private def runWithOpts(opts: Options) =
    for {
      oilUpgrade   <- TestButton.make()
      metalUpgrade <- TestButton.make(disabled = true)
      gemUpgrade   <- TestButton.make()
      woodUpgrade  <- TestButton.make(disabled = true)
      rescsEntry = TestSideNavEntry(TestSideNavButton("Earth Resources"))
      oilEntry   = TestSideNavEntry(TestSideNavButton("Oil", "1M", "1M"), oilUpgrade)
      metalEntry = TestSideNavEntry(TestSideNavButton("Metal", "1M", "1M"), metalUpgrade)
      gemEntry   = TestSideNavEntry(TestSideNavButton("Gem", "5M", "6M"), gemUpgrade)
      woodEntry  = TestSideNavEntry(TestSideNavButton("Wood", "5M", "6M"), woodUpgrade)
      sideNavs   = Vector(rescsEntry, oilEntry, metalEntry, gemEntry, woodEntry)
      layers     = TestStorage.default ++ TestUIInterface.create(createPage("Lava"), sideNavs)
      _               <- doWork(Options.setDefaults(opts), 0).provideCustomLayer(layers)
      oilClickCount   <- oilUpgrade.clicks
      metalClickCount <- metalUpgrade.clicks
      gemClickCount   <- gemUpgrade.clicks
      woodClickCount  <- woodUpgrade.clicks
    } yield (oilClickCount, metalClickCount, gemClickCount, woodClickCount)

  final val spec = suite("Auto Storage")(
    testM("should do nothing when disabled") {
      runWithOpts(Options(storageEnabled0 = false)).map {
        case (oilClickCount, metalClickCount, gemClickCount, woodClickCount) =>
          assert(oilClickCount)(equalTo(0)) &&
            assert(metalClickCount)(equalTo(0)) &&
            assert(gemClickCount)(equalTo(0)) &&
            assert(woodClickCount)(equalTo(0))
      }
    },
    testM("should work when only upgrade when full is true") {
      runWithOpts(Options(storageEnabled0 = true, onlyUpgradeStorageWhenFull0 = true)).map {
        case (oilClickCount, metalClickCount, gemClickCount, woodClickCount) =>
          assert(oilClickCount)(equalTo(1)) &&
            assert(metalClickCount)(equalTo(0)) &&
            assert(gemClickCount)(equalTo(0)) &&
            assert(woodClickCount)(equalTo(0))
      }
    },
    testM("should work when only upgrade when full is false") {
      runWithOpts(Options(storageEnabled0 = true, onlyUpgradeStorageWhenFull0 = false)).map {
        case (oilClickCount, metalClickCount, gemClickCount, woodClickCount) =>
          assert(oilClickCount)(equalTo(1)) &&
            assert(metalClickCount)(equalTo(0)) &&
            assert(gemClickCount)(equalTo(1)) &&
            assert(woodClickCount)(equalTo(0))
      }
    },
  )

}
