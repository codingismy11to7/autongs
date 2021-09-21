package autong

import autong.AutoNGMain.doWork
import autong.UIInterface.{Button, Card, Page, SideNavButton, SideNavEntry}
import zio.{IO, Ref, Task, ZIO}
import zio.test.Assertion._
import zio.test._

import scala.scalajs.js

object TestAutoStorage extends DefaultRunnableSpec {

  case class TestButton(clickCount: Ref[Int], name: Option[String] = None, _disabled: Boolean = false) extends Button {
    val click: Task[Unit]       = clickCount.update(_ + 1)
    val disabled: Task[Boolean] = Task(_disabled)
  }

  case class TestSideNavButton(
      _name: String,
      _amountStored: js.UndefOr[String] = js.undefined,
      _totalStorage: js.UndefOr[String] = js.undefined,
      click: Task[Unit] = Task.unit,
  ) extends SideNavButton {
    val name: IO[Option[Throwable], String]         = ZIO.succeed(_name).asSomeError
    val amountStored: IO[Option[Throwable], String] = ZIO.fromOption(_amountStored.toOption)
    val totalStorage: IO[Option[Throwable], String] = ZIO.fromOption(_totalStorage.toOption)
  }

  case class TestSideNavEntry(navBtn: TestSideNavButton, upgBtn: js.UndefOr[TestButton] = js.undefined)
      extends SideNavEntry {
    val navButton: IO[Option[Throwable], SideNavButton] = ZIO.succeed(navBtn).asSomeError
    val upgradeButton: IO[Option[Throwable], Button]    = ZIO.fromOption(upgBtn.toOption)
  }

  private def createPage(name: String, _cards: Vector[Card] = Vector.empty) = new Page {
    val pageName: IO[Option[Throwable], String]    = ZIO.succeed(name).asSomeError
    val cards: IO[Option[Throwable], Vector[Card]] = ZIO.succeed(_cards).asSomeError
  }

  private def runWithOpts(opts: Options) =
    for {
      oilClickRef   <- Ref.make(0)
      metalClickRef <- Ref.make(0)
      gemClickRef   <- Ref.make(0)
      woodClickRef  <- Ref.make(0)
      rescsEntry = TestSideNavEntry(TestSideNavButton("Earth Resources"))
      oilEntry   = TestSideNavEntry(TestSideNavButton("Oil", "1M", "1M"), TestButton(oilClickRef))
      metalEntry = TestSideNavEntry(TestSideNavButton("Metal", "1M", "1M"), TestButton(metalClickRef, _disabled = true))
      gemEntry   = TestSideNavEntry(TestSideNavButton("Gem", "5M", "6M"), TestButton(gemClickRef))
      woodEntry  = TestSideNavEntry(TestSideNavButton("Wood", "5M", "6M"), TestButton(woodClickRef, _disabled = true))
      sideNavs   = Vector(rescsEntry, oilEntry, metalEntry, gemEntry, woodEntry)
      layers     = TestStorage.default ++ TestUIInterface.create(createPage("Lava"), sideNavs)
      _               <- doWork(Options.setDefaults(opts), 0).provideCustomLayer(layers)
      oilClickCount   <- oilClickRef.get
      metalClickCount <- metalClickRef.get
      gemClickCount   <- gemClickRef.get
      woodClickCount  <- woodClickRef.get
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
