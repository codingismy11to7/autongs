package autong

import autong.UIInterface._
import zio._

import scala.scalajs.js

object TestUIInterface {

  def create(page: Page, sideNavs: Vector[SideNavEntry]): ULayer[Has[UIInterface]] = ZLayer.succeed {
    new UIInterface {
      override def currentPage: ZIO[Any, Option[Throwable], Page] = ZIO.succeed(page).asSomeError

      override def sideNavEntries: Task[Vector[SideNavEntry]] = ZIO.succeed(sideNavs)
    }
  }

  case class TestButton(clickCount: Ref[Int], name: Option[String] = None, _disabled: Boolean = false) extends Button {
    val click: Task[Unit]       = clickCount.update(_ + 1)
    val disabled: Task[Boolean] = Task(_disabled)

    val clicks: UIO[Int] = clickCount.get
  }

  object TestButton {

    def make(name: js.UndefOr[String] = js.undefined, disabled: Boolean = false): UIO[TestButton] = for {
      r <- Ref.make(0)
    } yield TestButton(r, name.toOption, disabled)

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

  def createPage(name: String, _cards: Vector[Card] = Vector.empty): Page = new Page {
    val pageName: IO[Option[Throwable], String]    = ZIO.succeed(name).asSomeError
    val cards: IO[Option[Throwable], Vector[Card]] = ZIO.succeed(_cards).asSomeError
  }

}
