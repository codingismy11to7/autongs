package autong

import autong.UIInterface._
import autong.Utils.RichStr
import zio._

import scala.scalajs.js

object TestUIInterface {

  def create(page: Page, sideNavs: Vector[SideNavEntry] = Vector.empty): ULayer[Has[UIInterface]] = ZLayer.succeed {
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

  case class TestSideNavEntry(navBtn: SideNavButton, upgBtn: js.UndefOr[Button] = js.undefined) extends SideNavEntry {
    val navButton: IO[Option[Throwable], SideNavButton] = ZIO.succeed(navBtn).asSomeError
    val upgradeButton: IO[Option[Throwable], Button]    = ZIO.fromOption(upgBtn.toOption)
  }

  case class TestCostRow(
      _rowName: String,
      _emcButton: js.UndefOr[Button] = js.undefined,
      remainingTime: js.UndefOr[String] = js.undefined,
  ) extends CostRow {
    val rowName: IO[Option[Throwable], String]       = ZIO.fromOption(_rowName.toOption)
    val emcButton: IO[Option[Throwable], Button]     = ZIO.fromOption(_emcButton.toOption)
    val timeRemaining: IO[Option[Throwable], String] = ZIO.fromOption(remainingTime.toOption)
  }

  case class TestProdRow(_rowName: String, _inputOrOutput: String) extends ProductionRow {
    val rowName: IO[Option[Throwable], String]       = ZIO.fromOption(_rowName.toOption)
    val inputOrOutput: IO[Option[Throwable], String] = ZIO.fromOption(_inputOrOutput.toOption)
  }

  case class TestSection(
      _title: js.UndefOr[String] = js.undefined,
      _max: js.UndefOr[Int] = js.undefined,
      _buyButtons: Vector[Button] = Vector.empty,
      _costRows: js.UndefOr[Vector[CostRow]] = js.undefined,
      _productionRows: js.UndefOr[Vector[ProductionRow]] = js.undefined,
  ) extends Section {
    val title: IO[Option[Throwable], String]                         = ZIO.fromOption(_title.toOption)
    val max: IO[Option[Throwable], Int]                              = ZIO.fromOption(_max.toOption)
    val allBuyButtons: Task[Vector[Button]]                          = Task.succeed(_buyButtons)
    val costRows: IO[Option[Throwable], Vector[CostRow]]             = ZIO.fromOption(_costRows.toOption)
    val productionRows: IO[Option[Throwable], Vector[ProductionRow]] = ZIO.fromOption(_productionRows.toOption)
  }

  case class TestCard(
      _name: js.UndefOr[String] = js.undefined,
      _count: js.UndefOr[Int] = js.undefined,
      _sections: Vector[Section] = Vector.empty,
  ) extends Card {
    val name: IO[Option[Throwable], String] = ZIO.fromOption(_name.toOption)
    val count: IO[Option[Throwable], Int]   = ZIO.fromOption(_count.toOption)
    val sections: Task[Vector[Section]]     = Task.succeed(_sections)
  }

  def createPage(name: String, _cards: Vector[Card] = Vector.empty): Page = new Page {
    val pageName: IO[Option[Throwable], String]    = ZIO.succeed(name).asSomeError
    val cards: IO[Option[Throwable], Vector[Card]] = ZIO.succeed(_cards).asSomeError
  }

}
