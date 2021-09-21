package autong

import autong.UIInterface._
import autong.Utils.RichStr
import zio._

import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichOption

object TestUIInterface {

  def create(page: Page, sideNavs: Vector[SideNavEntry] = Vector.empty): ULayer[Has[UIInterface]] = ZLayer.succeed {
    new UIInterface {
      override def currentPage: ZIO[Any, Option[Throwable], Page] = ZIO.succeed(page).asSomeError

      override def sideNavEntries: Task[Vector[SideNavEntry]] = ZIO.succeed(sideNavs)
    }
  }

  implicit class RichCard(val card: Card) extends AnyVal {

    def toDynCard: IO[Option[Throwable], DynCard] = for {
      n  <- card.name
      c  <- card.count
      m  <- card.costs.flatMap(_.max)
      cr <- card.costs.flatMap(_.costRows).optional.asSomeError
      pr <- card.production.flatMap(_.productionRows).optional.asSomeError
    } yield DynCard(n, c, m, cr.orUndefined, pr.orUndefined)

  }

  case class TestClickCountButton(clickCount: Ref[Int], name: Option[String] = None, _disabled: Boolean = false)
      extends Button {
    val click: Task[Unit]       = clickCount.update(_ + 1)
    val disabled: Task[Boolean] = Task(_disabled)

    val clicks: UIO[Int] = clickCount.get
  }

  object TestClickCountButton {

    def make(name: js.UndefOr[String] = js.undefined, disabled: Boolean = false): UIO[TestClickCountButton] = for {
      r <- Ref.make(0)
    } yield TestClickCountButton(r, name.toOption, disabled)

  }

  case class TestClickActionButton(
      _name: js.UndefOr[String] = js.undefined,
      _disabled: Boolean = false,
      onClick: Task[Unit] = Task.unit,
  ) extends Button {
    val name: Option[String]    = _name.toOption
    val click: Task[Unit]       = onClick
    val disabled: Task[Boolean] = Task.succeed(_disabled)
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

  case class DynamicStandardCard(
      stateRef: Ref[DynCard],
      costRows: js.UndefOr[Vector[CostRow]] = js.undefined,
      productionRows: js.UndefOr[Vector[ProductionRow]] = js.undefined,
  ) extends Card {
    val name: IO[Option[Throwable], String] = stateRef.map(_.name).get
    val count: IO[Option[Throwable], Int]   = stateRef.map(_.count).get

    val sections: Task[Vector[Section]] = stateRef.get.map { dc =>
      def clickAction(num: Int) = stateRef
        .update(dc => dc.copy(count = dc.count + num, max = dc.max - num))
        .whenM(stateRef.map(_.max).get.map(_ >= num))

      def eqBtn(num: Int) = if (dc.count < num)
        Some(TestClickActionButton(s"= $num", onClick = clickAction(num)))
      else None

      Vector(
        TestSection("Production", _productionRows = productionRows),
        TestSection("Costs", dc.max, _costRows = costRows),
        TestSection(_buyButtons =
          Vector(
            eqBtn(5),
            eqBtn(25),
            eqBtn(75),
            eqBtn(150),
            eqBtn(250),
            Some(TestClickActionButton("+ 1", onClick = clickAction(1))),
          ).flatten
        ),
      )
    }

  }

  object DynamicStandardCard {

    def make(
        dc: DynCard
    ): UIO[DynamicStandardCard] = for {
      ref <- Ref.make(dc)
    } yield DynamicStandardCard(ref, dc.costRows, dc.productionRows)

  }

  def createPage(name: String, _cards: Vector[Card] = Vector.empty): Page = new Page {
    val pageName: IO[Option[Throwable], String]    = ZIO.succeed(name).asSomeError
    val cards: IO[Option[Throwable], Vector[Card]] = ZIO.succeed(_cards).asSomeError
  }

  case class DynCard(
      name: String,
      count: Int = 0,
      max: Int = 0,
      costRows: js.UndefOr[Vector[CostRow]] = js.undefined,
      productionRows: js.UndefOr[Vector[ProductionRow]] = js.undefined,
  )

  def createDynamicPage(name: String, prefixCards: Vector[Card], dynCards: Vector[DynCard]): UIO[Page] = for {
    dynStdCards <- ZIO.foreach(dynCards)(DynamicStandardCard.make)
  } yield new Page {
    val pageName: IO[Option[Throwable], String]    = ZIO.fromOption(name.toOption)
    val cards: IO[Option[Throwable], Vector[Card]] = UIO.succeed(prefixCards ++ dynStdCards)
  }

}
