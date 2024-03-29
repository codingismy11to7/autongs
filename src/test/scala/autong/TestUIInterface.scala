package autong

import autong.UIInterface._
import autong.Utils.RichStr
import zio._
import zio.test._

import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichOption

object TestUIInterface extends DefaultRunnableSpec {

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
      m  <- card.maxCanBuild
      cr <- card.costRows.optional.asSomeError
      pr <- card.productionRows.optional.asSomeError
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
      _isNotEnoughStorage: Boolean = false,
  ) extends CostRow {
    val rowName: IO[Option[Throwable], String]       = ZIO.fromOption(_rowName.toOption)
    val emcButton: IO[Option[Throwable], Button]     = ZIO.fromOption(_emcButton.toOption)
    val timeRemaining: IO[Option[Throwable], String] = ZIO.fromOption(remainingTime.toOption)
    override val isNotEnoughStorage: Task[Boolean]   = ZIO.succeed(_isNotEnoughStorage)
  }

  case class TestProdRow(_rowName: String, _inputOrOutput: String) extends ProductionRow {
    val rowName: IO[Option[Throwable], String]       = ZIO.fromOption(_rowName.toOption)
    val inputOrOutput: IO[Option[Throwable], String] = ZIO.fromOption(_inputOrOutput.toOption)
  }

  case class TestSection(
      _title: js.UndefOr[String] = js.undefined
  ) extends Section {
    val title: IO[Option[Throwable], String] = ZIO.fromOption(_title.toOption)
    val allBuyButtons: Task[Vector[Button]]  = Task.succeed(Vector.empty)
  }

  case class TestBuyButtons(_buyButtons: Vector[Button]) extends Section {
    val title: IO[Option[Throwable], String] = ZIO.fromOption(None)
    val allBuyButtons: Task[Vector[Button]]  = Task.succeed(_buyButtons)
  }

  case class TestCard(
      _name: js.UndefOr[String] = js.undefined,
      _count: js.UndefOr[Int] = js.undefined,
      _sections: Vector[Section] = Vector.empty,
      max: js.UndefOr[Int] = js.undefined,
      _costRows: js.UndefOr[Vector[CostRow]] = js.undefined,
      _prodRows: js.UndefOr[Vector[ProductionRow]] = js.undefined,
  ) extends Card {
    val name: IO[Option[Throwable], String]              = ZIO.fromOption(_name.toOption)
    val count: IO[Option[Throwable], BigDecimal]         = ZIO.fromOption(_count.map(BigDecimal(_)).toOption)
    val sections: Task[Vector[Section]]                  = Task.succeed(_sections)
    val maxCanBuild: IO[Option[Throwable], BigDecimal]   = ZIO.fromOption(max.map(BigDecimal(_)).toOption)
    val costRows: IO[Option[Throwable], Vector[CostRow]] = ZIO.fromOption(_costRows.toOption)
    val productionRows: IO[Option[Throwable], Vector[ProductionRow]] = ZIO.fromOption(_prodRows.toOption)
  }

  case class DynamicStandardCard(
      stateRef: Ref[DynCard],
      _costRows: js.UndefOr[Vector[CostRow]] = js.undefined,
      _productionRows: js.UndefOr[Vector[ProductionRow]] = js.undefined,
  ) extends Card {
    val name: IO[Option[Throwable], String]                          = stateRef.map(_.name).get
    val count: IO[Option[Throwable], BigDecimal]                     = stateRef.map(_.count).get
    val maxCanBuild: IO[Option[Throwable], BigDecimal]               = stateRef.map(_.max).get
    val costRows: IO[Option[Throwable], Vector[CostRow]]             = ZIO.fromOption(_costRows.toOption)
    val productionRows: IO[Option[Throwable], Vector[ProductionRow]] = ZIO.fromOption(_productionRows.toOption)

    val sections: Task[Vector[Section]] = {
      def clickAction(num: Int) = stateRef
        .update(dc => dc.copy(count = dc.count + num, max = dc.max - num))
        .whenM(stateRef.map(_.max).get.map(_ >= num))

      def eqBtn(num: Int) = stateRef
        .map(_.count)
        .get
        .flatMap(count =>
          ZIO.fromOption {
            if (count < num)
              Some(TestClickActionButton(s"= $num", onClick = clickAction(num - count.toIntExact)))
            else None
          }
        )

      val buyBtns = ZIO.collect(
        Vector(
          eqBtn(5),
          eqBtn(25),
          eqBtn(75),
          eqBtn(150),
          eqBtn(250),
          ZIO.succeed(TestClickActionButton("+ 1", onClick = clickAction(1))).asSomeError,
        )
      )(identity)

      buyBtns.map { bb =>
        Vector(
          TestSection("Production"),
          TestSection("Costs"),
          TestBuyButtons(bb),
        )
      }
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
      count: BigDecimal = 0,
      max: BigDecimal = 0,
      costRows: js.UndefOr[Vector[CostRow]] = js.undefined,
      productionRows: js.UndefOr[Vector[ProductionRow]] = js.undefined,
  )

  def createDynamicPage(name: String, prefixCards: Vector[Card], dynCards: Vector[DynCard]): UIO[Page] = for {
    dynStdCards <- ZIO.foreach(dynCards)(DynamicStandardCard.make)
  } yield new Page {
    val pageName: IO[Option[Throwable], String]    = ZIO.fromOption(name.toOption)
    val cards: IO[Option[Throwable], Vector[Card]] = UIO.succeed(prefixCards ++ dynStdCards)
  }

  def testPNV(t: String, n: BigDecimal) = assertTrue(parseNumberValue(t) == Some(n))

  final val spec = suite("UIInterface")(
    suite("int parsing")(
      test("works with int") {
        testPNV("3", 3)
      },
      test("works with negative") {
        testPNV("-42", -42)
      },
      test("works with decimal") {
        testPNV("3.01k", 3010)
      },
      test("works with negative decimal") {
        testPNV("-3.01k", -3010)
      },
      test("works without decimal") {
        testPNV("-4T", BigDecimal(-4) * BigDecimal(1000).pow(4))
      },
    )
  )

}
