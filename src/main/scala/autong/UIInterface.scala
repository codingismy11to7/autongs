package autong

import autong.UIInterface.{Page, SideNavEntry}
import zio._

trait UIInterface {
  def currentPage: ZIO[Any, Option[Throwable], Page]
  def sideNavEntries: Task[Vector[SideNavEntry]]
}

object UIInterface {

  trait Page {
    def pageName: ZIO[Any, Option[Throwable], String]
    def cards: ZIO[Any, Option[Throwable], Vector[Card]]
  }

  trait SideNavButton {
    def name: ZIO[Any, Option[Throwable], String]

    def amountStored: ZIO[Any, Option[Throwable], String]
    def totalStorage: ZIO[Any, Option[Throwable], String]

    def click: Task[Unit]
  }

  trait SideNavEntry {
    def navButton: ZIO[Any, Option[Throwable], SideNavButton]

    def upgradeButton: ZIO[Any, Option[Throwable], Button]
  }

  trait CostRow {
    def rowName: ZIO[Any, Option[Throwable], String]

    def emcButton: ZIO[Any, Option[Throwable], Button]
    def timeRemaining: ZIO[Any, Option[Throwable], String]
  }

  trait ProductionRow {
    def rowName: ZIO[Any, Option[Throwable], String]
    def inputOrOutput: ZIO[Any, Option[Throwable], String]
  }

  trait Section {
    def title: ZIO[Any, Option[Throwable], String]

    def max: ZIO[Any, Option[Throwable], Int]

    def costRows: ZIO[Any, Option[Throwable], Vector[CostRow]]

    def productionRows: ZIO[Any, Option[Throwable], Vector[ProductionRow]]

    def buyButtons: Task[Option[Vector[Button]]]
  }

  final case class BulkBuyButton(btn: Button, buyAmount: Int) {
    val click: Task[Unit] = btn.click
  }

  object BulkBuyButton {

    def opt(btn: Button): Option[BulkBuyButton] = btn.name
      .filter(_ startsWith "=")
      .map(_.replaceFirst("=\\s*", ""))
      .flatMap(_.toIntOption)
      .map(BulkBuyButton(btn, _))

  }

  trait Button {
    def name: Option[String]

    def click: Task[Unit]

    def disabled: Task[Boolean]
  }

  trait Card {
    def sections: Task[Vector[Section]]

    def name: ZIO[Any, Option[Throwable], String]

    def count: ZIO[Any, Option[Throwable], Int]

    final lazy val production: ZIO[Any, Option[Throwable], Section] =
      sections.flatMap(ZIOfind(_)(_.title.optional.map(_ contains "Production"))).some

    final lazy val costs: ZIO[Any, Option[Throwable], Section] =
      sections.flatMap(ZIOfind(_)(_.title.optional.map(_ contains "Costs"))).some

    final lazy val buyButtons: ZIO[Any, Option[Throwable], Vector[Button]] =
      sections.map(_.lastOption).asSomeError.flatMap(ZIO.fromOption(_)).flatMap(_.buyButtons.some)

    final def buyButton(name: String): ZIO[Any, Option[Throwable], Button] =
      buyButtons.optional.map(_.flatMap(_.find(_.name contains name))).some

    final lazy val lastBuyButton: ZIO[Any, Option[Throwable], Button] = buyButtons.map(_.last)

    final lazy val firstBulkBuyButton: ZIO[Any, Option[Throwable], BulkBuyButton] =
      buyButtons.optional.map(_.flatMap(_.headOption).flatMap(BulkBuyButton.opt)).some

  }

  val currentPage: ZIO[Has[UIInterface], Option[Throwable], Page]       = ZIO.serviceWith[UIInterface](_.currentPage)
  val currentPageName: ZIO[Has[UIInterface], Option[Throwable], String] = currentPage.flatMap(_.pageName)

  val sideNavEntries: RIO[Has[UIInterface], Vector[SideNavEntry]] = ZIO.serviceWith[UIInterface](_.sideNavEntries)

  def findSideNav(name: String): ZIO[Has[UIInterface], Option[Throwable], SideNavEntry] =
    sideNavEntries
      .flatMap(sideNavs => ZIOfind(sideNavs)(_.navButton.flatMap(_.name).optional.map(_ contains name)))
      .some

  val currentPageCards: ZIO[Has[UIInterface], Option[Throwable], Vector[Card]] =
    currentPage.flatMap(_.cards)

  val currentPageCardsWithBuyButtons: ZIO[Has[UIInterface], Option[Throwable], Vector[Card]] =
    currentPageCards.flatMap(ZIO.filter(_)(_.buyButtons.optional.map(_.exists(_.nonEmpty)).asSomeError))

  lazy val live: ZLayer[Any, Nothing, Has[UIInterface]] = ZLayer.succeed {
    new liveHelpers.LiveUIInterface
  }

  private object liveHelpers {
    import autong.Utils.RichStr
    import org.scalajs.dom
    import org.scalajs.dom.ext._
    import org.scalajs.dom.{html, raw}

    private val isDiv: PartialFunction[raw.Node, html.Div]    = { case d: html.Div => d }
    private val isBtn: PartialFunction[raw.Node, html.Button] = { case b: html.Button => b }

    implicit private class RichElOpt(val v: Option[raw.Node]) extends AnyVal {
      def toDiv: Option[html.Div]    = v.collect(isDiv)
      def toBtn: Option[html.Button] = v.collect(isBtn)
    }

    private def queryTextContent(
        path: String,
        from: raw.NodeSelector = dom.document,
    ): ZIO[Any, Option[Throwable], String] =
      ZIO.fromOption(Option(from.querySelector(path)).flatMap(e => e.textContent.toOption))

    private def queryIntContent(path: String, from: raw.NodeSelector = dom.document): ZIO[Any, Option[Throwable], Int] =
      ZIO.fromOption(Option(from.querySelector(path)).flatMap(e => e.textContent.toOption).flatMap(_.toIntOption))

    private def queryBtn(
        path: String,
        from: raw.NodeSelector = dom.document,
    ): ZIO[Any, Option[Throwable], html.Button] =
      ZIO.fromOption(Option(from.querySelector(path)).toBtn)

    private[UIInterface] class LiveUIInterface extends UIInterface {

      val currentPage: ZIO[Any, Option[Throwable], Page] =
        ZIO.fromOption(Option(dom.document.querySelector("div.tab-pane.active")).toDiv.map(LivePage))

      val sideNavEntries: Task[Vector[SideNavEntry]] = ZIO(
        dom.document
          .querySelectorAll(
            "#sidebar > div.content > div.simplebar-wrapper div.simplebar-content > div > div.row > div.col > div.collapse.row > div.col > div.row"
          )
          .toVector
          .collect(isDiv)
          .map(LiveSideNavEntry)
      )

    }

    final private case class LivePage(div: html.Div) extends Page {

      val pageName: ZIO[Any, Option[Throwable], String] =
        queryTextContent("div > div div.col > h5[role='heading']", div)

      private def pageCards(pageContents: raw.Element): Task[Vector[Card]] =
        ZIO(pageContents.querySelectorAll("div.row.g-2 > div").toVector.collect(isDiv).map(LiveCard))

      val cards: ZIO[Any, Option[Throwable], Vector[Card]] =
        ZIO
          .fromOption(Option(div.querySelector("div.tab-pane.active > div > div:nth-child(2) > div.row.g-2")))
          .flatMap(e => pageCards(e).asSomeError)

    }

    final private case class LiveSideNavButton(btn: html.Button) extends SideNavButton {
      val name: ZIO[Any, Option[Throwable], String] = queryTextContent("div.row > div:nth-child(2)", btn)

      private val storageDiv: ZIO[Any, Option[Throwable], html.Div] =
        ZIO.fromOption(Option(btn.querySelector("div.row > div:last-child")).toDiv)

      val amountStored: ZIO[Any, Option[Throwable], String] = storageDiv.flatMap(d => queryTextContent("span", d))

      val totalStorage: ZIO[Any, Option[Throwable], String] =
        storageDiv.flatMap(d => queryTextContent("small", d)).map(s => if (s.startsWith("/")) s.substring(1) else s)

      val click: Task[Unit] = Task(btn.click())
    }

    final private case class LiveSideNavEntry(div: html.Div) extends SideNavEntry {

      val navButton: ZIO[Any, Option[Throwable], SideNavButton] =
        queryBtn("button.sidenav-item", div).map(LiveSideNavButton)

      val upgradeButton: ZIO[Any, Option[Throwable], Button] =
        queryBtn("button[aria-label='Upgrade storage']", div).map(LiveButton)

    }

    final private case class LiveCostRow(div: html.Div) extends CostRow {

      val rowName: ZIO[Any, Option[Throwable], String] =
        queryTextContent("div:nth-child(1) > button > div.row > div:nth-child(2)", div)

      val emcButton: ZIO[Any, Option[Throwable], Button] =
        queryBtn("div:nth-child(2) > button.me-3", div).map(LiveButton)

      val timeRemaining: ZIO[Any, Option[Throwable], String] = queryTextContent("div:last-child > small > span", div)
    }

    final private case class LiveProductionRow(div: html.Div) extends ProductionRow {

      val rowName: ZIO[Any, Option[Throwable], String] =
        queryTextContent("div:nth-child(1) > button > div.row > div:nth-child(2)", div)

      val inputOrOutput: ZIO[Any, Option[Throwable], String] =
        queryTextContent("div.col-auto > small:nth-child(1)", div)

    }

    final private case class LiveSection(div: html.Div) extends Section {

      // if there's max or calculator or whatever, the text is nested, otherwise it's just in the top heading-6
      val title: ZIO[Any, Option[Throwable], String] =
        queryTextContent("div.heading-6 > div.row > div.col-auto", div) orElse queryTextContent("div.heading-6", div)

      val max: ZIO[Any, Option[Throwable], Int] =
        queryIntContent("div.heading-6 > div.row > div:last-child > span:nth-child(2)", div)

      private def getRows[Row](name: String, f: (html.Div) => Row) =
        title.optional.map { title =>
          if (title contains name) Some(div.children.toVector.tail.collect(isDiv).map(f)).filter(_.nonEmpty)
          else None
        }.some

      val costRows: ZIO[Any, Option[Throwable], Vector[CostRow]] = getRows("Costs", LiveCostRow)

      val productionRows: ZIO[Any, Option[Throwable], Vector[ProductionRow]] = getRows("Production", LiveProductionRow)

      val buyButtons: Task[Option[Vector[Button]]] =
        Task(
          Option(div.querySelectorAll("div.row > div > button").toVector.collect(isBtn).map(LiveButton))
            .filter(_.nonEmpty)
        )

    }

    final private case class LiveButton(btn: html.Button) extends Button {
      def name: Option[String] = btn.textContent.toOption

      val click: Task[Unit] = Task(btn.click())

      val disabled: Task[Boolean] = Task(btn.classList.contains("disabled"))
    }

    final private case class LiveCard(div: html.Div) extends Card {

      val sections: Task[Vector[Section]] = ZIO(
        div
          .querySelectorAll("div.card.card-body > div.row.gx-3 > div:nth-child(2) > div.row.g-3 > div.col-12")
          .toVector
          .collect(isDiv)
          .map(LiveSection)
      )

      // "completed" cards have their name nested differently
      val name: ZIO[Any, Option[Throwable], String] =
        queryTextContent("div.card.card-body > div.row > div > div.row > div > div.row div > span.h6", div) orElse
          queryTextContent("div.card.card-body > div.row > div.text-truncate > span.h6", div)

      val count: ZIO[Any, Option[Throwable], Int] =
        queryIntContent(
          "div.card > div.row > div.col-12 > div.row > div.col-12 > div.row > div:nth-child(3) > span",
          div,
        )

    }

  }

}
