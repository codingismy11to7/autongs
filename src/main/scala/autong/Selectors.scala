package autong

import autong.Utils.RichStr
import japgolly.scalajs.react.ZIOfind
import org.scalajs.dom
import org.scalajs.dom.ext._
import org.scalajs.dom.{document, html, raw}
import zio.{Task, ZIO}

object Selectors {

  /*
  case class Button(b: html.Button) {
    def querySelector(p: String): Element             = b.querySelector(p)
    def querySelectorAll(selectors: String): NodeList = b.querySelectorAll(selectors)

    def classList = b.classList

    def click() =
      if (b.classList.contains("sidenav-item")) b.click()
      else if (currentPageName.forall(_ == "Dyson"))
        dom.console.warn("would click this!", b)
      else b.click()

  }
   */

  private val isDiv: PartialFunction[raw.Node, html.Div]    = { case d: html.Div => d }
  private val isBtn: PartialFunction[raw.Node, html.Button] = { case b: html.Button => b }

  implicit private class RichElOpt(val v: Option[raw.Node]) extends AnyVal {
    def toDiv: Option[html.Div]    = v.collect(isDiv)
    def toBtn: Option[html.Button] = v.collect(isBtn)
  }

  def queryTextContent(path: String, from: raw.NodeSelector = dom.document): ZIO[Any, Option[Throwable], String] =
    ZIO(Option(from.querySelector(path)).flatMap(e => e.textContent.toOption)).some

  def queryIntContent(path: String, from: raw.NodeSelector = dom.document): ZIO[Any, Option[Throwable], Int] =
    ZIO(Option(from.querySelector(path)).flatMap(e => e.textContent.toOption).flatMap(_.toIntOption)).some

  def queryBtn(path: String, from: raw.NodeSelector = dom.document): ZIO[Any, Option[Throwable], html.Button] =
    ZIO(Option(from.querySelector(path)).toBtn).some

  private def queryBtns(path: String)(from: raw.NodeSelector = document) =
    ZIO(from.querySelectorAll(path).toVector.collect(isBtn))

  case class SideNavButton(btn: html.Button) {
    val name: ZIO[Any, Option[Throwable], String] = queryTextContent("div.row > div:nth-child(2)", btn)

    val storageDiv: ZIO[Any, Option[Throwable], html.Div] =
      ZIO(Option(btn.querySelector("div.row > div:last-child")).toDiv).some

    val amountStored: ZIO[Any, Option[Throwable], String] = storageDiv.flatMap(d => queryTextContent("span", d))

    val totalStorage: ZIO[Any, Option[Throwable], String] =
      storageDiv.flatMap(d => queryTextContent("small", d)).map(s => if (s.startsWith("/")) s.substring(1) else s)

  }

  case class SideNavEntry(div: html.Div) {
    val navButton: ZIO[Any, Option[Throwable], SideNavButton] = queryBtn("button.sidenav-item", div).map(SideNavButton)
    val upgradeButton: ZIO[Any, Option[Throwable], html.Button] = queryBtn("button[aria-label='Upgrade storage']", div)
  }

  val sideNavEntries: Task[Vector[SideNavEntry]] = ZIO(
    document
      .querySelectorAll(
        "#sidebar > div.content > div.simplebar-wrapper div.simplebar-content > div > div.row > div.col > div.collapse.row > div.col > div.row"
      )
      .toVector
      .collect(isDiv)
      .map(SideNavEntry)
  )

  def findSideNav(name: String): ZIO[Any, Option[Throwable], SideNavEntry] =
    sideNavEntries
      .flatMap(sideNavs => ZIOfind(sideNavs)(_.navButton.flatMap(_.name).optional.map(_ contains name)))
      .some

  case class Page(div: html.Div) {

    val pageName: ZIO[Any, Option[Throwable], String] =
      queryTextContent("div > div div.col > h5[role='heading']", div)

  }

  val currentPage: ZIO[Any, Option[Throwable], Page] =
    ZIO(Option(dom.document.querySelector("div.tab-pane.active")).toDiv.map(Page)).some

  val currentPageName: ZIO[Any, Option[Throwable], String] = currentPage.flatMap(_.pageName)

  def pageContents(page: Page): ZIO[Any, Option[Throwable], raw.Element] =
    ZIO(Option(page.div.querySelector("div.tab-pane.active > div > div:nth-child(2) > div.row.g-2"))).some

  case class CostRow(div: html.Div) {

    val rowName: ZIO[Any, Option[Throwable], String] =
      queryTextContent("div:nth-child(1) > button > div.row > div:nth-child(2)", div)

    val emcButton: ZIO[Any, Option[Throwable], html.Button] = queryBtn("div:nth-child(2) > button.me-3", div)
    val timeRemaining: ZIO[Any, Option[Throwable], String]  = queryTextContent("div:last-child > small > span", div)
  }

  case class ProductionRow(div: html.Div) {

    val rowName: ZIO[Any, Option[Throwable], String] =
      queryTextContent("div:nth-child(1) > button > div.row > div:nth-child(2)", div)

    val inputOrOutput: ZIO[Any, Option[Throwable], String] = queryTextContent("div.col-auto > small:nth-child(1)", div)
  }

  case class Section(div: html.Div) {

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

    val costRows: ZIO[Any, Option[Throwable], Vector[CostRow]] = getRows("Costs", CostRow)

    val productionRows: ZIO[Any, Option[Throwable], Vector[ProductionRow]] = getRows("Production", ProductionRow)
  }

  case class BulkBuyButton(btn: html.Button, buyAmount: Int)

  object BulkBuyButton {

    def opt(btn: html.Button): Option[BulkBuyButton] = Some(btn)
      .flatMap(_.textContent.toOption)
      .filter(_ startsWith "=")
      .map(_.replaceFirst("=\\s*", ""))
      .flatMap(_.toIntOption)
      .map(BulkBuyButton(btn, _))

  }

  case class Card(div: html.Div) {

    val sections: Task[Vector[Section]] = ZIO(
      div
        .querySelectorAll("div.card.card-body > div.row.gx-3 > div:nth-child(2) > div.row.g-3 > div.col-12")
        .toVector
        .collect(isDiv)
        .map(Section)
    )

    val name: ZIO[Any, Option[Throwable], String] =
      queryTextContent("div.card.card-body > div.row > div > div.row > div > div.row div > span.h6", div)

    val count: ZIO[Any, Option[Throwable], Int] =
      queryIntContent("div.card > div.row > div.col-12 > div.row > div.col-12 > div.row > div:nth-child(3) > span", div)

    val production: ZIO[Any, Option[Throwable], Section] =
      sections.flatMap(ZIOfind(_)(_.title.optional.map(_ contains "Production"))).some

    val costs: ZIO[Any, Option[Throwable], Section] =
      sections.flatMap(ZIOfind(_)(_.title.optional.map(_ contains "Costs"))).some

    val buyButtons: ZIO[Any, Option[Throwable], Vector[html.Button]] =
      sections
        .map(_.lastOption)
        .map(_.map(_.div.querySelectorAll("div.row > div > button").toVector.collect(isBtn)).filter(_.nonEmpty))
        .some

    val firstBulkBuyButton: ZIO[Any, Option[Throwable], BulkBuyButton] =
      buyButtons.optional.map(_.flatMap(_.headOption).flatMap(BulkBuyButton.opt)).some

  }

  def pageCards(pageContents: raw.Element): Task[Vector[Card]] =
    ZIO(pageContents.querySelectorAll("div.row.g-2 > div").toVector.collect(isDiv).map(Card))

  val currentPageCards: ZIO[Any, Option[Throwable], Vector[Card]] =
    currentPage.flatMap(pageContents).flatMap(e => pageCards(e).asSomeError)

  val currentPageCardsWithBuyButtons: ZIO[Any, Option[Throwable], Vector[Card]] =
    currentPageCards.flatMap(ZIO.filter(_)(_.buyButtons.optional.map(_.exists(_.nonEmpty)).asSomeError))

  def segmentSections(segment: Card): ZIO[Any, Option[Throwable], (Section, Section)] =
    segment.sections.map(Some(_).filter(_.size == 2).map(s => s(0) -> s(1))).some

  private def buyButtons(btnRow: html.Div) = queryBtns("div.row > div.col-auto > button")(btnRow)

  def segmentBuyButtons(
      buttons: Section
  ): ZIO[Any, Option[Throwable], (html.Button, html.Button, html.Button, html.Button)] =
    buyButtons(buttons.div).map(Some(_).filter(_.size == 4).map(b => (b(0), b(1), b(2), b(3)))).some

  private def twoButtons(arr: Vector[html.Button]) = ZIO(Some(arr).filter(_.size == 2).map(b => b(0) -> b(1))).some

  def ringBuyButtons(buttons: Vector[html.Button]): ZIO[Any, Option[Throwable], (html.Button, html.Button)] =
    twoButtons(buttons)

  def swarmBuyButtons(buttons: Vector[html.Button]): ZIO[Any, Option[Throwable], (html.Button, html.Button)] =
    twoButtons(buttons)

}
