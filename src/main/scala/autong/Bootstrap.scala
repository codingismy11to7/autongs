package autong

import autong.ui.AutoNGMenu
import japgolly.scalajs.react._
import org.scalajs.dom
import zio.Console.printLine
import zio._

import scala.scalajs.js

object Bootstrap {

  final private val mainId = "autongsButton"

  def bootstrapUi(controller: AutoNG): RTask[Unit] = {
    val handleGlobal = for {
      oldCont <- ZIO.attempt(dom.window.asInstanceOf[js.Dynamic].autoNGs.asInstanceOf[js.UndefOr[AutoNG]])
      _       <- oldCont.fold(RT)(_.stop)
    } yield dom.window.asInstanceOf[js.Dynamic].autoNGs = controller.asInstanceOf[js.Object]

    val tryToLoad = printLine("Attempting to load AutoNG[S]") *> ZIO.attempt {
      Option(dom.document.querySelector("#page > div.header > div.row")).fold(false) { parent =>
        Option(dom.document.getElementById(mainId)).foreach { n =>
          ReactDOM.unmountComponentAtNode(n)
          Option(n.parentNode).foreach(_.removeChild(n))
        }
        val ourDiv = dom.document.createElement("div")
        ourDiv.id = mainId
        ourDiv.classList.add("col-auto")
        Option(parent.appendChild(ourDiv)).fold(false) { case d: dom.Element =>
          AutoNGMenu(controller).renderIntoDOM(d, RT *> printLine("Loaded AutoNG[S]"))
          true
        }
      }
    }

    def scheduleLoad(delay: Duration): RIO[Has[Console] with Has[Clock], Unit] =
      (RIO.unit *> scheduleLoad((delay * 2).max(5.seconds)).delay(delay)).unlessZIO(tryToLoad).unit

    handleGlobal *> scheduleLoad(10.millis)
  }

}
