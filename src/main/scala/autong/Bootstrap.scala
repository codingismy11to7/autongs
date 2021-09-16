package autong

import autong.ui.AutoNGMenu
import japgolly.scalajs.react._
import org.scalajs.dom
import zio.ZIO
import zio.clock.Clock
import zio.console.{putStrLn, Console}

import java.time.Duration
import scala.scalajs.js

object Bootstrap {
  private def max(a: Duration, b: Duration) = if (a.compareTo(b) >= 0) a else b

  final private val mainId = "autongsButton"

  def bootstrapUi(controller: AutoNG): RTask[Unit] = {
    val handleGlobal = for {
      oldCont <- ZIO.effect(dom.window.asInstanceOf[js.Dynamic].autoNGs.asInstanceOf[js.UndefOr[AutoNG]])
      _       <- oldCont.fold(RT)(_.stop)
    } yield dom.window.asInstanceOf[js.Dynamic].autoNGs = controller.asInstanceOf[js.Object]

    val tryToLoad = putStrLn("Attempting to load AutoNG[S]") *> ZIO.effect {
      Option(dom.document.querySelector("#page > div.header > div.row")).fold(false) { parent =>
        Option(dom.document.getElementById(mainId)).foreach { n =>
          ReactDOM.unmountComponentAtNode(n)
          Option(n.parentNode).foreach(_.removeChild(n))
        }
        val ourDiv = dom.document.createElement("div")
        ourDiv.id = mainId
        ourDiv.classList.add("col-auto")
        Option(parent.appendChild(ourDiv)).fold(false) { case d: dom.Element =>
          AutoNGMenu(controller).renderIntoDOM(d, RT *> putStrLn("Loaded AutoNG[S]"))
          true
        }
      }
    }

    def scheduleLoad(delay: Duration): ZIO[Console with Clock, Throwable, Unit] =
      ZIO.sleep(delay) *> scheduleLoad(max(delay.multipliedBy(2), Duration.ofSeconds(5))).unlessM(tryToLoad)

    handleGlobal *> scheduleLoad(Duration.ofMillis(10))
  }

}
