package autong.ui

import autong.AutoNG
import io.kinoplan.scalajs.react.material.ui.core.styles._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.ScalaFn.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

import scala.scalajs.js.Dynamic.{literal => lit}

object AutoNGMenu {

//  final private val theme = lit(palette = lit(primary = lit(main = "#00f500")))

  final private val theme = createTheme(
    ThemeOptions(
      palette = PaletteOptions(
        `type` = "dark",
        background = TypeBackgroundPartial(default = "#343C4A", paper = "#232A35"),
        primary = PaletteColorOptions(main = "#00f500"),
        secondary = PaletteColorOptions(main = "#f50000"),
      )
    )
  )

  case class Props(controller: AutoNG)

  final val AutoNGMenu = ScalaFnComponent
    .withHooks[Props]
    .useState(false)
    .render { (props, opened) =>
      val onSetOpen = (o: Boolean) => RT *> opened.setState(o)

      MuiThemeProvider(theme = theme)(
        ControllerContext.ctx.provide(props.controller)(
          <.button(
            ^.style := lit("color" -> theme.palette.primary.main),
            ^.onClick --> opened.modState(!_),
          )(
            SkipForward()
          ),
          OptionsDialog(opened.value, onSetOpen),
        )
      )
    }

  def apply(controller: AutoNG): Unmounted[Props] = AutoNGMenu(Props(controller))
}
