package autong.ui

import autong.{AutoNG, BuildMachinesOpts}
import autong.ui.icons.{GearWideConnected, SkipForward}
import io.kinoplan.scalajs.react.material.ui.core.styles._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.ScalaFn.Unmounted
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._

object AutoNGMenu {
  sealed private trait MachinesDlgState

  private object MachinesDlgState {
    final object Closed            extends MachinesDlgState
    final object OpenedFromOptions extends MachinesDlgState
    final object OpenedFromMenu    extends MachinesDlgState
  }

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
    .useState(MachinesDlgState.Closed: MachinesDlgState)
    .render { (props, opened, machinesDlgOpen) =>
      val onSetOpen = (o: Boolean) => RT *> opened.setState(o)

      val onBuyMachinesCancel = RT *>
        machinesDlgOpen.setState(MachinesDlgState.Closed) *>
        onSetOpen(true).when(machinesDlgOpen.value == MachinesDlgState.OpenedFromOptions)

      val buyMachines = (opts: BuildMachinesOpts) =>
        props.controller.buyMachines(opts) *> machinesDlgOpen.setState(MachinesDlgState.Closed)

      MuiThemeProvider(theme = theme)(
        ControllerContext.ctx.provide(props.controller)(
          <.button(
            ^.width := 32.px,
            ^.color := theme.palette.primary.main,
            ^.onClick --> opened.modState(!_),
          )(
            SkipForward()
          ),
          <.button(
            ^.width := 32.px,
            ^.color := theme.palette.secondary.dark,
            ^.onClick --> machinesDlgOpen.setState(MachinesDlgState.OpenedFromMenu),
          )(
            GearWideConnected()
          ),
          OptionsDialog(
            opened.value,
            onSetOpen,
            o => machinesDlgOpen.setState(if (o) MachinesDlgState.OpenedFromOptions else MachinesDlgState.Closed),
          ),
          if (machinesDlgOpen.value != MachinesDlgState.Closed) BuyMachinesDialog(onBuyMachinesCancel, buyMachines)
          else ReactFragment(),
        )
      )
    }

  def apply(controller: AutoNG): Unmounted[Props] = AutoNGMenu(Props(controller))
}
