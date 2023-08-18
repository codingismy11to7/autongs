package autong
package ui

import autong.Utils._
import autong.ui.ControllerContext._
import autong.ui.hooks.useStateFromProps
import autong.ui.icons.{GearWideConnected, GitHub, SkipForward}
import autong.ui.muifixes.FixedMuiDivider.{Orientation, Variant}
import autong.ui.muifixes.{FixedMuiButton, FixedMuiDivider}
import io.kinoplan.scalajs.react.bridge.JsWriter
import io.kinoplan.scalajs.react.material.ui.core._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.ScalaFn.Unmounted
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.html
import zio.prelude.EqualOps

import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichOption

object OptionsDialog {
  case class Props(open: Boolean, onSetOpen: (Boolean) => RTask[Unit], onSetBuyMachinesOpen: (Boolean) => RTask[Unit])

  private def startStopClicked(controller: AutoNG, started: Boolean) =
    if (started) controller.stop else controller.start

  private val defOpts = () => Options()

  final val Component = ScalaFnComponent
    .withHooks[Props]
    .custom(controller)
    .custom(isStarted)
    .custom(options)
    .customBy((_, _, _, savedOptions) => useStateFromProps[Options].apply(Some(savedOptions.toOptions) -> defOpts))
    .custom(sendNotif)
    .custom(currentNotif)
    .render { (props, controller, started, savedOptions, currOptions, sendNotif, currNotif) =>
      val handleClose = RT *> currOptions.setState(None) *> props.onSetOpen(false)

      val onBuyMachinesOpen = RT *> props.onSetBuyMachinesOpen(true) *> handleClose

      def setCurrOptions(f: (Options) => Options) =
        currOptions.modState(o => Some(f(o.getOrElse(Options()))))

      val nonNeg = (f: (Options, js.UndefOr[Int]) => Options) =>
        (s: String) => setCurrOptions(o => f(o, toNonNegInt(s.toOption).orUndefined))
      val setRingCount           = nonNeg((o, i) => o.copy(ringCount = i))
      val setSwarmCount          = nonNeg((o, i) => o.copy(swarmCount = i))
      val setTaskInterval        = nonNeg((o, i) => o.copy(taskInterval = i))
      val setAutoSciTechInterval = nonNeg((o, i) => o.copy(autoSciTechInterval = i))

      val onSave = controller.reconfigure(currOptions.value) *> sendNotif("Options saved") *> handleClose

      def switchCtrl(
          label: String,
          checked: Boolean,
          onSet: (Boolean) => (Options) => Options,
          disabled: Boolean = false,
      ) =
        MuiFormControlLabel[RTask](
          label = label: VdomNode,
          control = MuiSwitch(checked = checked, color = MuiSwitch.Color.primary)(
            ^.disabled := disabled,
            ^.onChange ==> ((e: ReactEventFromInput) => setCurrOptions(onSet(e.target.checked))),
          ).rawElement,
        )

      def divSwitchCtrl(
          label: String,
          checked: Boolean,
          onSet: (Boolean) => (Options) => Options,
          disabled: Boolean = false,
      ) =
        <.div(switchCtrl(label, checked, onSet, disabled))

      val autoScienceEnabled = currOptions.value.autoScienceEnabled getOrElse savedOptions.autoScienceEnabled
      val autoTechsEnabled   = currOptions.value.autoTechsEnabled getOrElse savedOptions.autoTechsEnabled

      val (currentNotif, markAsRead) = currNotif

      val selOnFocus: js.Function1[dom.FocusEvent, Unit] = e => e.target.asInstanceOf[html.Input].select()
      val saveOnEnter: js.Function1[dom.KeyboardEvent, Unit] =
        e => implicitly[JsWriter[RTask[Unit]]].toJs(onSave.when(e.key == "Enter"))

      val autoDyson = currOptions.value.autoDyson getOrElse savedOptions.autoDyson

      val openGithub = RT.as(dom.window.open("https://github.com/codingismy11to7/autongs", target = "_blank")).unit

      ReactFragment(
        MuiSnackbar[RTask](
          autoHideDuration = 3000,
          open = currentNotif.isDefined,
          onClose = js.defined((_, reason) => markAsRead.unless(reason == "clickaway")),
        ).apply(
          MuiSnackbarContent(
            action = MuiIconButton()(^.onClick --> props.onSetOpen(true), SkipForward()): VdomNode,
            message = currentNotif.map(s => s: VdomNode).getOrElse(""): VdomNode,
          )(
            ^.color := "#9aa4ab",
            ^.backgroundColor := "#232a35bf",
          )
        ),
        MuiDialog[RTask](
          open = props.open,
          onClose = js.defined((_, _) => handleClose),
        ).apply(
          MuiDialogContent()(
            <.div(
              MuiTooltip[RTask](title = "Automatically click EMC shortcuts. Must have shortcut enabled on EMC page.")
                .apply(
                  switchCtrl(
                    "Auto-EMC",
                    currOptions.value.autoEmc getOrElse savedOptions.autoEmc,
                    x => _.copy(autoEmc = x),
                  )
                ),
              MuiFormControlLabel[RTask](
                label = "Only Meteorite?": VdomNode,
                control = MuiCheckbox(color = MuiCheckbox.Color.primary)(
                  ^.disabled := !(currOptions.value.autoEmc getOrElse savedOptions.autoEmc),
                  ^.checked := (currOptions.value.emcOnlyMeteorite getOrElse false),
                  ^.onChange ==> ((e: ReactEventFromInput) =>
                    setCurrOptions(_.copy(emcOnlyMeteorite = e.target.checked))
                  ),
                ).rawElement,
              ),
              MuiTooltip[RTask](title = "Only click EMC shortcut when plasma/batteries full (for meteorite/other)")
                .apply(
                  MuiFormControlLabel[RTask](
                    label = "When Full?": VdomNode,
                    control = MuiCheckbox(color = MuiCheckbox.Color.primary)(
                      ^.disabled := !(currOptions.value.autoEmc getOrElse savedOptions.autoEmc),
                      ^.checked := (currOptions.value.emcOnlyWhenFull getOrElse true),
                      ^.onChange ==> ((e: ReactEventFromInput) =>
                        setCurrOptions(_.copy(emcOnlyWhenFull = e.target.checked))
                      ),
                    ).rawElement,
                  )
                ),
            ),
            MuiDivider()(),
            MuiTooltip[RTask](title = "Automatically build segments/rings/swarms/spheres").apply(
              divSwitchCtrl("Auto-Dyson", autoDyson, x => _.copy(autoDyson = x))
            ),
            MuiTooltip[RTask](title = "Build this many Dyson Rings before starting to build Swarms").apply(
              MuiTextField(label = "Ring Count": VdomNode, fullWidth = true)(
                ^.disabled := !autoDyson,
                ^.value := currOptions.value.ringCount,
                ^.onChange ==> ((e: ReactEventFromInput) => setRingCount(e.target.value)),
              )
            ),
            MuiTooltip[RTask](title = "Build this many Dyson Swarms before starting to build Sphere").apply(
              MuiTextField(label = "Swarm Count": VdomNode, fullWidth = true)(
                ^.disabled := !autoDyson,
                ^.value := currOptions.value.swarmCount,
                ^.onChange ==> ((e: ReactEventFromInput) => setSwarmCount(e.target.value)),
              )
            ),
            divSwitchCtrl(
              "Automatically buy Sphere",
              currOptions.value.autoBuySphere getOrElse savedOptions.autoBuySphere,
              x => _.copy(autoBuySphere = x),
              !autoDyson,
            ),
            divSwitchCtrl(
              "Buy Swarms after Sphere",
              currOptions.value.buySwarmsAfterSphere getOrElse savedOptions.buySwarmsAfterSphere,
              x => _.copy(buySwarmsAfterSphere = x),
              !autoDyson,
            ),
            MuiDivider()(),
            divSwitchCtrl(
              "Upgrade Storage",
              currOptions.value.storageEnabled getOrElse savedOptions.storageEnabled,
              x => _.copy(storageEnabled = x),
            ),
            MuiTooltip[RTask](title =
              "Automatically purchase items that don't cost other resources over time, when it gets an achievement"
            ).apply(
              divSwitchCtrl(
                """Buy "Free" Items""",
                currOptions.value.buyFreeItems getOrElse savedOptions.buyFreeItems,
                x => _.copy(buyFreeItems = x),
              )
            ),
            MuiTooltip[RTask](title =
              "Bulk buy machines on all pages. Should only be used when energy isn't a concern."
            ).apply(
              divSwitchCtrl(
                "Bulk Buy Machines",
                currOptions.value.bulkBuyMachines getOrElse savedOptions.bulkBuyMachines,
                x => _.copy(bulkBuyMachines = x),
              )
            ),
            MuiDivider()(),
            divSwitchCtrl("Automatically buy Science", autoScienceEnabled, x => _.copy(autoScienceEnabled = x)),
            divSwitchCtrl(
              "Automatically Unlock/Boost Technologies",
              autoTechsEnabled,
              x => _.copy(autoTechsEnabled = x),
            ),
            MuiDivider()(),
            MuiTooltip[RTask](title = "Buys the Astronomical Breakthrough, followed by Interstellar Radar Scanners")
              .apply(
                divSwitchCtrl(
                  "Buy Communications",
                  currOptions.value.buyCommunications getOrElse savedOptions.buyCommunications,
                  x => _.copy(buyCommunications = x),
                )
              ),
            MuiTooltip[RTask](title = "Buys Spaceship & spaceship parts")
              .apply(
                divSwitchCtrl(
                  "Buy Spaceship",
                  currOptions.value.buySpaceship getOrElse savedOptions.buySpaceship,
                  x => _.copy(buySpaceship = x),
                )
              ),
            MuiTooltip[RTask](title = "Builds Alcubierre Drives when on Antimatter page")
              .apply(
                divSwitchCtrl(
                  "Buy Antimatter",
                  currOptions.value.buyAntimatter getOrElse savedOptions.buyAntimatter,
                  x => _.copy(buyAntimatter = x),
                )
              ),
            MuiTooltip[RTask](title = "Buys military ships as soon as they're available for purchase").apply(
              divSwitchCtrl(
                "Buy Military",
                currOptions.value.buyMilitary getOrElse savedOptions.buyMilitary,
                x => _.copy(buyMilitary = x),
              )
            ),
            MuiDivider()(),
            MuiTextField(
              label = "Run Interval (ms)": VdomNode,
              fullWidth = true,
              inputProps = js.special.objectLiteral("onFocus" -> selOnFocus, "onKeyUp" -> saveOnEnter),
            )(
              ^.value := currOptions.value.taskInterval,
              ^.onChange ==> ((e: ReactEventFromInput) => setTaskInterval(e.target.value)),
            ),
            MuiTextField(label = "Auto-Science/Technologies Interval (ms)": VdomNode, fullWidth = true)(
              ^.value := currOptions.value.autoSciTechInterval,
              ^.onChange ==> ((e: ReactEventFromInput) => setAutoSciTechInterval(e.target.value)),
            ),
          ),
          MuiDialogActions()(
            <.div(^.flex := "auto")(
              MuiIconButton()(^.onClick --> openGithub, GitHub())
            ),
            MuiButton(color = if (started) MuiButton.Color.secondary else MuiButton.Color.primary)(
              ^.onClick --> startStopClicked(controller, started),
              s"${if (started) "Stop" else "Start"}",
            ),
            FixedMuiButton(startIcon = GearWideConnected(): VdomNode)(
              ^.onClick --> onBuyMachinesOpen,
              "Buy Machines",
            ),
            MuiButton()(
              ^.disabled := savedOptions.toOptions === currOptions.value,
              ^.onClick --> onSave,
              "Save Options",
            ),
          ),
        ),
      )
    }

  def apply(
      open: Boolean,
      onSetOpen: (Boolean) => RTask[Unit],
      onSetBuyMachinesOpen: (Boolean) => RTask[Unit],
  ): Unmounted[Props] = Component(Props(open, onSetOpen, onSetBuyMachinesOpen))

}
