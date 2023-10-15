package autong
package ui

import autong.Utils._
import autong.ui.ControllerContext._
import autong.ui.hooks.useStateFromProps
import autong.ui.icons.{GearWideConnected, GitHub, SkipForward}
import autong.ui.muifixes.FixedMuiButton
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
import scala.scalajs.js.special.{objectLiteral => obj}

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
          divStyle: js.Object = obj(),
      ) =
        <.div(^.style := divStyle)(switchCtrl(label, checked, onSet, disabled))

      val autoScienceEnabled = currOptions.value.autoScienceEnabled getOrElse savedOptions.autoScienceEnabled
      val autoTechsEnabled   = currOptions.value.autoTechsEnabled getOrElse savedOptions.autoTechsEnabled

      val (currentNotif, markAsRead) = currNotif

      val selOnFocus: js.Function1[dom.FocusEvent, Unit] = e => e.target.asInstanceOf[html.Input].select()
      val saveOnEnter: js.Function1[dom.KeyboardEvent, Unit] =
        e => implicitly[JsWriter[RTask[Unit]]].toJs(onSave.when(e.key == "Enter"))

      val autoDyson = currOptions.value.autoDyson getOrElse savedOptions.autoDyson

      val openGithub = RT.as(dom.window.open("https://github.com/codingismy11to7/autongs", target = "_blank")).unit

      val bulkBuyMachines = currOptions.value.bulkBuyMachines getOrElse savedOptions.bulkBuyMachines

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
            MuiTooltip[RTask](title = "Automatically build segments/rings/swarms/spheres, when on Dyson page").apply(
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
            MuiTooltip[RTask](title = "Buy sphere after the specified number of rings and swarms are built").apply(
              divSwitchCtrl(
                "Automatically buy Sphere",
                currOptions.value.autoBuySphere getOrElse savedOptions.autoBuySphere,
                x => _.copy(autoBuySphere = x),
                !autoDyson,
              )
            ),
            MuiTooltip[RTask](title =
              "Buy swarms if left idle on the Dyson page after Sphere is purchased (to boost Dark Matter)"
            )
              .apply(
                divSwitchCtrl(
                  "Buy Swarms after Sphere",
                  currOptions.value.buySwarmsAfterSphere getOrElse savedOptions.buySwarmsAfterSphere,
                  x => _.copy(buySwarmsAfterSphere = x),
                  !autoDyson,
                )
              ),
            MuiDivider()(),
            MuiTooltip[RTask](title = "Upgrades storage when possible AND the resource's storage is full").apply(
              divSwitchCtrl(
                "Upgrade Storage",
                currOptions.value.storageEnabled getOrElse savedOptions.storageEnabled,
                x => _.copy(storageEnabled = x),
              )
            ),
            MuiTooltip[RTask](title =
              "Try to upgrade storage when needed while on Nanoswarms/Comms/Spaceship/Antimatter/Military. " +
                "While idling on these screens with Auto-EMC on, the storage requirements can grow too large, which " +
                "will cause the EMC shortcut to not be displayed. In that case, upgrade storage for the affected resource " +
                "if the upgrade is enabled. If it's not enabled, manual intervention (switching Nanoswarms to that resource) " +
                "will be needed."
            ).apply(
              divSwitchCtrl(
                "Emergency Storage Upgrades",
                currOptions.value.upgradeStorageForEMC getOrElse savedOptions.upgradeStorageForEMC,
                x => _.copy(upgradeStorageForEMC = x),
              )
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
                bulkBuyMachines,
                x => _.copy(bulkBuyMachines = x),
              )
            ),
            MuiTooltip[RTask](title = "Only bulk buy when energy output is greater than 2M/s").apply(
              divSwitchCtrl(
                "Only with excess energy",
                currOptions.value.bulkBuyOnlyWhenRich getOrElse savedOptions.bulkBuyOnlyWhenRich,
                x => _.copy(bulkBuyOnlyWhenRich = x),
                !bulkBuyMachines,
                obj("display" -> (if (bulkBuyMachines) js.undefined else "none"), "paddingLeft" -> 32.px),
              )
            ),
            MuiDivider()(),
            MuiTooltip[RTask](title =
              "Buys science when on Science page, or switches to the page and buys science" +
                " at the specified interval when on an idle page (Dyson, Comms, Spaceship, etc)"
            ).apply(
              divSwitchCtrl("Automatically buy Science", autoScienceEnabled, x => _.copy(autoScienceEnabled = x))
            ),
            MuiTooltip[RTask](title =
              "Switches to Technologies page and buys any available at the" +
                " specified interval while on an idle page"
            ).apply(
              divSwitchCtrl(
                "Automatically Unlock/Boost Technologies",
                autoTechsEnabled,
                x => _.copy(autoTechsEnabled = x),
              )
            ),
            MuiDivider()(),
            MuiTooltip[RTask](title =
              "Automatically purchase Nanoswarms when on the Nanoswarm page. " +
                "Works well with Auto-EMC and Nanoswarms set to Energy"
            ).apply(
              divSwitchCtrl(
                "Buy Nanoswarms",
                currOptions.value.buyNanoswarm getOrElse savedOptions.buyNanoswarm,
                x => _.copy(buyNanoswarm = x),
              )
            ),
            MuiDivider()(),
            MuiTooltip[RTask](title =
              "Buys the Astronomical Breakthrough, followed by Interstellar Radar Scanners," +
                " when on Communications page"
            )
              .apply(
                divSwitchCtrl(
                  "Buy Communications",
                  currOptions.value.buyCommunications getOrElse savedOptions.buyCommunications,
                  x => _.copy(buyCommunications = x),
                )
              ),
            MuiTooltip[RTask](title = "Buys Spaceship & spaceship parts, when on Spaceship page")
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
            MuiTooltip[RTask](title =
              "Buys military ships as soon as they're available for purchase," +
                " when on the purchase page"
            ).apply(
              divSwitchCtrl(
                "Buy Military",
                currOptions.value.buyMilitary getOrElse savedOptions.buyMilitary,
                x => _.copy(buyMilitary = x),
              )
            ),
            MuiDivider()(),
            MuiTooltip[RTask](title =
              "How often to try to do all the selected actions. 500 milliseconds (half a second) " +
                "is a good default, weighing responsiveness vs bogging down the game"
            ).apply(
              MuiTextField(
                label = "Run Interval (ms)": VdomNode,
                fullWidth = true,
                inputProps = js.special.objectLiteral("onFocus" -> selOnFocus, "onKeyUp" -> saveOnEnter),
              )(
                ^.value := currOptions.value.taskInterval,
                ^.onChange ==> ((e: ReactEventFromInput) => setTaskInterval(e.target.value)),
              )
            ),
            MuiTooltip[RTask](title =
              "How often to switch to Science and/or Technologies pages to buy available items, while on an idle page." +
                " 60,000 milliseconds (every minute) seems to be reasonable."
            ).apply(
              MuiTextField(label = "Auto-Science/Technologies Interval (ms)": VdomNode, fullWidth = true)(
                ^.value := currOptions.value.autoSciTechInterval,
                ^.onChange ==> ((e: ReactEventFromInput) => setAutoSciTechInterval(e.target.value)),
              )
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
