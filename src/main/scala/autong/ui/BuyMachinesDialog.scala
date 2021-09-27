package autong.ui

import autong.{BuildMachinesOpts, UIInterface}
import autong.UIInterface.currentPageCardsWithBuyButtons
import io.kinoplan.scalajs.react.material.ui.core._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.ScalaFn.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import zio.ZIO

import scala.scalajs.js

object BuyMachinesDialog {
  case class Props(onCancel: RTask[Unit], onStart: (BuildMachinesOpts) => RTask[Unit])

  private case class Machine(name: String, hasMax: Boolean)

  private def createMachineList(leaveUnbuilt: Boolean, machines: Vector[Machine]) =
    if (leaveUnbuilt) machines.filter(_.hasMax) else machines

  private val collectCurrentMachines = currentPageCardsWithBuyButtons.unsome
    .flatMap {
      case None => RT.as(Vector.empty[Machine])

      case Some(cards) =>
        ZIO.collect(cards) { card =>
          for {
            maxOpt <- card.maxCanBuild.unsome.asSomeError
            name   <- card.name.unsome.map(_.filterNot(_ == "Upgrade Storage")).some
          } yield Machine(name, maxOpt.isDefined)
        }
    }
    .provideCustomLayer(UIInterface.live)

  final val Component = ScalaFnComponent
    .withHooks[Props]
    .useState(Vector.empty[Machine])
    .useState(true)
    .useState(Set.empty[String])
    .useEffectOnMountBy((_, machinesState, _, limitTo) =>
      collectCurrentMachines.flatMap { machines =>
        machinesState.setState(machines) *> limitTo.setState(
          createMachineList(leaveUnbuilt = true, machines).map(_.name).toSet
        )
      }
    )
    .localValBy((_, machines, leaveUnbuilt, _) => createMachineList(leaveUnbuilt.value, machines.value))
    .render { (p, machines, leaveUnbuilt, limitTo, machineList) =>
      val onLeaveUnbuiltClicked = (newLeaveUnbuilt: Boolean) => {
        val newMachines = createMachineList(newLeaveUnbuilt, machines.value)
        limitTo.modState(o => o.filter(i => newMachines.exists(_.name == i))) *> leaveUnbuilt.setState(newLeaveUnbuilt)
      }

      val handleListItemClicked =
        (name: String) => limitTo.modState(o => if (o.contains(name)) o.filter(i => i != name) else o + name)

      val allSelected = limitTo.value.size == machineList.size

      val handleSelectAllClicked = limitTo.setState(if (allSelected) Set.empty else machineList.map(_.name).toSet)

      MuiDialog[RTask](open = true, onClose = js.defined((_, _) => p.onCancel)).apply(
        MuiDialogTitle()("Select Machines"),
        MuiDialogContent()(
          <.div(
            MuiFormControlLabel[RTask](
              label = "Try to leave unpurchased": VdomNode,
              control = MuiSwitch(checked = leaveUnbuilt.value)(
                ^.onChange ==> ((e: ReactEventFromInput) => onLeaveUnbuiltClicked(e.target.checked))
              ).rawElement,
            )
          ),
          if (machineList.isEmpty) MuiDialogContentText()("No Machines found")
          else
            MuiList()(
              (MuiListItem(button = true)(
                ^.onClick --> handleSelectAllClicked,
                MuiListItemIcon()(MuiCheckbox()(^.checked := allSelected)),
                MuiListItemText(primary = "Select All": VdomNode)(),
              ): VdomNode) :: machineList
                .map[VdomNode](m =>
                  MuiListItem(button = true)(
                    ^.onClick --> handleListItemClicked(m.name),
                    MuiListItemIcon()(MuiCheckbox()(^.checked := limitTo.value.contains(m.name))),
                    MuiListItemText(primary = m.name: VdomNode)(),
                  )
                )
                .toList: _*
            ),
        ),
        MuiDialogActions()(
          MuiButton()(^.onClick --> p.onCancel, "Cancel"),
          MuiButton()(
            ^.disabled := limitTo.value.isEmpty,
            ^.onClick --> p.onStart(BuildMachinesOpts(leaveUnbuilt.value, limitTo.value)),
            "Start",
          ),
        ),
      )
    }

  def apply(onCancel: RTask[Unit], onStart: (BuildMachinesOpts) => RTask[Unit]): Unmounted[Props] = Component(
    Props(onCancel, onStart)
  )

}
