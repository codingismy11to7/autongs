package autong.ui

import autong.AutoNG.{BulkBuyListener, OptionsListener, StartListener}
import autong.{AutoNG, Options, RequiredOptions}
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.Context
import japgolly.scalajs.react.hooks.CustomHook
import org.scalajs.dom

object ControllerContext {
  val ctx: Context[AutoNG] = React.createContext(null)

  val controller: CustomHook[Unit, AutoNG] = CustomHook[Unit].useContext(ctx).buildReturning((_, c) => c)

  val isStarted: CustomHook[Unit, Boolean] = CustomHook[Unit]
    .custom(controller)
    .useState(false)
    .useEffectWithDepsBy((_, controller, _) => controller) { (_, _, started) => controller =>
      val list: StartListener = v => started.setState(v)
      controller.addStartListener(list).as(controller.removeStartListener(list))
    }
    .buildReturning((_, _, started) => started.value)

  val isBulkBuying: CustomHook[Unit, Boolean] = CustomHook[Unit]
    .custom(controller)
    .useState(false)
    .useEffectWithDepsBy((_, controller, _) => controller) { (_, _, buying) => controller =>
      val list: BulkBuyListener = b => buying.setState(b)
      controller.addBulkBuyListener(list).as(controller.removeStartListener(list))
    }
    .buildReturning((_, _, buying) => buying.value)

  val options: CustomHook[Unit, RequiredOptions] = CustomHook[Unit]
    .custom(controller)
    .useState(Options.default)
    .useEffectWithDepsBy((_, controller, _) => controller) { (_, _, options) => controller =>
      val list: OptionsListener = o => options.setState(o)
      controller.addOptionsListener(list).as(controller.removeOptionsListener(list))
    }
    .buildReturning((_, _, options) => options.value)

  val currentNotif: CustomHook[Unit, (Option[String], RPure[Unit])] = CustomHook[Unit]
    .custom(controller)
    .useState(Option.empty[String])
    .useCallbackBy((_, _, notif) => (notStr: String) => RPure(dom.console.log(notStr)) *> notif.setState(Some(notStr)))
    .useCallbackBy((_, _, notif, _) => notif.setState(None))
    .useEffectWithDepsBy((_, controller, _, notifFunc, _) => controller -> notifFunc) { (_, _, _, _, _) =>
      { case (controller, notifFunc) =>
        controller.addNotifListener(notifFunc).as(controller.removeNotifListener(notifFunc))
      }
    }
    .buildReturning((_, _, notif, _, markAsRead) => notif.value -> markAsRead)

  val sendNotif: CustomHook[Unit, (String) => RTask[Unit]] = CustomHook[Unit]
    .custom(controller)
    .useCallbackWithDepsBy((_, cont) => cont) { (_, _) => controller => (notif: String) =>
      controller.sendNotification(notif)
    }
    .buildReturning((_, _, sendNotif) => sendNotif)

}
