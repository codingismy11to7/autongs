package autong.ui

import autong.{AutoNG, Options, RequiredOptions}
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.Context
import japgolly.scalajs.react.hooks.CustomHook
import org.scalajs.dom
import zio.{Dequeue, UManaged}

object ControllerContext {

  private def createSyncListener[T](subscribe: UManaged[Dequeue[T]])(onItem: (T) => RTask[Unit]) = {
    val startListening = for {
      q <- subscribe
      _ <- q.take.flatMap(onItem).forever.toManaged_
    } yield {}

    val listFiberZ = startListening.useForever.forkDaemon

    RPure(zio.Runtime.default.unsafeRunTask(listFiberZ)).map { frt =>
      RPure(zio.Runtime.default.unsafeRunAsync_(frt.interrupt))
    }
  }

  val ctx: Context[AutoNG] = React.createContext(null)

  val controller: CustomHook[Unit, AutoNG] = CustomHook[Unit].useContext(ctx).buildReturning((_, c) => c)

  val isStarted: CustomHook[Unit, Boolean] = CustomHook[Unit]
    .custom(controller)
    .useState(false)
    .useEffectWithDepsBy((_, controller, _) => controller) { (_, _, started) => controller =>
      createSyncListener(controller.subscribeToStarted)(v => started.setState(v))
    }
    .buildReturning((_, _, started) => started.value)

  val options: CustomHook[Unit, RequiredOptions] = CustomHook[Unit]
    .custom(controller)
    .useState(Options.default)
    .useEffectWithDepsBy((_, controller, _) => controller) { (_, _, options) => controller =>
      createSyncListener(controller.subscribeToOptions)(o => options.setState(o))
    }
    .buildReturning((_, _, options) => options.value)

  val currentNotif: CustomHook[Unit, (Option[String], RPure[Unit])] = CustomHook[Unit]
    .custom(controller)
    .useState(Option.empty[String])
    .useCallbackBy((_, _, notif) => (notStr: String) => RPure(dom.console.log(notStr)) *> notif.setState(Some(notStr)))
    .useCallbackBy((_, _, notif, _) => notif.setState(None))
    .useEffectWithDepsBy((_, controller, _, notifFunc, _) => controller -> notifFunc) { (_, _, _, _, _) =>
      { case (controller, notifFunc) =>
        createSyncListener(controller.subscribeToNotifs)(s => notifFunc(s))
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
