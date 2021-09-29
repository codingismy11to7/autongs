package autong

import zio.console.{putStrLn, Console}
import zio._

object TestNotifications {
  val empty: ULayer[Has[Notifications]] = ZLayer.succeed(_ => UIO.unit)

  val console: URLayer[Console, Has[Notifications]] =
    ZLayer.fromFunction(c => (notification: String) => putStrLn(notification).orDie.provide(c))

}
