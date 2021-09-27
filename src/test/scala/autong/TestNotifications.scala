package autong

import zio.Console.printLine
import zio._

object TestNotifications {
  val empty: ULayer[Has[Notifications]] = ZLayer.succeed(_ => UIO.unit)

  val console: URLayer[Has[Console], Has[Notifications]] =
    ZLayer.fromFunction(c => (notification: String) => printLine(notification).orDie.provide(c))

}
