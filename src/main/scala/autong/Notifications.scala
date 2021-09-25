package autong

import zio.{Has, RIO, Task, ZIO}

trait Notifications {
  def send(notification: String): Task[Unit]
}

object Notifications {

  def sendNotification(notif: String): RIO[Has[Notifications], Unit] =
    ZIO.serviceWith[Notifications](n => n.send(notif))

}
