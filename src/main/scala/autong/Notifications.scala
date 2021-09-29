package autong

import zio._

trait Notifications {
  def send(notification: String): UIO[Unit]
}

object Notifications {

  def sendNotification(notif: String): URIO[Has[Notifications], Unit] =
    ZIO.serviceWith[Notifications](_.send(notif))

}
