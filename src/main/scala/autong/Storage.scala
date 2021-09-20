package autong

import autong.Utils.RichStr
import org.scalajs.dom
import zio._

import scala.scalajs.js

trait Storage {
  def store(key: String, obj: js.Any): Task[Unit]
  def load(key: String): Task[Option[js.Any]]
}

object Storage {

  lazy val live: ZLayer[Any, Nothing, Has[Storage]] = ZLayer.succeed {
    new Storage {
      override def store(key: String, obj: js.Any): Task[Unit] =
        Task(dom.window.localStorage.setItem(key, js.JSON.stringify(obj)))
      override def load(key: String): Task[Option[js.Any]] =
        Task(dom.window.localStorage.getItem(key)).map(_.toOption).map(_.map(js.JSON.parse(_)))
    }
  }

  def store(key: String, obj: js.Any): RIO[Has[Storage], Unit] = ZIO.serviceWith[Storage](_.store(key, obj))
  def load(key: String): RIO[Has[Storage], Option[js.Any]]     = ZIO.serviceWith[Storage](_.load(key))
}
