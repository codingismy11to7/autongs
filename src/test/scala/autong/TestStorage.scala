package autong

import zio.{Has, Ref, Task, ZLayer}

import scala.scalajs.js

object TestStorage {

  lazy val default: ZLayer[Any, Nothing, Has[Storage]] = (for {
    ref <- Ref.make(Map.empty[String, js.Any])
  } yield new Storage {
    override def store(key: String, obj: js.Any): Task[Unit] = ref.update(_ + (key -> obj))
    override def load(key: String): Task[Option[js.Any]]     = ref.map(_.get(key)).get
  }).toLayer

}
