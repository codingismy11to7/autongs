package autong

import autong.UIInterface.{Page, SideNavEntry}
import zio.{Has, Task, ULayer, ZIO, ZLayer}

object TestUIInterface {

  def create(page: Page, sideNavs: Vector[SideNavEntry]): ULayer[Has[UIInterface]] = ZLayer.succeed {
    new UIInterface {
      override def currentPage: ZIO[Any, Option[Throwable], Page] = ZIO.succeed(page).asSomeError

      override def sideNavEntries: Task[Vector[SideNavEntry]] = ZIO.succeed(sideNavs)
    }
  }

}
