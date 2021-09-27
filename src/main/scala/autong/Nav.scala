package autong

import autong.UIInterface.{currentPageName, findSideNav}
import zio._

import java.time.Duration

object Nav {

  def navToPage(pageName: String): RIO[Has[Clock] with Has[UIInterface], Unit] =
    findSideNav(pageName).flatMap(_.navButton).unsome.flatMap {
      case Some(btn) =>
        def check(retryTime: Duration = 10.millis): RIO[Has[Clock] with Has[UIInterface], Unit] =
          (ZIO.yieldNow.delay(retryTime) *> check(retryTime * 2))
            .unlessZIO(currentPageName.unsome.map(_ contains pageName))
            .unit

        btn.click *> check()

      case None => ZIO.fail(new Exception(s"Couldn't find $pageName"))
    }

}
