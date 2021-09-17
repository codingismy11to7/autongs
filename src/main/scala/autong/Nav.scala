package autong

import autong.Selectors.{currentPageName, findSideNav}
import zio._
import zio.clock.Clock
import zio.duration._

import java.time.Duration

object Nav {

  def navToPage(pageName: String): RIO[Clock, Unit] =
    findSideNav(pageName).flatMap(_.navButton).map(_.btn).optional.flatMap {
      case Some(btn) =>
        def check(retryTime: Duration = 10.millis): RIO[Clock, Unit] =
          (ZIO.yieldNow.delay(retryTime) *> check(retryTime * 2))
            .unlessM(currentPageName.optional.map(_ contains pageName))

        Task(btn.click()) *> check()

      case None => ZIO.fail(new Exception(s"Couldn't find $pageName"))
    }

}
