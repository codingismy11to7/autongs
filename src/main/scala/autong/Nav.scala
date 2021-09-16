package autong

import autong.Selectors.{currentPageName, findSideNav}
import japgolly.scalajs.react.RTask
import zio.ZIO
import zio.duration._

import java.time.Duration

object Nav {

  def navToPage(pageName: String): RTask[Unit] =
    findSideNav(pageName).flatMap(_.navButton).map(_.btn).optional.flatMap {
      case Some(btn) =>
        def check(retryTime: Duration = 10.millis): RTask[Unit] =
          (ZIO.sleep(retryTime) *> ZIO.yieldNow *> check(retryTime * 2))
            .unlessM(currentPageName.optional.map(_ contains pageName))

        RTask(btn.click()) *> check()

      case None => ZIO.fail(new Exception(s"Couldn't find $pageName"))
    }

}
