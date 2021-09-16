package autong

import autong.Buying.buildAllMachines
import autong.Nav.navToPage
import zio.clock.Clock
import zio.{RIO, Task}

object Science {
  val buildAllScience: Task[Unit] = buildAllMachines()

  val navAndBuildAllScience: RIO[Clock, Unit] = navToPage("Science") *> buildAllScience
}
