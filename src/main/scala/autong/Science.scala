package autong

import autong.Buying.buildAllMachines
import autong.Nav.navToPage
import zio.clock.Clock
import zio.{Has, RIO}

object Science {
  val buildAllScience: RIO[Has[UIInterface], Unit] = buildAllMachines()

  val navAndBuildAllScience: RIO[Has[UIInterface] with Clock, Unit] = navToPage("Science") *> buildAllScience
}
