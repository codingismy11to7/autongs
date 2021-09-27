package autong

import autong.Buying.buildAllMachines
import autong.Nav.navToPage
import zio._

object Science {
  val buildAllScience: RIO[Has[UIInterface], Unit] = buildAllMachines()

  val navAndBuildAllScience: RIO[Has[UIInterface] with Has[Clock], Unit] = navToPage("Science") *> buildAllScience
}
