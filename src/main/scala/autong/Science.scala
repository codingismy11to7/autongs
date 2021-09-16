package autong

import autong.Buying.buildAllMachines
import autong.Nav.navToPage
import japgolly.scalajs.react.RTask

object Science {
  val buildAllScience: RTask[Unit] = buildAllMachines()

  val navAndBuildAllScience: RTask[Unit] = navToPage("Science") *> buildAllScience
}
