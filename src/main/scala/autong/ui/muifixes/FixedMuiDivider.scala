package autong.ui.muifixes

import io.kinoplan.scalajs.react.bridge.{ReactBridgeComponent, WithPropsNoChildren}
import io.kinoplan.scalajs.react.material.ui.core.MuiDivider.autoNoChildren
import io.kinoplan.scalajs.react.material.ui.core.{ComponentPropType, MuiDividerExtensions}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object FixedMuiDivider extends ReactBridgeComponent with MuiDividerExtensions with FixedMuiDividerExtensions {
  override protected lazy val componentValue: js.Function = RawComponent

  @JSImport("@material-ui/core", "Divider")
  @js.native
  object RawComponent extends js.Function

  def apply(
      absolute: js.UndefOr[Boolean] = js.undefined,
      classes: js.UndefOr[Map[ClassKey.Value, String]] = js.undefined,
      component: js.UndefOr[ComponentPropType] = js.undefined,
      flexItem: js.UndefOr[Boolean] = js.undefined,
      orientation: js.UndefOr[Orientation.Value] = js.undefined,
      inset: js.UndefOr[Boolean] = js.undefined,
      light: js.UndefOr[Boolean] = js.undefined,
      variant: js.UndefOr[Variant.Value] = js.undefined,
  ): WithPropsNoChildren = autoNoChildren

}

trait FixedMuiDividerExtensions {

  object Orientation extends Enumeration {
    type Value = String

    val vertical   = "vertical"
    val horizontal = "horizontal"
  }

}
