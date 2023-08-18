package autong.ui.muifixes

import io.kinoplan.scalajs.react.bridge.{ReactBridgeComponent, WithProps}
import io.kinoplan.scalajs.react.material.ui.core.{ComponentPropType, ComponentRefType, MuiButtonExtensions}
import japgolly.scalajs.react.vdom.VdomNode

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object FixedMuiButton extends ReactBridgeComponent with MuiButtonExtensions {
  override protected lazy val componentValue: js.Function = RawComponent

  @JSImport("@material-ui/core", "Button")
  @js.native
  object RawComponent extends js.Function

  def apply(
      classes: js.UndefOr[Map[ClassKey.Value, String]] = js.undefined,
      color: js.UndefOr[Color.Value] = js.undefined,
      component: js.UndefOr[ComponentPropType] = js.undefined,
      disableFocusRipple: js.UndefOr[Boolean] = js.undefined,
      disableRipple: js.UndefOr[Boolean] = js.undefined,
      endIcon: js.UndefOr[VdomNode] = js.undefined,
      startIcon: js.UndefOr[VdomNode] = js.undefined,
      fullWidth: js.UndefOr[Boolean] = js.undefined,
      mini: js.UndefOr[Boolean] = js.undefined,
      size: js.UndefOr[Size.Value] = js.undefined,
      variant: js.UndefOr[Variant.Value] = js.undefined,
      buttonRef: js.UndefOr[ComponentRefType] = js.undefined,
      action: js.UndefOr[js.Function] = js.undefined,
      centerRipple: js.UndefOr[Boolean] = js.undefined,
      disableTouchRipple: js.UndefOr[Boolean] = js.undefined,
      focusRipple: js.UndefOr[Boolean] = js.undefined,
      focusVisibleClassName: js.UndefOr[String] = js.undefined,
      onFocusVisible: js.UndefOr[js.Function] = js.undefined,
      TouchRippleProps: js.UndefOr[js.Object] = js.undefined,
  ): WithProps = auto

}
