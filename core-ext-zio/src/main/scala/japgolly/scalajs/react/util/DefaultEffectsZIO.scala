package japgolly.scalajs.react.util

import japgolly.scalajs.react.userdefined.Effects
import japgolly.scalajs.react.util.EffectZIO.{RPure, RTask}

sealed trait DefaultEffectsLowPri extends DefaultEffectsApiLowPri {
  final override type Async[A] = RTask[A]
  @inline implicit final override val Async: EffectZIO.asynczio.type = EffectZIO.asynczio
}

object DefaultEffects extends DefaultEffectsLowPri with DefaultEffectsApi {
  override type Sync[A] = RPure[A]
  @inline implicit override val Sync: EffectZIO.zpure.type = EffectZIO.zpure
}

abstract class EffectFallbacks extends Effects {
  implicit override def sync: Effect.Sync[RPure] = EffectZIO.zpure
  implicit override def zio: Effect.Async[RTask] = EffectZIO.asynczio
}
