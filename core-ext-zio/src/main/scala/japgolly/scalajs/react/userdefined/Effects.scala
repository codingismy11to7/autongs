package japgolly.scalajs.react.userdefined

import japgolly.scalajs.react.util.EffectZIO.{RPure, RTask}
import japgolly.scalajs.react.util.{Effect, EffectZIO}

abstract class Effects {
  implicit def sync: Effect.Sync[RPure] = EffectZIO.zpure
  implicit def zio: Effect.Async[RTask] = EffectZIO.asynczio
}
