package japgolly.scalajs

import japgolly.scalajs.react.util.EffectZIO
import zio.ZIO
import zio.prelude.Equal
import zio.prelude.fx.ZPure

import scala.scalajs.js

package object react extends japgolly.scalajs.react.internal.Core with japgolly.scalajs.react.ReactZIO {
  type RPure[+A] = EffectZIO.RPure[A]
  type RTask[+A] = EffectZIO.RTask[A]

  final val RT: RTask[Unit] = ZIO.unit
  final val RP: RPure[Unit] = ZPure.unit

  def RTask[T](t: => T): RTask[T] = ZIO(t)
  def RPure[T](t: => T): RPure[T] = ZPure.attempt(t)

  implicit def eqUndefOr[T: Equal]: Equal[js.UndefOr[T]] = Equal.make { (a, b) =>
    import zio.prelude.EqualOps
    (js.isUndefined(a) && js.isUndefined(b)) || a.fold(false)(a => b.fold(false)(b => a === b))
  }

  import scala.language.implicitConversions
  implicit def pure2zio[A](p: RPure[A]): RTask[A] = p.toZIO
}
