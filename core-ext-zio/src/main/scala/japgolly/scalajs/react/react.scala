package japgolly.scalajs

import japgolly.scalajs.react.util.EffectZIO
import zio.prelude.Equal
import zio.prelude.fx.ZPure
import zio.{ZEnv, ZIO}

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

  implicit class RichTask[T](val t: RTask[T]) extends AnyVal {
    def purify: RPure[T] = RPure(zio.Runtime.default.unsafeRunTask(t))
  }

  import scala.language.implicitConversions
  implicit def pure2zio[A](p: RPure[A]): RTask[A] = p.toZIO
  /*

  implicit def widenpure[R <: ZEnv, E <: Throwable, A](z: ZPure[Nothing, Nothing, Nothing, R, E, A]): RPure[A] =
    z.asInstanceOf[RPure[A]]
   */

  implicit def widenzio[R <: ZEnv, E <: Throwable, A](z: ZIO[R, E, A]): RTask[A] = z.asInstanceOf[RTask[A]]
}
