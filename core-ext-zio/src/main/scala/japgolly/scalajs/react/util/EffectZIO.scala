package japgolly.scalajs.react.util

import zio.prelude.fx.ZPure
import zio.{Runtime, ZEnv, ZIO}

import scala.util.Try

object EffectZIO {

  import japgolly.scalajs.react.util.Effect._

  type RPure[+A] = ZPure[Nothing, Any, Any, ZEnv, Throwable, A]

//  type RPure[+A] = ZIO[ZEnv, Throwable, A]

  implicit object zpure extends Sync.WithDefaults[RPure] {

    override val empty: RPure[Unit] = ZPure.unit

    @inline override def isEmpty[A](f: RPure[A]): Boolean = f eq empty

    @inline override def delay[A](a: => A): RPure[A] = ZPure.attempt(a)

    @inline override def pure[A](a: A): RPure[A] = ZPure.succeed(a)

    @inline override def map[A, B](fa: RPure[A])(f: A => B): RPure[B] = fa.map(f)

    @inline override def flatMap[A, B](fa: RPure[A])(f: A => RPure[B]): RPure[B] = fa.flatMap(f)

    @inline override def runSync[A](fa: => RPure[A]): A =
//      Runtime.default.unsafeRunTask(fa)
      fa.provide(Runtime.default.environment).runEither.fold(throw _, identity)

  }

  type RTask[+A] = ZIO[ZEnv, Throwable, A]

  implicit object asynczio extends Async.WithDefaults[RTask] {
    @inline override def delay[A](a: => A): RTask[A] = ZIO.attempt(a)

    @inline override def pure[A](a: A): RTask[A] = ZIO.succeed(a)

    @inline override def map[A, B](fa: RTask[A])(f: A => B): RTask[B] = fa.map(f)

    @inline override def flatMap[A, B](fa: RTask[A])(f: A => RTask[B]): RTask[B] = fa.flatMap(f)

    override def finallyRun[A, B](fa: RTask[A], runFinally: RTask[B]): RTask[A] =
      fa.either.flatMap { ta =>
        runFinally.either.flatMap { tb =>
          ZIO.fromEither(ta -> tb match {
            case (Right(_), Left(b)) => Left(b)
            case _                   => ta
          })
        }
      }

    override def async[A](fa: Async.Untyped[A]): RTask[A] =
      ZIO.async(callback => fa(ta => () => callback(ZIO.fromTry(ta))))

    override def async_(onCompletion: Sync.Untyped[Unit] => Sync.Untyped[Unit]): RTask[Unit] =
      for {
        p <- delay(JsUtil.newPromise[Unit]())
        _ <- delay(onCompletion(p._2(tryUnit))())
        _ <- fromJsPromise(p._1)
      } yield {}

    override def runAsync[A](fa: => RTask[A]): Async.Untyped[A] =
      f => () => Runtime.default.unsafeRunAsyncWith(fa)(ea => f(ea.toEither.toTry)())

    override def dispatch[A](fa: RTask[A]): Unit = Runtime.default.unsafeRunAsync(fa)
  }

  private lazy val tryUnit = Try(())
}
