package japgolly.scalajs.react

import zio.prelude.Equal

import scala.annotation.nowarn

object ReactZIO extends ReactZIO

@nowarn("cat=unused")
trait ReactZIO {

  @inline implicit final def reactCatsEqKey: Equal[Key] =
    Equal.default

  /*
  @inline implicit final def ReactCatsExtReusabilityObj(e: Reusability.type): X.ReactCatsExtReusabilityObj.type =
    X.ReactCatsExtReusabilityObj

  @inline implicit final def reactCatsReusabilityIor[A: Reusability, B: Reusability]: Reusability[A Ior B] =
    X.reactCatsReusabilityIor

  @inline implicit final def reactCatsProfunctorRefFull[F[_], X]: Profunctor[Ref.FullF[F, *, X, *]] =
    X.reactCatsProfunctorRefFull
   */

}
