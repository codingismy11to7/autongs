package autong.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.hooks.CustomHook
import zio.prelude.Equal

object hooks {

  final def usePrevious[T]: CustomHook[T, Option[T]] = CustomHook[T]
    .useRef[Option[T]](None)
    .useEffectBy((value, ref) => RPure(ref.value = Some(value)))
    .buildReturning((_, ref) => ref.value)

  implicit private def f1r[A, R]: Reusability[(A) => R]       = Reusability.byRef
  implicit private def f2r[A, B, R]: Reusability[(A, B) => R] = Reusability.byRef

  case class ClearStateProps[T](
      value: Option[T],
      stateHolder: Hooks.UseState[Option[T]],
      eqCB: (Option[T], Option[T]) => Boolean,
  )

  def useClearStateOnPropChangeCB[T: Reusability]: CustomHook[ClearStateProps[T], Unit] =
    CustomHook[ClearStateProps[T]]
      .customBy(p => usePrevious[Option[T]](p.value))
      .useEffectWithDepsBy((p, prev) => (p.value, p.stateHolder, p.eqCB, prev.flatten)) { (_, _) =>
        { case (value, stateHolder, eqCB, prev) =>
          if (value.isDefined && !eqCB(value, prev)) stateHolder.withEffect[RPure].setState(None) else RP
        }
      }
      .build

  case class UseStateProps[T](propVal: Option[T], defaultVal: () => T, equalityCB: (Option[T], Option[T]) => Boolean)

  class CustState[T](val value: T, st: Hooks.UseState[Option[T]]) {
    def setState(tOpt: Option[T]): RPure[Unit]             = st.setState(tOpt)
    def modState(f: (Option[T]) => Option[T]): RPure[Unit] = setState(f(Some(value)))
  }

  def useStateFromPropsCB[T: Reusability]: CustomHook[UseStateProps[T], CustState[T]] = CustomHook[UseStateProps[T]]
    .useState(Option.empty[T])
    .customBy((p, editedState) =>
      useClearStateOnPropChangeCB[T].apply(ClearStateProps[T](p.propVal, editedState, p.equalityCB))
    )
    .localValBy { (p, editedState) =>
      val edited            = if (p.equalityCB(p.propVal, editedState.value)) None else editedState.value
      val editedOrFromProps = edited.orElse(p.propVal)
      editedOrFromProps.getOrElse(p.defaultVal())
    }
    .useRefBy((_, edited, value) => new CustState[T](value, edited))
    .customBy((_, _, value, _) => usePrevious.apply(value))
    .localValBy { (p, edited, value, ssRef, prevValue) =>
      if (!p.equalityCB(Some(value), prevValue)) ssRef.value = new CustState[T](value, edited)
      ssRef.value
    }
    .buildReturning((_, _, _, _, _, ss) => ss)

  def useStateFromProps[T: Reusability: Equal]: CustomHook[(Option[T], () => T), CustState[T]] =
    CustomHook[(Option[T], () => T)]
      .useRef {
        val eq = implicitly[Equal[Option[T]]]
        (a: Option[T], b: Option[T]) => eq.equal(a, b)
      }
      .customBy { (p, eqCBRef) =>
        val (propVal, defaultVal) = p
        useStateFromPropsCB[T].apply(UseStateProps[T](propVal, defaultVal, eqCBRef.value))
      }
      .buildReturning((_, _, r) => r)

  /*
  val useIsMounted: CustomHook[Unit, Boolean] = CustomHook[Unit]
    .useRef(true)
    .useEffectBy((_, ref) =>
      RPure {
        putStrLn("unmounting") *>
          RPure(ref.value = false)
      }
    )
    .buildReturning((_, ref) => ref.value)
   */

}
