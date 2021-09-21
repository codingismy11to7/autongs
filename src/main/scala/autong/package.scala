import zio.ZIO

package object autong {

  def ZIOfind[R, E, A](as: Iterable[A])(f: (A) => ZIO[R, E, Boolean]): ZIO[R, E, Option[A]] =
    ZIO.collectFirst(as)(a => f(a).map(passes => if (passes) Some(a) else None))

}
