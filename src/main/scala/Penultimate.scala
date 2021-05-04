import shapeless._
import shapeless.ops.hlist

trait Penultimate[L] {
  type Out
  def apply(l: L): Out
}

object Penultimate {
  type Aux[L, O] = Penultimate[L] { type Out = O }
  def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p

  implicit def hlistPenultimate[L <: HList, M <: HList, O](
      implicit
      init: hlist.Init.Aux[L, M],
      last: hlist.Last.Aux[M, O]
  ): Penultimate.Aux[L, O] =
    new Penultimate[L] {
      type Out = O
      def apply(l: L): O =
        last.apply(init.apply(l))
    }

  implicit def genericPenultimate[A, R, O](
      implicit
      generic: Generic.Aux[A, R],
      penultimate: Penultimate.Aux[R, O]
  ): Penultimate.Aux[A, O] = new Penultimate[A] {
    override type Out = O

    override def apply(l: A): Out = penultimate.apply(generic.to(l))
  }

  implicit class PenultimateOps[A](a: A) {
    def penultimate(implicit inst: Penultimate[A]): inst.Out =
      inst.apply(a)
  }
}

object PenultimateMain extends App {
  import Penultimate._
  type BigList = String :: Int :: Boolean :: Double :: HNil

  val bigList: BigList = "foo" :: 123 :: true :: 456.0 :: HNil
  bigList.penultimate
  Penultimate[BigList].apply(bigList)
}
