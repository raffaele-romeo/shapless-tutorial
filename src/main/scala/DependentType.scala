import shapeless.ops.hlist.{IsHCons, Last}
import shapeless.{Generic, HList, HNil}

case class Vec(x: Int, y: Int)
case class Rect(origin: Vec, size: Vec)

trait Second[L <: HList]{
  type Out
  def apply(value: L): Out
}

object Second {
  type Aux[L <: HList, O] = Second[L] {type Out = O}

  def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst
}

object DependentType extends App {

  import Second._
//  implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
//    new Second[A :: B :: Rest] {
//      type Out = B
//      def apply(value: A :: B :: Rest): B =
//        value.tail.head
//    }

  def lastField[A, Repr <: HList](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    last: Last[Repr]
  ): last.Out = last.apply(gen.to(input))

  def getWrappedValue[A, Repr <: HList, Head](in: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, Head, HNil]
  ): Head = gen.to(in).head

  //val second1 = Second[String :: Boolean :: Int :: HNil]

//  second1("foo" :: true :: 123 :: HNil)

  lastField(Rect(Vec(1, 2), Vec(2, 3)))
}
