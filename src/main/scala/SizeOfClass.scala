import shapeless.{Generic, HList, Nat}
import shapeless.ops.{hlist, nat}

trait SizeOf[A] {
  def value: Int
}

object SizeOfClass {
  implicit def genericSizeOf[A, L <: HList, N <: Nat](
      implicit generic: Generic.Aux[A, L],
      size: hlist.Length.Aux[L, N],
      sizeToInt: nat.ToInt[N]): SizeOf[A] = {
    new SizeOf[A] {
      override def value: Int = sizeToInt.apply()
    }
  }

  def sizeOf[A](implicit sizeOf: SizeOf[A]): Int = sizeOf.value
}

object sizeOfMain extends App {
  import SizeOfClass._

  println(sizeOf[IceCream1])
}
