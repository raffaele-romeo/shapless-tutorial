import shapeless._
import shapeless.ops.hlist

trait ProductMapper[A, B, P] {
  def apply(a: A): B
}

object GenericProductMapper {
  implicit def genericProductMapper[A,
    B,
    P <: Poly,
    ARepr <: HList,
    BRepr <: HList](
                     implicit aGen: Generic.Aux[A, ARepr],
                     bGen: Generic.Aux[B, BRepr],
                     mapper: hlist.Mapper.Aux[P, ARepr, BRepr]
                   ): ProductMapper[A, B, P] = { (a: A) =>
    bGen.from(mapper.apply(aGen.to(a)))
  }
}

object MappingClasses {
  implicit class ProductMapperOps[A](a: A) {
    class Builder[B] {
      def apply[P <: Poly](poly: P)(
          implicit pm: ProductMapper[A, B, P]): B = pm.apply(a)
    }

    def mapTo[B]: Builder[B] = new Builder[B]
  }
}

object conversions extends Poly1 {
  implicit val intCase: Case.Aux[Int, Boolean]   = at(_ > 0)
  implicit val boolCase: Case.Aux[Boolean, Int]  = at(if (_) 1 else 0)
  implicit val strCase: Case.Aux[String, String] = at(identity)
}

case class IceCream1(name: String, numCherries: Int, inCone: Boolean)
case class IceCream2(name: String, hasCherries: Boolean, numCones: Int)

object MappingClassesMain extends App {
  import MappingClasses._
  import GenericProductMapper._

  //IceCream1("Sundae", 1, inCone = false).mapTo[IceCream2](conversions)
}
