
trait Case[P, A] {
  type Result
  def apply(a: A): Result
}

trait Poly {
  def apply[A](arg: A)(implicit cse: Case[this.type, A]): cse.Result = cse.apply(arg)
}

object myPloy extends Poly {
  implicit def intCase: Case[this.type, Int] = new Case[this.type, Int] {
    override type Result = Double

    override def apply(a: Int): Double = a / 2.0
  }

  implicit def stringCase: Case[this.type, String] = new Case[this.type, String] {
    override type Result = Int

    override def apply(a: String): Int = a.length
  }
}

object PolyTypeMain extends App {
  myPloy.apply(123)
}

