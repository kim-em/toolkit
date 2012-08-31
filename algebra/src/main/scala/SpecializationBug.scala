import scala.collection.GenIterable

// This doesn't capture the bug, oh well.

object SpecializationBug {

  trait X[@specialized(Int, Long) A] {
    def f(p: A, q: A): A
    def f(p: A, qs: A*): A = qs.fold(p)(f _)
    def g(p: A, k: Int) = throw new UnsupportedOperationException
  }
  
  trait Y[@specialized(Int, Long) A] extends X[A] {
    def z: A
    def f(ps: GenIterable[A]): A = ps.fold(z)(f _)
  }
  
  object Z extends Y[Int] {
    def z = 0
    def f(p: Int, q: Int) = p + q    
  }
  
  object Q {
    val ps = (0 to 5).toIterable
    Z.f(ps)
    Z.f(1,2)
  }
}