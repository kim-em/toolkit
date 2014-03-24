package net.tqft.toolkit.algebra.numberfields

import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.ComplexConjugation
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.cones.CubicalCone

trait CyclotomicNumberField[A] extends NumberField[A] with ComplexConjugation[Polynomial[A]] { cnf =>
  val order: Int
  override lazy val generator = Polynomial.cyclotomic(order)(coefficients) // has to be lazy so coefficentField is available

  override lazy val galoisGroup = ???
  override lazy val galoisGroupAction = ???

  private def zeta = Polynomial.identity(coefficients)
  private val zetaInversePowers = {
    import net.tqft.toolkit.functions.Memo._
    def f(n: Int) = inverse(power(zeta, n))
    (f _).memo
  }

  def bar(q: Polynomial[A]) = sum(normalize(q).terms.map({ case (k, a) => scalarMultiply(a, zetaInversePowers(k)) }))

  def house_?(p: Polynomial[A]): Boolean = galoisConjugates(p).forall(x => ???)
  def housesUpTo(limit: Double): Iterator[Polynomial[A]] = {
    houseConeApproximation.enumeratePointsBelowWall(???, limit)(this).filter(house_?)
  }

  def houseConeApproximation: CubicalCone[A, Polynomial[A]] = ???

  object RealPart extends NumberField[A] with ComplexConjugation[Polynomial[A]] {
    override def coefficients = cnf.coefficients
    override lazy val generator = cnf.minimalPolynomial(cnf.add(cnf.zeta, cnf.inverse(cnf.zeta)))

    override val galoisGroup = ???
    override val galoisGroupAction = ???

    override def bar(q: Polynomial[A]) = q
  }

}
