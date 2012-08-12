package net.tqft.toolkit.algebra

trait NumberField[A] extends Field[Polynomial[A]] {
  def coefficientField: Field[A]
  val generator: Polynomial[A]
  lazy val degree = generator.maximumDegree.get

  protected lazy val polynomials = Polynomials.over(coefficientField) // has to be lazy so coefficientField is available

  private val powers = {
    import net.tqft.toolkit.functions.Memo._
    def f(n: Int) = polynomials.remainder(polynomials.monomial(n), generator)
    (f _).memo
  }
  def normalize(q: Polynomial[A]) = {
    q.maximumDegree match {
      case None => zero
      case Some(k) if k < degree => q
      case Some(k) if k < 2 * degree => {
        Polynomial((q.terms.flatMap {
          case (n, a) if n < degree => List((n, a))
          case (n, a) => powers(n).terms.map { case (m, b) => (m, coefficientField.multiply(a, b)) }
        }): _*)(coefficientField)
      }
      case _ => polynomials.remainder(q, generator)
    }
  }

  override def fromInt(x: Int) = polynomials.fromInt(x)
  override def inverse(q: Polynomial[A]) = {
    if (q == zero) throw new ArithmeticException("/ by zero")
    val (_, b, u) = (polynomials.extendedEuclideanAlgorithm(generator, q))
    require(u.maximumDegree == Some(0))
    scalarMultiply(coefficientField.inverse(u.constantTerm(coefficientField)), b)
  }
  override def negate(q: Polynomial[A]) = polynomials.negate(q)
  override lazy val zero = polynomials.zero
  override lazy val one = polynomials.one
  override def multiply(a: Polynomial[A], b: Polynomial[A]) = normalize(polynomials.multiply(a, b))
  override def add(a: Polynomial[A], b: Polynomial[A]) = polynomials.add(a, b)

  def scalarMultiply(a: A, p: Polynomial[A]): Polynomial[A] = polynomials.scalarMultiply(a, p)
}

object NumberField {
  def apply[A: Field](p: Polynomial[A]): NumberField[A] = new NumberField[A] {
    override val generator = p
    override val coefficientField = implicitly[Field[A]]
  }

  def cyclotomic[A: Field](n: Int): CyclotomicNumberField[A] = new CyclotomicNumberField[A] {
    override val order = n
    override val coefficientField = implicitly[Field[A]]
  }
}

