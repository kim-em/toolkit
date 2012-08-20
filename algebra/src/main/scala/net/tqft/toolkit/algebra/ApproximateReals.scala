package net.tqft.toolkit.algebra


trait ApproximateReals[A] extends ApproximateField[A] {
  def fromInteger[I:IntegerModel](i: I): A = {
    i match {
      case i: Int => fromInt(i)
      case i => fromBigDecimal(BigDecimal(implicitly[IntegerModel[I]].toBigInt(i)))
    }
  }
  def fromDouble(x: Double): A
  def fromBigDecimal(x: BigDecimal): A
  def setPrecision(x: A): A
  def bigDecimalValue(x: A): BigDecimal
}

trait ImplicitApproximateReals extends ImplicitOrderedFields {
  override implicit val Doubles: ApproximateReals[Double] = Gadgets.Doubles
}

object ApproximateReals extends ImplicitApproximateReals
