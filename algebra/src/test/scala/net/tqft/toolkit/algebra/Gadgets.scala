package net.tqft.toolkit.algebra

object Gadgets {
  def additiveMonoidOfIntegers = implicitly[AdditiveMonoid[Int]]
  def additiveGroupOfIntegers = implicitly[AdditiveGroup[Int]]
  def rigOfIntegers = implicitly[Rig[Int]]
  def ringOfIntegers = implicitly[Ring[Int]]
  
  def euclideanRingOfIntegers = implicitly[EuclideanRing[Int]]
  def orderedEuclideanRingOfIntegers = implicitly[OrderedEuclideanRing[Int]]

  def integerModelForIntegers = implicitly[IntegerModel[Int]]
  
  def additiveMonoidOfBigInts = implicitly[AdditiveMonoid[BigInt]]
  def additiveGroupOfBigInts = implicitly[AdditiveGroup[BigInt]]
  def rigOfBigInts = implicitly[Rig[BigInt]]
  def ringOfBigInts = implicitly[Ring[BigInt]]
  
  def euclideanRingOfBigInts = implicitly[EuclideanRing[BigInt]]
  def orderedEuclideanRingOfBigInts = implicitly[OrderedEuclideanRing[BigInt]]
  
  def rigOfRationals = implicitly[Rig[Fraction[Int]]]
  def ringOfRationals = implicitly[Ring[Fraction[Int]]]
  def euclideanRingOfRationals = implicitly[EuclideanRing[Fraction[Int]]]
  def fieldOfRationals = implicitly[Field[Fraction[Int]]]
  def orderedFieldOfRationals = implicitly[OrderedField[Fraction[Int]]]
  
  def fieldOfBigRationals = implicitly[Field[Fraction[BigInt]]]
  def orderedFieldOfBigRationals = implicitly[OrderedField[Fraction[BigInt]]]
  
  def fieldOfDoubles = implicitly[Field[Double]]
  def orderedFieldOfDoubles = implicitly[OrderedField[Double]]
  

}