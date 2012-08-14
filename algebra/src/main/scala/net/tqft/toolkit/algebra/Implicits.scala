package net.tqft.toolkit.algebra

object Implicits {
  implicit val integersAsRationals = Gadgets.integersAsRationals
  implicit val bigIntegersAsBigRationals = Gadgets.bigIntegersAsBigRationals
  implicit val integersAsBigInts = Gadgets.integersAsBigInts
}