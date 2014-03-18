package net.tqft.toolkit.algebra.matrices

object EmptyBucket extends App {
  val bucket = net.tqft.toolkit.amazon.S3("positive-symmetric-decompositions")
  println(bucket.size)
  bucket.clear
}