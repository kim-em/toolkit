package net.tqft.toolkit.algebra.khovanov

// This object just exists so that compiler verifies for us that it knows how to construct the relevant tensor categories.
// It's not useful at runtime.
object ImplicitVerification {
  import Types._
  implicit def S = SurfacesModRelations
  implicitly[TensorCategory[O1, M1]]
  implicitly[TensorCategory[O2, M2]]
  implicitly[TensorCategory[O3, M3]]
}