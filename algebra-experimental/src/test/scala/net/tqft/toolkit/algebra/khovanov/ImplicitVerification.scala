package net.tqft.toolkit.algebra.khovanov

// This object just exists so that compiler verifies for us that it knows how to construct the relevant tensor categories.
// It's not useful at runtime.
object ImplicitVerification {
  implicit def S = SurfacesModRelations

  type O1 = DirectSum[GradedSmoothing]
  type M1 = Matrix[GradedSmoothing, LinearComboCan]
  type O2 = ComplexOver[GradedSmoothing, LinearComboCan]
  type M2 = ChainMapOver[GradedSmoothing, LinearComboCan]
  type O3 = Complexes[GradedSmoothing, LinearComboCan]
  type M3 = ChainMaps[GradedSmoothing, LinearComboCan]

  implicitly[TensorCategory[O1, M1]]
  implicitly[TensorCategory[O2, M2]]
  implicitly[AdditiveTensorCategory[O3, M3]]
}