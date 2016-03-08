package net.tqft.toolkit.algebra.khovanov

/**
 * @author scott
 */
object KhovanovHomology extends Functor[Tangle, Cobordism, Complexes[GradedSmoothing, LinearComboCan], ChainMaps[GradedSmoothing, LinearComboCan]] {
  private val cobordisms = implicitly[TensorCategory[Tangle, Cobordism]]
  private implicit def S = SurfacesModRelations
  private type O = Complexes[GradedSmoothing, LinearComboCan]
  private type M = ChainMaps[GradedSmoothing, LinearComboCan]

  private val complexes = implicitly[AdditiveTensorCategory[O, M]]
  val simplification = {
    // summon Gaussian elimination!
    implicitly[Simplification[Complexes[GradedSmoothing, LinearComboCan], ChainMaps[GradedSmoothing, LinearComboCan]]]
  }

  override def applyToObject(t: Tangle) = {
    // divide and conquer, simplifying along the way
    require(t.arcs.isEmpty) // TODO, handle arcs, after the crossings
    t.crossings.foldLeft(empty)({
      case (complex, crossing) => {
        val newComplex = complexes.tensorObjects(complex, applyToCrossing(crossing))
        simplification.fullSimplify(newComplex)._1.target
      }
    })
  }

  private def applyToCrossing(c: Crossing): O = ???
  private def applyToArc(a: Arc): O = ???
  private val empty: O = Seq(Complex(0, Seq(Seq(GradedSmoothing.empty)), Seq.empty))

  override def applyToMorphism(c: Cobordism) = {
    val most = c.frames.foldLeft(complexes.identity(empty))({
      case (mapSoFar, frame) => simplification.simplifySource(complexes.compose(mapSoFar, applyToElementaryCobordism(frame)))
    })
    // FIXME: identify any arcs that are not touched by any frame, and tensor on their identities
    most
  }

  private lazy val findIsomorphism = FindIsomorphism.using(simplification)
  private def applyToElementaryCobordism(c: ElementaryCobordism): M = {
    // FIXME: absurdly, this recalculates the Reidemeister moves every time they're needed! cache them appropriately
    val source = applyToObject(c.source)
    val target = applyToObject(c.target)
    val (sf, sg) = simplification.fullSimplify(source)
    val (tf, tg) = simplification.fullSimplify(target)
    ???
  }

}