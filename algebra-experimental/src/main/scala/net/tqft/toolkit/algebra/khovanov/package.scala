package net.tqft.toolkit.algebra

/**
 * @author scott
 */
package object khovanov {

  type DirectSum[A] = Seq[A]

  type ComplexOver[O, M] = Complex[DirectSum[O], Matrix[O, M]]
  type ChainMapOver[O, M] = ChainMap[DirectSum[O], Matrix[O, M]]

  type Complexes[O, M] = DirectSum[ComplexOver[O, M]]
  type ChainMaps[O, M] = Matrix[ComplexOver[O, M], ChainMapOver[O, M]]

}