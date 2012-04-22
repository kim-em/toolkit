package org.fusionatlas.eigenvalues

import net.tqft.toolkit.algebra.Matrix
import net.tqft.toolkit.algebra.Gadgets

object Hadamard12 {

  val n = 12
  val matrix = List(List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), List(1, -1, -1, 1, -1, -1, -1, 1, 1, 1, -1, 1), List(1, 1, -1, -1, 1, -1, -1, -1, 1, 1, 1, -1), List(1, -1, 1, -1, -1, 1, -1, -1, -1, 1, 1, 1), List(1, 1, -1, 1, -1, -1, 1, -1, -1, -1, 1, 1), List(1, 1, 1, -1, 1, -1, -1, 1, -1, -1, -1, 1), List(1, 1, 1, 1, -1, 1, -1, -1, 1, -1, -1, -1), List(1, -1, 1, 1, 1, -1, 1, -1, -1, 1, -1, -1), List(1, -1, -1, 1, 1, 1, -1, 1, -1, -1, 1, -1), List(1, -1, -1, -1, 1, 1, 1, -1, 1, -1, -1, 1), List(1, 1, -1, -1, -1, 1, 1, 1, -1, 1, -1, -1), List(1, -1, 1, -1, -1, -1, 1, 1, 1, -1, 1, -1)).map(_.map(_.toByte))

  val profile = Array.tabulate(n, n, n, n)((i, j, k, l) => ((for (a <- 0 until n) yield {
    matrix(i)(a) * matrix(j)(a) * matrix(k)(a) * matrix(l)(a)
  }).sum / 4).toByte)

   val transfer2 = Matrix(for (a <- 0 until n * n toList) yield {
    val i = a % n
    val j = (a / n) % n
    wrapByteArray(Array.tabulate(n * n)(b => {
      val p = b % n
      val q = (b / n) % n
      (profile(i)(j)(p)(q) * profile(i)(j)(p)(q)).toByte
    }))
  })
  
  lazy val transfer3 = Matrix(for (a <- 0 until n * n * n toList) yield {
    val i = a % n
    val j = (a / n) % n
    val k = (a / (n * n)) % n
    wrapByteArray(Array.tabulate(n * n * n)(b => {
      val p = b % n
      val q = (b / n) % n
      val r = (b / (n * n)) % n
      (profile(i)(j)(q)(p) * profile(j)(k)(r)(q) * profile(k)(i)(p)(r)).toByte
    }))
  })

  lazy val transfer4 = Matrix(for (a <- 0 until n * n * n * n toList) yield {
    val i = a % n
    val j = (a / n) % n
    val k = (a / (n * n)) % n
    val l = a / (n * n * n)
    wrapByteArray(Array.tabulate(n * n * n * n)(b => {
      val p = b % n
      val q = (b / n) % n
      val r = (b / (n * n)) % n
      val s = b / (n * n * n)
      (profile(i)(j)(q)(p) * profile(j)(k)(r)(q) * profile(k)(l)(s)(r) * profile(l)(i)(p)(s)).toByte
    }))
  })
}