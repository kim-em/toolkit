package net.tqft.toolkit.arithmetic

object Factor {
  def apply(i: BigInt): List[BigInt] = byECM(i.toString).map(BigInt(_))
  def apply(i: Long): List[Long] = byECM(i.toString).map(_.toLong)
  def apply(n: Int): List[Int] = {
    if (n.abs < 3000000) {
      bySieve(n)
    } else {
      byECM(n)
    }
  }

  def bySieve(n: Int): List[Int] = {
    if (n < 0) {
        -1 :: apply(-n)
      } else if (n == 0) {
        List(0)
      } else {
        impl(n, Nil, Primes.iterator).flatMap({ case (1, k) => Seq(); case (p, k) => Seq.fill(k)(p) })
      }
  }
  def byECM(n: Int): List[Int] = byECM(n.toString).map(_.toInt)
  
  @scala.annotation.tailrec
  private def impl(n: Int, previousFactors: List[(Int, Int)], primes: Iterator[Int]): List[(Int, Int)] = {
    val p = primes.next
    if (p * p > n) {
      ((n, 1) :: previousFactors).reverse
    } else {
      var m = n
      var k = 0
      while (m % p == 0) {
        k = k + 1
        m = m / p
      }
      impl(m, (p, k) :: previousFactors, primes)
    }
  }

  private lazy val ECMAppletCache = scala.collection.mutable.Map[Thread, ecm]()
  private def getECMApplet = {
    val currentThread = Thread.currentThread
    if (!ECMAppletCache.contains(currentThread)) {
      val newECM = new ecm()
      newECM.init
      ECMAppletCache.put(currentThread, newECM)
    }
    ECMAppletCache(currentThread)
  }

  private def byECM(i: String): List[String] = {
//    try {
//      BigInt(i)
//    } catch {
//      case _ => throw new NumberFormatException("Could not parse " + i + " as a BigInt")
//    }

    if (i == "0") {
      List(i)
    } else if (i == "1") {
      List()
    } else {
      //      val originalHeadlessState = System.getProperty("java.awt.headless")
      //      System.setProperty("java.awt.headless", "true")

      val ecmApplet = getECMApplet
      ecmApplet.textNumber = i.toString
      //      ecmApplet.run
      ecmApplet.startNewFactorization(true)

      //      if (originalHeadlessState == null) {
      //        System.clearProperty("java.awt.headless")
      //      } else {
      //        System.setProperty("java.awt.headless", originalHeadlessState)
      //      }

      val output = ecmApplet.upperTextArea.split("\n\n")(0).replace("\n", "").replace(" ", "")
      if (output.endsWith("isprime")) {
        List(output.stripSuffix("isprime").ensuring(_ == i).ensuring(BigInt(_).isProbablePrime(20)))
      } else {
        val List(input: String, answer: String) = output.split("=").toList
        require(input == i.toString)
        def parseExponents(s: String) = {
          if (s.contains('^')) {
            val List(base, exponent) = s.split('^').toList
            List.fill(exponent.toInt)(BigInt(base))
          } else {
            List(BigInt(s))
          }
        }
        val factors = answer.split("(x|\\*)").toList.flatMap(parseExponents)
        require(factors.reduce(_ * _).toString == i)
        factors.map(_.toString)
      }
    }
  }

}