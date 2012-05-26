package net.tqft.toolkit.arithmetic.ecm

object Factor extends App {

  apply(BigInt("3284464815846549089237979195192428016693977093"))

  def apply(i: BigInt): List[BigInt] = factor(i.toString).map(BigInt(_))
  def apply(i: Long): List[Long] = factor(i.toString).map(_.toLong)
  def apply(i: Int): List[Int] = factor(i.toString).map(_.toInt)
  
  private def factor(i: String): List[String] = {
    try {
      BigInt(i)
    } catch {
      case _ => throw new NumberFormatException("Could not parse " + i + " as a BigInt")
    }
    
    if (i == "0") {
      List(i)
    } else if(i == "1") {
      List()
    } else
    {
      val originalHeadlessState = System.getProperty("java.awt.headless")
      System.setProperty("java.awt.headless", "true")

      val ecmApplet = new ecm()
      ecmApplet.init
      ecmApplet.textNumber = i.toString
      ecmApplet.run

      if (originalHeadlessState == null) {
        System.clearProperty("java.awt.headless")
      } else {
        System.setProperty("java.awt.headless", originalHeadlessState)
      }

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