package net.tqft.toolkit

object Extractors {
  object Int {
    def unapply(s: String): Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
  }
  object Long {
    def unapply(s: String): Option[Long] = try {
      Some(s.toLong)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
  }
  object Double {
    def unapply(s: String): Option[Double] = try {
      Some(s.toDouble)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
  }

  import util.matching.Regex

  implicit class RegexContext(sc: StringContext) {
    def r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }
}