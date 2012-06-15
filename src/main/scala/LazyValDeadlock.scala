// 'not-a-bug', per https://issues.scala-lang.org/browse/SI-5808

object LazyValDeadLock extends App {
  lazy val X = 0
  lazy val Y = {
    // 'mentioning' X here solves the problem:
    // X
    for (i <- 0 until 2 par) yield  {
      println(i)
      X
    }
  }
  println(Y)
}







