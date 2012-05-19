object LazyValDeadLock extends App {
  lazy val X = 0
  lazy val Y = {
    for (i <- 0 until 2 par) yield  {
      println(i)
      X
    }
  }
  println(Y)
}







