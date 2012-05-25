object MinSpeedTest extends App {

  val cmp = implicitly[Ordering[java.lang.Long]]
  
  def randomLongs: Vector[java.lang.Long] = Vector.fill(1000000)(scala.util.Random.nextLong)

  def timing[R](f: => R): (Long, R) = {
    val startTime = System.nanoTime
    val result = f
    ((System.nanoTime - startTime) / 1000000, result)
  }

  def minTiming = { val r = randomLongs; timing(r.min)._1 }
  def reduceLeftTiming = { val r = randomLongs; timing(r.reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y))._1 }
  
  while(true) {
    println((minTiming, reduceLeftTiming))
  }
  
//  def reduceTiming = { val r = randomLongs; timing(r.reduce((x, y) => if (cmp.lteq(x, y)) x else y))._1 }
//  def parMinTiming = { val r = randomLongs.par; timing(r.min)._1 }
//  def parReduceLeftTiming = { val r = randomLongs.par; timing(r.reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y))._1 }
//  def parReduceTiming = { val r = randomLongs.par; timing(r.reduce((x, y) => if (cmp.lteq(x, y)) x else y))._1 }
//
//  def allTiming = {
//    (
//      minTiming,
//      reduceLeftTiming,
//      reduceTiming,
//      parMinTiming,
//      parReduceLeftTiming,
//      parReduceTiming)
//  }
//
//  // warmup
//  print("warming up")
//  for (i <- 0 until 0) {
//    print(".")
//    allTiming
//  }
//  println("")
//
//  for (i <- 0 until 1000) println(allTiming)
}