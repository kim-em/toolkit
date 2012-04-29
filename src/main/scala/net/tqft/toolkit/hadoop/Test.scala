package net.tqft.toolkit.hadoop

import com.nicta.scoobi.Scoobi.{ DList, ScoobiApp }

object Test extends ScoobiApp {
  val lines = DList("the quick brown fox jumps over the lazy dog", "scoobi and hadoop are fun")

  val counts: DList[(String, Int)] = lines.flatMap(_.split(" "))
    .map(word => (word, 1))
    .groupByKey
    .combine(_ + _)

  import ScoobiHelper._
  println(counts.hither)
}