package net.tqft.toolkit.algebra.magma

import java.io.PrintWriter
import java.io.FileWriter
import java.io.File

object SaveSmallGroups extends App {

  val out = new PrintWriter(new FileWriter(new File("src/main/resources/small-groups.txt")))

  val maxOrder = args.headOption.getOrElse("60").toInt

  val output = for (order <- 1 to maxOrder; index <- (1 to Magma.numberOfSmallGroups(order)).par) yield {
    val buffer = new StringBuffer()
    
    def p(s: String) = {
      buffer.append(s + "\n")
      println(s)
    }

    val (group, actions) = Magma.smallGroupWithActions(order, index)
    p(s"SmallGroup($order, $index)")
    for (g <- group.generators) p(g.mkString(","))
    p("")
    for (action <- actions) {
      val elements = action.elements.toSeq.sorted
      for (g <- group.generators) {
        p((for (x <- elements) yield action.act(g, x)).mkString(","))
      }
      p("")
    }
    p("---")
    buffer.toString
  }
  
  for(line <- output) {
    out.println(line)
  }

}