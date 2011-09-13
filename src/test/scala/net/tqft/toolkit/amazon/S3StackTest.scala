package net.tqft.toolkit.amazon

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class S3StackTest extends FlatSpec with ShouldMatchers {

  "S3Stack" should "work correctly" in {
	  val stack = S3Stack("net.tqft.toolkit.test", "stack")
	  stack.push("hello")
	  stack.push("world!")
	  stack.pop should equal (Some("world!"))
	  stack.pop should equal (Some("hello"))
  }


}

