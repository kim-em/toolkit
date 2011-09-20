package net.tqft.toolkit.amazon

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class S3Test extends FlatSpec with ShouldMatchers {

  "apply" should "work correctly" in {
	  val bucket = S3("net.tqft.toolkit.test")
	  bucket.put("aaa", "bbb")
	  val size = bucket.keys.size
	  bucket.contains("aaa") should equal(true)
	  bucket("aaa") should equal("bbb")
	  bucket.remove("aaa")
	  bucket.contains("aaa") should equal(false)
	  bucket.keys.size should equal(size - 1)
	  
	  bucket += (("a","b"))
	  bucket += (("b", """x
y
z"""))
  }
  "GZIP" should "work correctly" in {
	  val bucket = S3.GZIP("net.tqft.toolkit.test")
	  bucket.put("ccc", "bbb")
	  val size = bucket.keys.size
	  bucket.contains("ccc") should equal(true)
	  bucket("ccc") should equal("bbb")
	  bucket.remove("ccc")
	  bucket.contains("ccc") should equal(false)
	  bucket.keys.size should equal(size - 1)
  }

}

