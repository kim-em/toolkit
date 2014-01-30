package net.tqft.wiki

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class WikiMapTest extends FlatSpec with ShouldMatchers {

  val wikiScriptURL = "http://tqft.net/mlp/index.php"
  val wm = WikiMap(wikiScriptURL)

  "get" should "load a page" in {
    wm("Main Page") should not be ('empty)
  }

  "+=" should "write content to a page" in {
    wm.login("testbot", "zytopex")
    wm("Sandbox") = "testbot here 1/2"
    wm("Sandbox") should equal("testbot here 1/2")
    wm("Sandbox") = "testbot here\n 2/2"
    wm("Sandbox") should equal("testbot here\n 2/2")
  }

//  "-=" should "delete a page" in {
//    wm.login("testbot", "zytopex")
//    wm("User:Testbot/Sandbox") = "testbot here"
//    wm -= "User:Testbot/Sandbox"
//    wm("User:Testbot/Sandbox") should equal("")
//  }

}

