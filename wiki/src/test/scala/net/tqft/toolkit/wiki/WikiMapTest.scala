//package net.tqft.toolkit.wiki
//
//import org.scalatest._
//import org.scalatest.junit.JUnitRunner
//import org.junit.runner.RunWith
//import scala.math._
//
//@RunWith(classOf[JUnitRunner])
//class WikiMapTest extends FlatSpec with Matchers {
//
//  val wikiScriptURL = "http://tqft.net/mlp/index.php"
//  val wm = WikiMap(wikiScriptURL)
//
//  "get" should "load a page" in {
//    wm("Main Page") should not be ('empty)
//  }
//
////  "+=" should "write content to a page" in {
////    wm.login("testbot", "zytopex")
////    wm("Sandbox") = "testbot here 1/2"
////    wm("Sandbox") should equal("testbot here 1/2")
////    wm("Sandbox") = "testbot here\n 2/2"
////    wm("Sandbox") should equal("testbot here\n 2/2")
////  }
//
//  "get" should "load a page via SQL if a jdbc string has been specified" in {
//    wm.enableSQLReads("jdbc:mysql://mysql.tqft.net/tqft_net?user=readonly1&password=foobar1", "mlp_")
//    wm("Main Page") should not be ('empty)
//  }
//  
////  "-=" should "delete a page" in {
////    wm.login("testbot", "zytopex")
////    wm("User:Testbot/Sandbox") = "testbot here"
////    wm -= "User:Testbot/Sandbox"
////    wm("User:Testbot/Sandbox") should equal("")
////  }
//  
//  FirefoxDriver.quit
//}
//
