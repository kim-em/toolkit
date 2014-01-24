package net.tqft.wiki

import org.openqa.selenium.WebDriver
import net.tqft.toolkit.Logging
import scala.io.Source
import org.openqa.selenium.By
import org.openqa.selenium.JavascriptExecutor
import net.tqft.toolkit.Throttle

trait WikiMap extends scala.collection.mutable.Map[String, String] {
  def wikiScriptURL: String

  var _username: String = null
  var _password: String = null

  private var throttle: Throttle = Throttle.none
  
  def login(username: String, password: String) {
    // TODO error handling?
    
    _username = username
    _password = password
    driver.get(wikiScriptURL + "?title=Special:UserLogin")
    val name = driver.findElement(By.id("wpName1"))
    name.clear()
    name.sendKeys(username)
    val pass = driver.findElement(By.id("wpPassword1"))
    pass.clear()
    pass.sendKeys(password)
    driver.findElement(By.id("wpLoginAttempt")).click
  }
  
  def setThrottle(millis: Int) = {
    throttle = Throttle.rateLimited(millis)
  }
  
  private def driver = FirefoxDriver.driverInstance
  private def actionURL(title: String, action: String) = {
    wikiScriptURL + "?title=" + java.net.URLEncoder.encode(title, "UTF-8") + "&action=" + action
  }

  // Members declared in scala.collection.MapLike 
  override def get(key: String): Option[String] = try {
    Some(Source.fromURL(actionURL(key, "raw")).getLines().mkString("\n"))
  } catch {
    case e: java.io.FileNotFoundException => None
    case e: Exception =>
      Logging.error("Exception while loading wiki page " + key, e)
      None
  }
  override def iterator: Iterator[(String, String)] = ???

  // Members declared in scala.collection.mutable.MapLike 
  override def -=(key: String) = {
    driver.get(actionURL(key, "delete"))
    driver.findElement(By.id("wpConfirmB")).click
    this
  }
  override def +=(kv: (String, String)) = {
    if (get(kv._1) != Some(kv._2)) {
      try {
        throttle(false)
        driver.get(actionURL(kv._1, "edit"))
        driver.asInstanceOf[JavascriptExecutor].executeScript("document.getElementById('" + "wpTextbox1" + "').value = \"" + kv._2.replaceAllLiterally("\n", "\\n").replaceAllLiterally("\"", "\\\"") + "\";");
        driver.findElement(By.id("wpSave")).click
      } catch {
        case e: Exception => {
          Logging.warn("Exception while editing wiki page: ", e)
          FirefoxDriver.quit
          if (_username != null) {
            login(_username, _password)
          }

          +=(kv)
        }
      }
    }
    this
  }
}

object WikiMap {
  def apply(wikiScriptURL: String): WikiMap = {
    val _wikiScriptURL = wikiScriptURL
    new WikiMap {
      override val wikiScriptURL = _wikiScriptURL
    }
  }
}

object FirefoxDriver {
  private var driverOption: Option[WebDriver] = None

  def driverInstance = {
    if (driverOption.isEmpty) {
      Logging.info("Starting Firefox/webdriver")
      //      val profile = new FirefoxProfile();
      //      profile.setPreference("network.proxy.socks", "localhost");
      //      profile.setPreference("network.proxy.socks_port", "1081");
      //      profile.setPreference("network.proxy.type", 1)
      driverOption = Some(new org.openqa.selenium.firefox.FirefoxDriver( /*profile*/ ))
      Logging.info("   ... finished starting Firefox")
    }
    driverOption.get
  }

  def quit = {
    driverOption.map(_.quit)
    driverOption = None
  }

}