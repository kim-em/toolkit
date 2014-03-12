package net.tqft.toolkit.wiki

import org.openqa.selenium.WebDriver
import net.tqft.toolkit.Logging
import scala.io.Source
import org.openqa.selenium.By
import org.openqa.selenium.JavascriptExecutor
import net.tqft.toolkit.Throttle
import scala.slick.driver.MySQLDriver.simple._
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import scala.slick.lifted.ProvenShape.proveShapeOf
import scala.slick.profile.RelationalProfile.SimpleQL.Table

trait WikiMap extends scala.collection.mutable.Map[String, String] {
  private class Revision(tag: Tag, tablePrefix: String) extends Table[(Int, Int)](tag, tablePrefix + "revision") {
    def rev_id = column[Int]("rev_id", O.PrimaryKey)
    def rev_text_id = column[Int]("rev_text_id")
    def * = (rev_id, rev_text_id)
  }
  private class Text(tag: Tag, tablePrefix: String) extends Table[(Int, String)](tag, tablePrefix + "text") {
    def old_id = column[Int]("old_id", O.PrimaryKey)
    def old_text = column[String]("old_text")
    def * = (old_id, old_text)
  }
  private class Page(tag: Tag, tablePrefix: String) extends Table[(Int, String, Int)](tag, tablePrefix + "page") {
    def page_id = column[Int]("page_id", O.PrimaryKey)
    def page_title = column[String]("page_title")
    def page_latest = column[Int]("page_latest")
    def * = (page_id, page_title, page_latest)
  }

  private def Revisions = TableQuery(tag => new Revision(tag, _tablePrefix))
  private def Texts = TableQuery(tag => new Text(tag, _tablePrefix))
  private def Pages = TableQuery(tag => new Page(tag, _tablePrefix))

  def wikiScriptURL: String

  private[this] var _username: String = null
  private[this] var _password: String = null
  private[this] var _jdbc: String = null
  private[this] var _tablePrefix = ""

  var throttle: Throttle = Throttle.linearBackoff(10000)

  def login(username: String, password: String) {
    _username = username
    _password = password
    try {
      driver.get(wikiScriptURL + "?title=Special:UserLogin")
      val name = driver.findElement(By.id("wpName1"))
      name.clear()
      name.sendKeys(username)
      val pass = driver.findElement(By.id("wpPassword1"))
      pass.clear()
      pass.sendKeys(password)
      driver.findElement(By.id("wpLoginAttempt")).click
      throttle(true)
    } catch {
      case e: Exception => {
        Logging.warn("Exception while trying to log in: ", e)
        FirefoxDriver.quit
        throttle(false)
        login(username, password)
      }
    }
  }

  def setThrottle(millis: Int) = {
    throttle = Throttle.rateLimited(millis)
  }

  def enableSQLReads(jdbcConnectionString: String, tablePrefix: String = "") = {
    val r1 = get("Main Page")
    Logging.info("Verifying jdbc connection string...")

    _jdbc = jdbcConnectionString
    _tablePrefix = tablePrefix

    if (!_jdbc.contains("useUnicode")) {
      _jdbc = _jdbc + "&useUnicode=true&characterEncoding=utf-8"
    }

    if (get("Main Page") != r1) {
      Logging.warn("... could not read 'Main Page' via jdbc")
      _jdbc = null
      _tablePrefix = ""
    }
  }

  private var driverFactory: Driver = HtmlUnitDriver
  
  def useHtmlUnit {
    driverFactory = HtmlUnitDriver
  }
  def useFirefox {
    driverFactory = FirefoxDriver
  }
  
  private def driver = driverFactory.driverInstance
  private def actionURL(title: String, action: String) = {
    wikiScriptURL + "?title=" + java.net.URLEncoder.encode(title, "UTF-8") + "&action=" + action
  }

  // Members declared in scala.collection.MapLike 
  override def get(key: String): Option[String] = {
    val result = (if (_jdbc != null) {
      try {
        import scala.slick.driver.MySQLDriver.simple._
        Database.forURL(_jdbc, driver = "com.mysql.jdbc.Driver") withSession { implicit session =>
          (for (
            p <- Pages;
            if p.page_title === key.replaceAll(" ", "_").stripPrefix("Data:");
            r <- Revisions;
            if r.rev_id === p.page_latest;
            t <- Texts;
            if t.old_id === r.rev_text_id
          ) yield t.old_text).firstOption
        }
      } catch {
        case e: Exception =>
          Logging.error("Exception while reading from SQL: ", e)
          None
      }
    } else {
      None
    }).orElse(
      try {
        Some(Source.fromURL(actionURL(key, "raw")).getLines().mkString("\n"))
      } catch {
        case e: java.io.FileNotFoundException => None
        case e: Exception =>
          Logging.error("Exception while loading wiki page " + key, e)
          None
      })
    Logging.info(key + " ---> " + result)
    result
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
        driver.get(actionURL(kv._1, "edit"))
        driver.asInstanceOf[JavascriptExecutor].executeScript("document.getElementById('" + "wpTextbox1" + "').value = \"" + kv._2.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\n", "\\n").replaceAllLiterally("\"", "\\\"") + "\";");
        driver.findElement(By.id("wpSave")).click
        throttle(true)
      } catch {
        case e: Exception => {
          Logging.warn("Exception while editing wiki page: ", e)
          throttle(false)
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

trait Driver {
  private var driverOption: Option[WebDriver] = None

  def driverInstance = {
    if (driverOption.isEmpty) {
      Logging.info("Starting browser")
      driverOption = Some(createDriver)
      Logging.info("   ... finished starting browser")
    }
    driverOption.get
  }

  protected def createDriver: WebDriver

  def quit = {
    try {
      driverOption.map(_.quit)
    } catch {
      case e: Exception => Logging.warn("Exception while closing browser.", e)
    }
    driverOption = None
  }
}

object HtmlUnitDriver extends Driver {
  override def createDriver = new HtmlUnitDriver()
}

object FirefoxDriver extends Driver {
  override def createDriver = {
    //      val profile = new FirefoxProfile();
    //      profile.setPreference("network.proxy.socks", "localhost");
    //      profile.setPreference("network.proxy.socks_port", "1081");
    //      profile.setPreference("network.proxy.type", 1)
    new org.openqa.selenium.firefox.FirefoxDriver( /*profile*/ )

  }
}
