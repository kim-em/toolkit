package net.tqft.toolkit

import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 

trait Logging {
  val log = LogFactory.getLog(getClass)
  
  private def now = new java.util.Date().toString
  private def thread = Thread.currentThread.getName
  
  def trace(msg: => Any) = if (log.isTraceEnabled) log.trace(now + " " + thread + " " + msg)
  def debug(msg: => Any) = if (log.isDebugEnabled) log.debug(now + " " + thread + " " + msg)
  def info(msg: => Any) = if (log.isInfoEnabled) log.info(now + " " + thread + " " + msg)
  def warn(msg: => Any) = if (log.isWarnEnabled) log.warn(now + " " + thread + " " + msg)
  def error(msg: => Any, e:Throwable) = if (log.isErrorEnabled) log.error(now + " " + thread + " " + msg,e)
  def fatal(msg: => Any, e:Throwable) = if (log.isFatalEnabled) log.fatal(now + " " + thread + " " + msg,e)
}

object Logging extends Logging