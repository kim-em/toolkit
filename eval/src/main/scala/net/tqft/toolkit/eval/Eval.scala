//package net.tqft.toolkit.eval
//
//import scala.collection.mutable.ListBuffer
//import scala.tools.nsc._
//import scala.tools.nsc.interpreter._
//import java.io.PrintWriter
//import java.io.OutputStreamWriter
//import java.io.PipedInputStream
//import java.io.PipedOutputStream
//import scala.io.Source
//import java.io.BufferedReader
//import java.io.Reader
//import java.io.InputStreamReader
//
//trait Eval {
//
//  protected val settings = {
//    val s = new Settings
//
//    s.embeddedDefaults[Eval]    
//    s
//  }
//
//  val pis = new PipedInputStream
//  val reader = new BufferedReader(new InputStreamReader(pis))
//  def newConsoleLines = {
//    val output = new ListBuffer[String]
//    while (reader.ready) {
//      output += reader.readLine()
//    }
//    output.mkString("\n")
//  }
//
//  private lazy val interpreter = new IMain(settings, new PrintWriter(new PipedOutputStream(pis)))
//  private lazy val _history = new ListBuffer[(String, Option[(String, Any)], String)]
//
//  def history = _history.toList
//
//  def outputNames = _history collect { case (_, Some((name, _)), _) => name }
//  def lastOutputName = outputNames.lastOption
//
//  private def extractOutput = interpreter.mostRecentVar match {
//    case name if Some(name) == lastOutputName || name == "" => /* nothing new */ None
//    case name => Some((name, interpreter.valueOfTerm(name).get))
//  }
//
//  def apply(command: String): Option[Any] = eval(command)
//
//  def evalWithNameAndOutput(command: String) = {
//    interpreter.interpret(command)
//    val output = extractOutput
//    val consoleLines = newConsoleLines
//    _history += ((command, output, consoleLines))
//    (output, consoleLines)
//  }
//  def eval(command: String): Option[Any] = evalWithNameAndOutput(command)._1 map { _._2 }
//
//}
//
//object Eval extends Eval