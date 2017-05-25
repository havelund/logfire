package examples.crv15

import rete._
import java.io._
import org.apache.commons.csv._
import scala.collection.JavaConverters._

object Util {
  def time[R](block: => R): R = {
    val t1 = System.currentTimeMillis()
    val result = block
    val t2 = System.currentTimeMillis()
    val ms = (t2 - t1).toFloat
    val sec = ms / 1000
    println()
    println("Elapsed time: " + sec + "s")
    result
  }
}
import Util._

object Main {
  object Options {
    var PRINT_LINENUMBER_EACH = 0
  }

  def setOptions(args: List[String], monitor: Monitor) {
    monitor.HISTORY = false
    monitor.STOP_AFTER_FAIL = true
    readOptions(args, monitor)
  }

  def readOptions(args: List[String], monitor: Monitor) {
    args match {
      case "-witness" :: rest =>
        monitor.HISTORY = true
        readOptions(rest, monitor)
      case "-print" :: rest =>
        monitor.PRINT = true
        readOptions(rest, monitor)
      case "-printevents" :: rest =>
        monitor.PRINT_EVENTS = true
        readOptions(rest, monitor)
      case "-printnr" :: each :: rest =>
        Options.PRINT_LINENUMBER_EACH = each.toInt
        readOptions(rest, monitor)
      case Nil =>
      case _ =>
        error(s"invalid option: ${args.mkString(" ")}")
    }
  }

  def printHelp() {
    println()
    println(s"Correct format is:")
    println()
    println("  logfire <spec name> <file name> <option>*")
    println()
    println("where:")
    println()
    println("  <spec name> ::= <team>.benchmark<number>")
    println("  <team>      ::= marq | rithm | oclr | rvmonitor | breach |  logfire")
    println("  <number>    ::= 1 | 2 | 3 | 4 | 5")
    println("  <option>    ::= -witness | -print | -printevents | -printnr <int>")
    println()
    println("Examples:")
    println()
    println("  logfire oclr.benchmark3 kh/mylogs/oclr/log3.csv")
    println()
    println("  logfire oclr.benchmark3 kh/mylogs/oclr/log3.csv -witness")
    println()
    println("  logfire oclr.benchmark3 kh/mylogs/oclr/log3.csv -witness -print")
    println()
    println("Try again!")
    println()
  }

  def error(msg: String): Nothing = {
    println(s"*** $msg")
    printHelp()
    System.exit(0).asInstanceOf[Nothing]
  }

  def getMonitor(name: String): Monitor = {
    try {
      val monitorName = s"examples.crv15.$name.M"
      Class.forName(monitorName).newInstance().asInstanceOf[Monitor]
    } catch {
      case e =>
        e.printStackTrace()
        error(s"spec file $name not found")
    }
  }

  def main(args: Array[String]) {
    if (args.length < 2)
      error(s"Wrong number of arguments")
    val monitor = getMonitor(args(0))
    val logFile = args(1)
    setOptions(args.toList.tail.tail, monitor)
    val in: Reader = new BufferedReader(new FileReader(logFile))
    val records: Iterable[CSVRecord] = CSVFormat.DEFAULT.withHeader().parse(in).asScala
    time {
      if (Options.PRINT_LINENUMBER_EACH == 0) {
        records foreach monitor.submit
      } else {
        var lineNr = 0
        for (record <- records) {
          lineNr += 1
          if (lineNr % Options.PRINT_LINENUMBER_EACH == 0) {
            if (lineNr >= 1000000)
              println(lineNr.toDouble / 1000000 + " M")
            else
              println(lineNr.toDouble / 1000 + " K")
          }
          monitor.submit(record)
        }
      }
      monitor.terminate()
      in.close()
    }
  }
}
