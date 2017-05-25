package examples.crv15.oclr.benchmark1

import rete._

/*
 *  The pattern states that whenever event d occurs, event c must 
 *  have occurred before and the time distance from the occurrence 
 *  of event c must be at least 100 time units. 
 *  
 *  Spec: globally c preceding at least 100 tu d;
 *  
 *  Events:
 *  
 *  - c(time)
 *  - d(time)
 */


class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map()
    map += 'kind -> Symbol(record.get(0))
    map += 'one -> record.get(1).substring(1).toInt
    addMapEvent(map)
  }
}

class M extends CSVMonitor {
  val c, d = event
  val C = fact

  "r1" -- c('time) & not(C('_)) |->
    insert(C('time))

  "r2" -- C('time1) & d('time2) |->
    ensure("time difference >= 100")('time2 - 'time1 >= 100)

  "r3" -- d('_) & not(C('_)) |->
    fail("d occurs without preceeding c")
}

object Evaluate extends MonitorFeeder {
  val a = 'a
  val b = 'b
  val c = 'c
  val d = 'd
  val e = 'e

  val trace1 = List(
    a(10),
    c(100),
    b(120),
    d(210),
    d(240))

  val trace_1 = List(
    a(10),
    b(120),
    d(150),
    c(210),
    d(240))

  val trace_2 = List(
    a(10),
    c(100),
    b(120),
    d(150),
    d(240))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_2 foreach m.addMapEvent
    m.terminate()
  }
}
