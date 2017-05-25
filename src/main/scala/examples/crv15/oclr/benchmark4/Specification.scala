package examples.crv15.oclr.benchmark4

import rete._

/*
 * This property can be expressed by the between-and scope and the 
 * universality pattern. The scope of this expression selects all the 
 * trace segments delimited by event a (not included) and event c 
 * (not included). The pattern states that within the segment, 
 * every occurrence of an event should be event b. 
 * 
 * Spec: between a and c always b
 * 
 * Events:
 * 
 *  - a
 *  - b
 *  - c
 *  
 *  The invalid trial trace originally contained a 'd', but I 
 *  have converted that to an 'a' in the trial trace, assuming
 *  there are only 3 kinds of events: a, b and c. I have asked
 *  for clarification on the wiki.
 */

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map()
    map += 'kind -> Symbol(record.get(0))
    addMapEvent(map)
  }
}

class M extends CSVMonitor {
  val a, b, c = event
  val InZone, NoB = fact

  "r1" -- a() & not(InZone()) |-> insert(InZone())
  
  "r2" -- InZone() & a() |-> insert(NoB())  
  
  "r3" -- InZone() & c() |-> remove(InZone)

  "r4" -- NoB() & c() |->
    fail("a non-b detected between a and c")
}

// "r" -- InZone() & d() |-> insert(NoB())

object Evaluate extends MonitorFeeder {
  val a = 'a
  val b = 'b
  val c = 'c
  val d = 'd

  val trace1 = List(
    a(),
    b(),
    c(),
    a(),
    c())

  // no segment is selected from the trace by the scope between a and c 
  val trace2 = List(
    a(),
    b(),
    b(),
    b(),
    a())

  // an event d occurs between the first occurrence of event a and the first occurrence of event c
  val trace_1 = List(
    a(),
    b(),
    a(), // changed to a() from d()
    c(),
    a())

  // an event d occurs between the first occurrence of event a and the first occurrence of event c
  val trace_2 = List(
    a(),
    a(),
    c())    
    
  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_2 foreach m.addMapEvent
    m.terminate()
  }
}
