package examples.crv15.oclr.benchmark2

import rete._

/*
 * This property can be expressed using the before scope 
 * and the response pattern. The scope of this expression 
 * selects the trace segment from the beginning to the instant 
 * (included) 1000 time units before the first occurrence of event 
 * c from the trace. The pattern states that within the segment, 
 * whenever event a occurs, event b should occur within 1000 time 
 * units (included) after its occurrence. 
 * 
 * Spec: before c at least 1000 tu b responding at most 1000 tu a;
 * 
 * Events:
 * 
 * - a(time)
 * - b(time)
 * - c(time)
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
  val a, b, c = event
  val A, AB, ScopeDefined = fact

  "r1" -- c('time) & not(ScopeDefined('_)) |->
    insert(ScopeDefined('time - 1000))

  "r2" -- a('time) |-> insert(A('time))

  "r3" -- A('time1) & b('time2) |->
    replace(A)(AB('time1, 'time2))

  "r4" -- A('time) & ScopeDefined('limit) |-> {
    if ('time > 'limit) remove(A)
  }

  "r5" -- AB('time1, 'time2) & ScopeDefined('limit) |->
    ensure("a not in time frame, or b should follow a within time frame")(
        'time1 > 'limit || ('time2 <= 'limit && 'time2 - 'time1 <= 1000))

  "r6" -- END() & A('_) & ScopeDefined('_) |->
    fail("a not followed by b")
}

object Evaluate extends MonitorFeeder {
  val a = 'a
  val b = 'b
  val c = 'c

  val trace1 = List(
    a(10),
    b(100),
    b(600),
    c(1200),
    b(1500))

  // no segment is selected from the trace by the scope before c at least 1000 tu
  val trace2 = List(
    a(10),
    b(100),
    c(600),
    b(1200),
    a(1500))

  // no event c occurs
  val trace3 = List(
    a(10),
    b(100),
    b(600),
    b(1200),
    a(1500))

  // there is no event b occurring after the first event a within the segment selected by before c at least 1000 tu
  val trace_1 = List(
    a(10),
    a(100),
    b(600),
    c(1200),
    b(1500))

  // there is no event b occurring within 1000 time units after the last event a
  val trace_2 = List(
    a(10),
    a(100),
    b(1200),
    a(1500),
    c(4000))

  // the distance between the first event a and the first event b is greater than 1000 time units
  val trace_3 = List(
    a(10),
    a(100),
    b(1200),
    b(1500),
    c(4000))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_3 foreach m.addMapEvent
    m.terminate()
  }
}
