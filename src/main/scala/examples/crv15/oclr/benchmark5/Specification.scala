package examples.crv15.oclr.benchmark5

import rete._

/*
 * OCLR property 5 This property states that event b should not occur 
 * more than 5 times between the second occurrence of event a and the 
 * instant that is at least 100 time units before an occurrence of event c. 
 * 
 * More detailed formulation:
 * This property can be expressed by the between-and scope and the existence pattern. 
 * The scope of this expression selects the trace segment from the second occurrence of 
 * event a (not included) and the instant that is at least 100 time units (included) before 
 * the next occurrence of event c. The pattern states that within the segment, the number 
 * of occurrences of event b should not exceed 5.
 * 
 * Spec: between 2 a and c at least 100 tu eventually at most 5 b
 * 
 * Events:
 * 
 * - a(time)
 * - b(time)
 * - c(time) 
 * - d(time) - occurs in example trace, but not mentioned as event
 * - e(time) - occurs in real trace, but not mentioned as event
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

class M1 extends Monitor {
  val a, b, c = event
  val A, B, BPlus = fact

  "r1" -- a('_) & not(A('_)) |-> insert(A(1))

  "r2" -- A(1) & a('_) |-> update(A(2))

  "r3" -- A(2) & b('_) & not(B('_)) |-> insert(B(1))

  "r4" -- B('count) & not(BPlus('_)) & b('time) |-> {
    if ('count < 5)
      update(B('count + 1))
    else
      insert(BPlus('time))
  }

  "r5" -- BPlus('time1) & c('time2) |->
    avoid("5 b's occurring within scope")('time1 <= 'time2 - 100)
}

class M extends CSVMonitor {
  val a, b, c = event
  val Init, A, B, BPlus = fact

  "r1" -- Init() & a('_) |-> replace(Init)(A(1))

  "r2" -- A(1) & a('_) |-> update(A(2))

  "r3" -- A(2) & b('_) |-> replace(A)(B(1))

  "r4" -- B('count) & b('time) |-> {
    if ('count < 5)
      update(B('count + 1))
    else
      replace(B)(BPlus('time))
  }

  "r5" -- BPlus('time1) & c('time2) |->
    avoid("5 b's occurring within scope")('time1 <= 'time2 - 100)

  addFact(Init)()
}

object Evaluate extends MonitorFeeder {
  val a = 'a
  val b = 'b
  val c = 'c
  val d = 'd

  val trace1 = List(
    a(10),
    a(100),
    b(200),
    b(400),
    c(800),
    c(1000),
    d(1100),
    d(1200),
    d(1400),
    d(1500))

  // no segment is selected from the trace by the scope between 2 a and c at least 100 tu
  val trace2 = List(
    a(10),
    a(100),
    b(200),
    b(400),
    b(800),
    b(1000),
    d(1100),
    d(1200),
    d(1400),
    d(1500))

  // my own trace
  val trace3 = List(
    a(10),
    a(100),
    b(200),
    b(400),
    b(800),
    b(1000),
    b(1001),
    b(1002),
    d(1100),
    d(1200),
    d(1400),
    d(1500))

  val trace_1 = List(
    a(10),
    a(100),
    b(200),
    b(400),
    b(800),
    b(1000),
    d(1100),
    b(1200),
    b(1400),
    c(1500))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace3 foreach m.addMapEvent
    m.terminate()
  }
}
