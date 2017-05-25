package examples.crv15.oclr.benchmark3

import rete._

/*
 * This property can be expressed using the after scope and the absence pattern. 
 * The scope of this expression selects the trace segment from the second occurrence 
 * of event b (not included) to the end of the trace. The pattern states that within 
 * the segment, no occurrence of event c should be found. 
 * 
 * Spec: after 2 b never c;
 * 
 * Events:
 * 
 * - b
 * - c
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
  val b, c = event
  val B = fact

  "r1" -- b() & not(B('_)) |-> B(1)
  "r2" -- B(1) & b() |-> update(B(2))
  "r3" -- B(2) & c() |-> fail("c follows two bs")
}

object Evaluate extends MonitorFeeder {
  val a = 'a
  val b = 'b
  val c = 'c

  val trace1 = List(
    a(),
    b(),
    a(),
    b(),
    a())

  val trace2 = List(
    a(),
    b(),
    a(),
    c(),
    c())

  val trace_1 = List(
    a(),
    b(),
    a(),
    b(),
    c())

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_1 foreach m.addMapEvent
    m.terminate()
  }
}
