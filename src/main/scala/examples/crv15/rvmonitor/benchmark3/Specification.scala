package examples.crv15.rvmonitor.benchmark3

import rete._

/*
 *  In Java programs, the classical "hasNext" property requires an iterator's 
 *  "hasNext" method to be called before calling its "next" method, and that 
 *  "hasNext" must return true. Violation of such property may cause a "NoSuchElementExcepiton" 
 *  at runtime.
 *  
 *  Events:
 *  - hasNextTrue(iterator, result)
 *  - hasNextFalse(iterator, result) - always follows previous event with same args (meaningless)
 *  - next(iterator)
 */

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  // event, Iterator, bool

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map()
    val kind = Symbol(record.get(0))
    map += 'kind -> kind
    kind match {
      // hasNextTrue, 2134607032, true
      case 'hasNextTrue =>
        map += 'one -> record.get(1).trim
        map += 'two -> record.get(2).trim.toBoolean
        addMapEvent(map)
      // next, 2134607032, 
      case 'next =>
        map += 'one -> record.get(1).trim
        addMapEvent(map)
      case _ =>
    }
  }
}

class M extends CSVMonitor {
  val hasNextTrue, next = event
  val Safe = fact

  "r1" -- hasNextTrue('i, true) |-> insert(Safe('i))

  "r2" -- Safe('i) & next('i) |-> remove(Safe)

  "r3" -- next('i) & not(Safe('i)) |->
    fail("next called unsafely")
}

object Evaluate extends MonitorFeeder {
  val hasNextTrue = 'hasNextTrue
  val hasNextFalse = 'hasNextFalse
  val next = 'next

  val trace1 = List(
    hasNextTrue(2134607032, true),
    hasNextFalse(2134607032, true),
    next(2134607032))

  val trace2 = List(
    hasNextTrue(1546908073, true),
    hasNextFalse(1546908073, true),
    hasNextTrue(1546908073, true),
    hasNextFalse(1546908073, true),
    next(1546908073))

  val trace3 = List(
    hasNextTrue(902478634, true),
    hasNextFalse(902478634, true),
    hasNextTrue(2114444063, false),
    hasNextFalse(2114444063, false),
    next(902478634))

  val trace_1 = List(
    hasNextTrue(1470344997, true),
    hasNextFalse(1470344997, true),
    next(1470344997),
    next(1470344997))

  val trace_2 = List(
    hasNextTrue(2039810346, false),
    hasNextFalse(2039810346, false),
    next(2039810346))

  val trace_3 = List(
    hasNextTrue(1448061896, true),
    hasNextFalse(1448061896, true),
    next(382750013))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_3 foreach m.addMapEvent
    m.terminate()
  }
}
