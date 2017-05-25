package examples.crv15.rvmonitor.benchmark4

import rete._

/*
 * This property requires that three specified methods of a class should 
 * be invoked equal number of times by any object of that class. 
 * 
 * Events:
 * - a(obj)
 * - b(obj)
 * - c(obj)
 * - done(obj)
 */

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map()
    map += 'kind -> Symbol(record.get(0))
    map += 'one -> record.get(1).trim
    addMapEvent(map)
  }
}

class M_Slow extends CSVMonitor {
  val a, b, c, done = event
  val ABC = fact

  "r1" -- a('o) & not(ABC('o, '_, '_, '_)) |-> insert(ABC('o, 1, 0, 0))
  "r2" -- b('o) & not(ABC('o, '_, '_, '_)) |-> insert(ABC('o, 0, 1, 0))
  "r3" -- c('o) & not(ABC('o, '_, '_, '_)) |-> insert(ABC('o, 0, 0, 1))

  "r4" -- a('o) & ABC('o, 'ca, 'cb, 'cc) |->
    update(ABC('o, 'ca + 1, 'cb, 'cc))

  "r5" -- b('o) & ABC('o, 'ca, 'cb, 'cc) |->
    update(ABC('o, 'ca, 'cb + 1, 'cc))

  "r6" -- c('o) & ABC('o, 'ca, 'cb, 'cc) |->
    update(ABC('o, 'ca, 'cb, 'cc + 1))

  "r7" -- ABC('o, 'ca, 'cb, 'cc) & done('o) |->
    ensure("same number of a, b and c")('ca.i == 'cb.i && 'cb.i == 'cc.i)
}

// This one does not cut it. Too slow.

class M extends CSVMonitor {
  val a, b, c, done = event
  val A, B, C = fact

  "r1" -- a('o) & not(A('o, '_)) |-> insert(A('o, 1))
  "r2" -- b('o) & not(B('o, '_)) |-> insert(B('o, 1))
  "r3" -- c('o) & not(C('o, '_)) |-> insert(C('o, 1))

  "r4" -- a('o) & A('o, 'count) |->
    update(A('o, 'count + 1))

  "r5" -- b('o) & B('o, 'count) |->
    update(B('o, 'count + 1))

  "r6" -- c('o) & C('o, 'count) |->
    update(C('o, 'count + 1))

  "r7" -- A('o, 'ca) & B('o, 'cb) & C('o, 'cc) & done('o) |-> 
    ensure("same number of a, b and c")('ca.i == 'cb.i && 'cb.i == 'cc.i)

  "r8" -- done('o) & not(A('o)) |-> fail()
  "r9" -- done('o) & not(B('o)) |-> fail()
  "r10" -- done('o) & not(C('o)) |-> fail()
}

object Evaluate extends MonitorFeeder {
  val a = 'a
  val b = 'b
  val c = 'c
  val done = 'done

  val trace1 = List(
    a(311239812),
    b(311239812),
    c(311239812),
    done(311239812))

  val trace2 = List(
    c(311239812),
    a(311239812),
    a(311239812),
    b(311239812),
    c(311239812),
    b(311239812),
    done(311239812))

  val trace3 = List(
    a(873610597),
    c(1497845528),
    c(873610597),
    b(1497845528),
    a(1497845528),
    b(873610597),
    done(1497845528),
    done(873610597))

  val trace_1 = List(
    a(311239812),
    c(311239812),
    done(311239812))

  val trace_2 = List(
    a(311239812),
    b(311239812),
    c(311239812),
    a(311239812),
    done(311239812))

  val trace_3 = List(
    a(873610597),
    b(311239812),
    c(311239812),
    done(311239812))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_3 foreach m.addMapEvent
    m.terminate()
  }
}
