package examples.syde.g2_javaapi.g1_hasnext

import rete._

/**
 * Has next.
 *
 * This property applies to every java.util.Iterator object. Warns if
 * next() is invoked when hasNext() is not invoked or returns false. This property
 * requires that hasNext() be called before next() and that hasNext() return true
 */

class HasNext1 extends Monitor {
  "r1" -- 'hasnext('i, 'res) |-> {
    if ('res) insert('Safe('i))
  }
  "r2" -- 'Safe('i) & 'next('i) |-> remove('Safe)
  "r3" -- 'next('i) & not('Safe('i)) |-> fail()
}

// Use this:

class HasNext2 extends Monitor {
  "r1" -- 'hasnext('i, true) |-> insert('Safe('i))
  "r2" -- 'Safe('i) & 'next('i) |-> remove('Safe)
  "r3" -- 'next('i) & not('Safe('i)) |-> fail()
}

object Evaluate extends MonitorFeeder {
  val hasnext = 'hasnext
  val next = 'next

  val trace1 = List(
    hasnext(1, true),
    next(1),
    hasnext(1, true),
    hasnext(2, true),
    next(1),
    hasnext(1, true),
    next(2),
    next(1),
    hasnext(1, true),
    next(1),
    hasnext(1, false))

  val trace_1 = List(
    hasnext(1, true),
    next(1),
    hasnext(1, true),
    hasnext(2, true),
    next(1),
    hasnext(1, true),
    next(2),
    next(1),
    next(2),
    hasnext(1, true),
    next(1),
    hasnext(1, false),
    next(1))

  def main(args: Array[String]) {
    val m = new HasNext1
    m.PRINT = true
    trace_1 foreach m.addMapEvent
  }
}
