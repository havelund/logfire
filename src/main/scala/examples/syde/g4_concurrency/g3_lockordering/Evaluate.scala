package examples.syde.g4_concurrency.g3_lockordering

import rete._

/**
 * Lock ordering.
 *
 * This property represents a conservative deadlock-avoidance strategy that
 * prevents cycles between locks by ordering the taking and releasing of locks.
 * The property states that for every two (different ) locks if they are taken
 * in one order in one part of the system they are not then taken in a different
 * order in another part of the system.
 */

class LockOrdering extends Monitor {
  "r1" -- 'lock('t, 'l) |-> insert('Locked('t, 'l))
  "r2" -- 'Locked('t, 'l) & 'unlock('t, 'l) |-> remove('Locked)
  "r3" -- 'Locked('t, 'l1) & 'lock('t, 'l2) |-> insert('Edge('l1, 'l2))
  "r4" -- 'Edge('l1, 'l2) & 'Edge('l2, 'l1) |-> fail()
}

object Evaluate extends MonitorFeeder {
  val lock = 'lock
  val unlock = 'unlock

  val trace1 = List(
    lock("T1", "r1"),
    unlock("T1", "r1"),
    lock("T1", "r2"),
    unlock("T1", "r2"),
    lock("T2", "r2"),
    unlock("T2", "r2"),
    lock("T2", "r1"),
    unlock("T2", "r1"))

  val trace_1 = List(
    lock("T1", "r1"),
    lock("T1", "r2"),
    unlock("T1", "r2"),
    unlock("T1", "r1"),
    lock("T2", "r2"),
    lock("T2", "r1"),
    unlock("T2", "r1"),
    unlock("T2", "r2"))

  def main(args: Array[String]) {
    val m = new LockOrdering
    m.PRINT = true
    trace_1 foreach m.addMapEvent
  }
}
