
package examples.syde.g4_concurrency.g4_lockorderinggeneralized

import rete._

/**
 * Lock ordering generalized.
 *
 * This property represents a deadlock-avoidance strategy that
 * prevents cycles between locks by ordering the taking and releasing of locks.
 * The property states that for every N (different ) locks if they are taken
 * in a cyclic order by N different threads it reflects a deadlock potential.
 */

object Util {
  implicit def liftSet[T](s1: Set[T]) = new {
    def disjoint(s2: Set[T]) = s1.intersect(s2).isEmpty
  }
}
import Util._

class LockOrderingGeneralized extends Monitor {
  val lock, unlock = event
  val Locked, Edge = fact

  "r1" -- lock('t, 'l) |-> insert(Locked('t, 'l))
  "r2" -- Locked('t, 'l) & unlock('t, 'l) |-> remove(Locked)
  "r3" -- Locked('t, 'l1) & lock('t, 'l2) |-> insert(Edge(Set('t.any), 'l1, 'l2))
  "r4" -- Edge('s1, 'l1, 'l2) & Edge('s2, 'l2, 'l3) |-> {
    if ('s1.set.disjoint('s2.set)) insert(Edge('s1.set.union('s2.set), 'l1, 'l3))
  }
  "r5" -- Edge('_, 'l1, 'l2) |-> {
    ensure('l1.any != 'l2.any)
  }
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

  val trace_2 = List(
    // Thread T1;
    lock("T1", "r1"),
    lock("T1", "r2"),
    unlock("T1", "r2"),
    unlock("T1", "r1"),
    // Thread T2:    
    lock("T2", "r2"),
    lock("T2", "r3"),
    unlock("T2", "r3"),
    unlock("T2", "r2"),
    // Thread T3:
    lock("T3", "r3"),
    lock("T3", "r1"),
    unlock("T3", "r1"),
    unlock("T3", "r3"))

  def main(args: Array[String]) {
    val m = new LockOrderingGeneralized
    m.PRINT = true
    trace1 foreach m.addMapEvent
  }
}
