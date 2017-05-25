package examples.crv15.logfire.benchmark5

import rete._
import java.util.ArrayList

/*
 * 
 */

object Util {
  implicit def liftSet[T](s1: Set[T]) = new {
    def disjoint(s2: Set[T]) = s1.intersect(s2).isEmpty
  }
}
import Util._

/*
 * The property concerns a multi-threaded program. A thread can take and release 
 * locks. The fact that a thread t takes a lock x is represented by the event lock(t,x). 
 * The fact that a thread t unlocks a lock x is represented by the event unlock(t,x).
 * 
 * The property states that there should be no cycles in the lock graph produced 
 * the following way:
 * 
 * - when a thread t takes a lock x2 while it already holds a lock x1, 
 *   an edge is created from x1 to x2.
 *   
 * - a deadlock potential is detected if for any lock x, by following edges, 
 *   one can reach back to x (a cycle)
 *   
 * Note that this algorithm does not only detect actual deadlocks, it also 
 * detects potentials for deadlocks: a cycle suggests that there could be a 
 * deadlock in this or a different execution of the system.
 *   
 * The requirement is as follows.
 *   
 *   Requirement: Locks should be acquired such that a deadlock cannot occur. 
 *     Specifically there should be a partial order defined on the locks and lock 
 *     acquisitions should follow this order.
 */

class M_Threads extends Monitor {
  val lock, unlock = event
  val Granted, Edge = fact

  "r1" -- lock('t, 'x) |-> insert(Granted('t, 'x))
  "r2" -- Granted('t, 'x) & unlock('t, 'x) |-> remove(Granted)
  "r3" -- Granted('t, 'x1) & lock('t, 'x2) |-> insert(Edge(Set('t.any), 'x1, 'x2))
  "r4" -- Edge('s1, 'x1, 'x2) & Edge('s2, 'x2, 'x3) |-> {
    if ('s1.set.disjoint('s2.set)) insert(Edge('s1.set.union('s2.set), 'x1, 'x3))
  }
  "r5" -- Edge('_, 'x1, 'x2) |-> ensure('x1.any != 'x2.any)
}

// Use this:

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map();
    map += 'kind -> Symbol(record.get(0))
    map += 'one -> record.get(1).trim
    map += 'two -> record.get(2).trim
    addMapEvent(map);
  }
}

class M extends CSVMonitor {
  val lock, unlock = event
  val Granted, Edge = fact

  "r1" -- lock('t, 'x) |-> insert(Granted('t, 'x))
  "r2" -- Granted('t, 'x) & unlock('t, 'x) |-> remove(Granted)
  "r3" -- Granted('t, 'x1) & lock('t, 'x2) |-> insert(Edge('x1, 'x2))
  "r4" -- Edge('x1, 'x2) & Edge('x2, 'x3) & not(Edge('x1, 'x3)) |-> insert(Edge('x1, 'x3))
  "r5" -- Edge('x1, 'x2) |-> ensure('x1.any != 'x2.any)
}

object Evaluate extends MonitorFeeder {
  val lock = 'lock
  val unlock = 'unlock

  val trace1 = List(
    lock(1, 1),
    lock(1, 2),
    unlock(1, 2),
    unlock(1, 1),
    lock(2, 2),
    lock(2, 3),
    unlock(2, 3),
    unlock(2, 2))

  val trace_1 = List(
    lock(1, 1),
    lock(1, 2),
    unlock(1, 2),
    unlock(1, 1),
    lock(2, 2),
    lock(2, 3),
    unlock(2, 3),
    unlock(2, 2),
    lock(3, 3),
    lock(3, 1),
    unlock(3, 1),
    unlock(3, 3))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace1 foreach m.addMapEvent
    m.terminate()
  }
}