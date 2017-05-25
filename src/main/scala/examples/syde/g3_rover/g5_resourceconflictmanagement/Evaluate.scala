package examples.syde.g3_rover.g5_resourceconflictmanagement

import rete._

/**
 * Resource conflict management.
 *
 * This property represents the management of conflicts between resources
 * as managed by a planetary rovers internal resource management system - or
 * any resource management system in general. It is assumed that conflicts
 * between resources are declared at the beginning of operation and that after
 * this point resources that are in conflict with each other cannot be granted at the
 * same time.
 *
 * A conflict between resources r1 and r2 is captured by the event conflict(r1,r2)
 * and a conflict is symmetrical. Resources are granted and cancelled using grant(r)
 * and cancel(r) respectively.
 * 
 * DISCUSSION:
 * 
 * Do we need the Locks predicate? Is it a problem that a resource does get granted when
 * it is a failure? (and the failure is reported).
 */

class ResourceConflictManagement extends Monitor {
  "r1" -- 'conflict('r1, 'r2) |-> {
    insert('Conflict('r1, 'r2))
    insert('Conflict('r2, 'r1))
  }
  "r2" -- 'grant('r) & not('Granted('r)) & not('Locks('_, 'r)) |-> insert('Granted('r))
  "r3" -- 'Granted('r) & 'grant('r) |-> fail()
  "r4" -- 'Locks('_, 'r) & 'grant('r) |-> fail()
  "r5" -- 'Granted('r1) & 'Conflict('r1, 'r2) |-> 'Locks('r1, 'r2)
  "r6" -- 'Granted('r) & 'cancel('r) |-> remove('Granted)
  "r7" -- 'Locks('r1,'r2) & 'cancel('r1) |-> remove('Locks)
  "r8" -- 'cancel('r) & not('Granted('r)) |-> fail()
}

object Evaluate extends MonitorFeeder {
  val conflict = 'conflict
  val grant = 'grant
  val cancel = 'cancel

  val trace1 = List(
    conflict("r1", "r10"),
    conflict("r2", "r20"),
    grant("r1"),
    grant("r2"),
    cancel("r1"),
    cancel("r2"),
    grant("r10"))

  val trace_1 = List(
    conflict("r1", "r10"),
    conflict("r2", "r20"),
    grant("r1"),
    grant("r2"),
    grant("r1"),
    cancel("r1"),
    cancel("r2"),
    cancel("r2"),
    grant("r10"),
    grant("r1"),
    cancel("r10"))

  def main(args: Array[String]) {
    val m = new ResourceConflictManagement
    m.PRINT = true
    trace_1 foreach m.addMapEvent
  }
}
