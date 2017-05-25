package examples.syde.g3_rover.g4_resourcemanagement

import rete._

/**
 * Resource management.
 *
 * Every resource should only be held by at most one
 * task at any one time. If a resource is granted to a task it should be cancelled be-
 * fore being granted to another task. This is therefore a mutual exclusion property.
 * The event grant(task,resource) captures that task is granted resource.
 */

class ResourceManagement extends Monitor {
  "r1" -- 'grant('t, 'r) & not('Granted('_, 'r)) |-> insert('Granted('t, 'r))
  "r2" -- 'Granted('_, 'r) & 'grant('_, 'r) |-> fail()
  "r3" -- 'Granted('t, 'r) & 'cancel('t, 'r) |-> remove('Granted)
  "r4" -- 'cancel('t, 'r) & not('Granted('t, 'r)) |-> fail()
}

object Evaluate extends MonitorFeeder {
  val grant = 'grant
  val cancel = 'cancel

  val trace1 = List(
    grant("T1", "r1"),
    grant("T2", "r2"),
    cancel("T1", "r1"),
    cancel("T2", "r2"))

  val trace_1 = List(
    grant("T1", "r1"),
    grant("T2", "r2"),
    grant("T3","r1"),
    cancel("T1", "r1"),
    cancel("T3", "r2"))    
    
  def main(args: Array[String]) {
    val m = new ResourceManagement
    m.PRINT = true
    trace_1 foreach m.addMapEvent
  }
}
