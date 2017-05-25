package examples.crv15.logfire.benchmark4

import rete._
import java.util.ArrayList

/*
 * The property concerns a resource manager on board a rover on 
 * which several tasks are executing. A task can get resources granted, 
 * and will then eventually release the resources back to the resource manager. 
 * In addition, certain resources can be declared as being in conflict with each 
 * other. Two conflicting resources cannot be granted at the same time (to the 
 * same or to different tasks).
 * 
 * The grant of a resource r to a task t is represented by the event grant(r). 
 * We ignore the task since it is not important for the property. Similarly, 
 * the release of a resource r (by a task t) is represented by release(r). 
 * The declaration of a conflict between two resources is represented by 
 * conflict(r1,r2). Conflicts will be declared at the beginning of the log.
 * 
 * The requirements are as follows. 
 * 
 * Requirement 1: when a resource has been granted (to a task) 
 * it must be released before it can be granted again.
 * 
 * Requirement 2:No two conflicting resources can be granted at 
 * the same time. 
 */

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map();
    val kind = Symbol(record.get(0))
    map += 'kind -> kind
    kind match {
      //  conflict, resource, resource
      case 'conflict =>
        map += 'one -> record.get(1).trim
        map += 'two -> record.get(2).trim
      //  grant, task, resource
      case 'grant =>
        map += 'one -> record.get(2).trim
      //  release, task, resource
      case 'release =>
        map += 'one -> record.get(2).trim
    }
    addMapEvent(map)
  }
}

class M extends CSVMonitor {
  val conflict, grant, release = event
  val Conflict, Granted, Locks = fact

  "r1" -- conflict('r1, 'r2) |-> {
    insert(Conflict('r1, 'r2))
    insert(Conflict('r2, 'r1))
  }
  "r2" -- grant('r) & not(Granted('r)) & not(Locks('_, 'r)) |-> insert(Granted('r))
  "r3" -- Granted('r) & grant('r) |-> fail("double grant")
  "r4" -- Locks('_, 'r) & grant('r) |-> fail("granting lock in conflict")
  "r5" -- Granted('r1) & Conflict('r1, 'r2) |-> Locks('r1, 'r2)
  "r6" -- Granted('r) & release('r) |-> remove(Granted)
  "r7" -- Locks('r1, 'r2) & release('r1) |-> remove(Locks)
  //"r8" -- release('r) & not(Granted('r)) |-> fail("release of non-granted lock") // not on website, nor in informal req.
}

object Evaluate extends MonitorFeeder {
  val grant = 'grant
  val release = 'release
  val conflict = 'conflict

  val trace1 = List(
    conflict(1, 3),
    grant(1),
    grant(2),
    release(1),
    release(2))

  val trace_1 = List(
    conflict(1, 3),
    grant(1),
    grant(3),
    release(1),
    release(3))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_1 foreach m.addMapEvent
    m.terminate()
  }
}
