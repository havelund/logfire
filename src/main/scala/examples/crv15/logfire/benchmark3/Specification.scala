package examples.crv15.logfire.benchmark3

// Same as benchmark1

import rete._
import java.util.ArrayList

/*
 * The property concerns a resource manager on board a rover 
 * on which several tasks are executing. A task can get resources 
 * granted, and should then eventually release the resources back 
 * to the resource manager.
 * 
 * The grant of a resource r to a task t is represented by the event 
 * grant(t,r). The release of a resource r by a task t is represented 
 * by release(t,r).
 * 
 * The requirements are as follows. 
 * 
 *  Requirement 1: when a resource has been granted to a task 
 *  it must be released by the task before it can be granted 
 *  again (to the same or to another task).
 *  
 *  Requirement 2: when a resource has been granted to a task 
 *  it must eventually (before the end of the log) be released 
 *  by that same task.
 *  
 *  Requirement 3: when a task releases a resource it must have 
 *  previously been granted that resource and not released it yet.
 */

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
  val grant, release = event
  val Granted = fact

  "r1" -- grant('t, 'r) & not(Granted('_, 'r)) |-> insert(Granted('t, 'r))
  "r2" -- Granted('_, 'r) & grant('_, 'r) |-> fail("bad grant")
  "r3" -- Granted('t, 'r) & release('t, 'r) |-> remove(Granted)
  "r4" -- release('t, 'r) & not(Granted('t, 'r)) |-> fail("bad release")

  hot(Granted)
}

object Evaluate extends MonitorFeeder {
  val grant = 'grant
  val release = 'release

  val trace1 = List(
    grant(1, 1),
    release(1, 1),
    grant(2, 2),
    release(2, 2),
    grant(3, 3),
    release(3, 3))

  val trace2 = List(
    grant(1, 1),
    grant(2, 2),
    grant(3, 3),
    release(2, 2),
    release(1, 1),
    release(3, 3))

  val trace_1 = List(
    grant(1, 1),
    grant(2, 1),
    release(1, 1))

  val trace_2 = List(
    grant(1, 1),
    grant(2, 2),
    release(1, 1),
    release(3, 2))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_2 foreach m.addMapEvent
    m.terminate()
  }
}