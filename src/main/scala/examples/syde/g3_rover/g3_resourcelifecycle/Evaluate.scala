package examples.syde.g3_rover.g3_resourcelifecycle

import rete._

/**
 * Resource lifecycle.
 *
 * This property represents the lifecycle of a resource as
 * managed by a planetary rovers internal resource management system - or any
 * resource management system in general. The lifecycle goes as follows:
 *
 * - A resource may be requested
 * - A requested resource may be denied or granted
 * - A granted resource may be rescinded or cancelled
 * - A resource may only be requested if any previous granting has been cancelled
 * - A granted resource must eventually be cancelled
 *
 * This refl ects three states, idle, requested, granted, and their transitions. We
 * use the events request(r), deny(r), grant(r), rescind(r) and cancel(r) for a
 * resource r.
 * 
 * DISCUSSION:
 * 
 * Change property by removing second to last requirement and add the requirement:
 * 
 * - A resource cannot be granted if it is already granted and not yet cancelled.
 */

class ResourceLifecycle extends Monitor {  
  "r1" -- 'request('r) |-> insert('Req('r))
  "r2" -- 'Req('r) & 'deny('r) |-> remove('Req)
  "r3" -- 'Req('r) & 'grant('r) |-> {
    remove('Req)
    insert('Grant('r))
  }
  "r4" -- 'Grant('r) & 'cancel('r) |-> remove('Grant)    
  "r5" -- 'deny('r) & not('Req('r)) |-> fail()    
  "r6" -- 'grant('r) & not('Req('r)) |-> fail()  
  "r7" -- 'grant('r) & 'Grant('r) |-> fail()    
  "r8" -- 'rescind('r) & not('Grant('r)) |-> fail()
  "r9" -- 'cancel('r) & not('Grant('r)) |-> fail()
  
  hot('Grant)
}

object Evaluate extends MonitorFeeder {
  val request = 'request
  val deny = 'deny 
  val grant = 'grant
  val rescind = 'rescind
  val cancel = 'cancel
  
  val trace1 = List(
    request(1),
    request(2),
    grant(1),
    request(1),
    grant(2),
    deny(1),
    rescind(1),
    cancel(1),
    cancel(2)
  )  
    
  val trace_1 = List(
    request(1),
    grant(1),
    deny(1),
    rescind(1),
    request(2),
    grant(1),
    cancel(2),
    request(1),
    grant(2),
    grant(2),
    deny(1),
    rescind(1),
    cancel(1)
  )  
  
  def main(args: Array[String]) {
    val m = new ResourceLifecycle
    m.PRINT = true
    trace_1 foreach m.addMapEvent
    m.terminate()
  }
}
