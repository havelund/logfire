package examples.rv2016tutorial.lockordering

import rete._

class LockOrderingTwoLocks extends Monitor {
  val acquire, release = event
  val Locked, Edge = fact  
  
  "r1" -- acquire('t, 'l) |-> insert(Locked('t, 'l))
  "r2" -- Locked('t, 'l) & release('t, 'l) |-> remove(Locked)
  "r3" -- Locked('t, 'l1) & acquire('t, 'l2) |-> insert('Edge('l1, 'l2))
  "r4" -- Edge('l1, 'l2) & Edge('l2, 'l1) |-> fail()
}

class LockOrdering extends Monitor {
  val acquire, release = event
  val Locked, Edge = fact

  "r1" -- acquire('t, 'l) |-> insert(Locked('t, 'l))
  "r2" -- Locked('t, 'l) & release('t, 'l) |-> remove(Locked)
  "r3" -- Locked('t, 'l1) & acquire('t, 'l2) |-> insert(Edge('l1, 'l2))
  "r4" -- Edge('l1, 'l2) & Edge('l2, 'l3) & not(Edge('l1, 'l3)) |-> insert(Edge('l1, 'l3))
  "r5" -- Edge('l1, 'l2) |-> { if (get('l1) == get('l2)) fail() }
}

class LockOrderingPartial extends Monitor {
  val acquire, release = event
  val Locked, Edge = fact

  "r1" -- acquire('t, 'l) |-> insert(Locked('t, 'l))
  "r2" -- Locked('t, 'l) & release('t, 'l) |-> remove(Locked)
  "r3" -- Locked('t, 'l1) & acquire('t, 'l2) |-> insert(Edge('l1, 'l2))
  "r4" -- Edge('l1, 'l2) & Edge('l2, 'l3) & not(Edge('l1, 'l3))  |-> insert(Edge('l1, 'l3))
  "r5" -- Edge('l1,'l2) & Locked('t,'l2) & acquire('t,'l1) |-> fail()
}

object ApplyMonitor {
  def main(args: Array[String]) {
    val m = new LockOrderingPartial
    
    m.PRINT = true
    m.addEvent('acquire)('T1, 'A)
    m.addEvent('acquire)('T1, 'B)
    m.addEvent('release)('T1, 'B)
    m.addEvent('release)('T1, 'A)

    m.addEvent('acquire)('T2, 'B)
    m.addEvent('acquire)('T2, 'C)
    m.addEvent('release)('T2, 'C)
    m.addEvent('release)('T2, 'B)

    m.addEvent('acquire)('T3, 'C)
    m.addEvent('acquire)('T3, 'A)
    m.addEvent('release)('T3, 'A)
    m.addEvent('release)('T3, 'C)
  }
}

