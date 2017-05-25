package examples.fm

import rete._

class LockOrder extends Monitor {
  val acquire, release = event
  val Locked, Edge = fact

  "acquire" -- acquire('t, 'l) |-> insert(Locked('t, 'l))
  "release" -- Locked('t, 'l) & release('t, 'l) |-> remove(Locked)
  "edge"    -- Locked('t, 'l1) & acquire('t, 'l2) |-> insert(Edge('l1, 'l2))
  "cycle"   -- Edge('l1, 'l2) & Edge('l2, 'l1) |-> fail()
}

object ApplyMonitor {
  def main(args: Array[String]) {
    val m = new LockOrder
    m.PRINT = true
    m.addEvent('acquire)("t1", "A")
    m.addEvent('acquire)("t1", "B")
    m.addEvent('release)("t1", "B")
    m.addEvent('release)("t1", "A")

    m.addEvent('acquire)("t2", "B")
    m.addEvent('acquire)("t2", "A")
    m.addEvent('release)("t2", "A")
    m.addEvent('release)("t2", "B")
  }
}

