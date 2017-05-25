package examples.ftscs

import rete._

class NoLockCyclesPositional extends Monitor {
  val lock, unlock = event
  val Locked, Edge = fact

  "lock" -- lock('t, 'l) |-> insert(Locked('t, 'l))
  "unlock" -- Locked('t, 'l) & unlock('t, 'l) |-> remove(Locked)
  "edge" -- Locked('t, 'l1) & lock('t, 'l2) |-> insert(Edge('l1, 'l2))
  "close" -- Edge('l1, 'l2) & Edge('l2, 'l3) & not(Edge('l1, 'l3)) |-> insert(Edge('l1, 'l3))
  "cycle" -- Edge('l1, 'l2) |-> {
    if (get('l1) == get('l2)) fail("cycle detected on " + get('l1))
  }
}

class NoLockCyclesMaps extends Monitor {
  val lock, unlock = event
  val Locked, Edge = fact

  "lock" -- lock('task -> 't, 'lock -> 'l) |-> insert(Locked('t, 'l))
  "unlock" -- Locked('t, 'l) & unlock('task -> 't, 'lock -> 'l) |-> remove(Locked)
  "edge" -- Locked('t, 'l1) & lock('task -> 't, 'lock -> 'l2) |-> insert(Edge('l1, 'l2))
  "close" -- Edge('l1, 'l2) & Edge('l2, 'l3) & not(Edge('l1, 'l3)) |-> insert(Edge('l1, 'l3))
  "cycle" -- Edge('l1, 'l2) |-> {
    if (get('l1) == get('l2)) fail("cycle detected on " + get('l1))
  }
}

class NoLockCyclesThreads extends Monitor {
  val lock, unlock = event
  val Locked, Edge = fact

  def getset(s: Symbol) = get[Set[Int]](s)

  var cycles: Set[Set[Int]] = Set()

  "lock" -- lock('task -> 't, 'lock -> 'l) |-> insert(Locked('t, 'l))
  "unlock" -- Locked('t, 'l) & unlock('task -> 't, 'lock -> 'l) |-> remove(Locked)
  "edge" -- Locked('t, 'l1) & lock('task -> 't, 'lock -> 'l2) |-> insert(Edge(Set(get[Int]('t)), 'l1, 'l2))
  "close" -- Edge('s1, 'l1, 'l2) & Edge('s2, 'l2, 'l3) & not(Edge('_, 'l1, 'l3)) |-> {
    if (getset('s1).intersect(getset('s2)).isEmpty) insert(Edge(getset('s1).union(getset('s2)), 'l1, 'l3))
  }
  "cycle" -- Edge('s, 'l1, 'l2) |-> {
    if (get('l1) == get('l2) & !cycles.contains(getset('s))) 
      fail("cycle detected between threads" + get('s))
    cycles += getset('s)
  }
}

object ApplyMonitor1 {
  def main(args: Array[String]) {
    val m = new NoLockCyclesPositional
    //val m = new NoLockCyclesMaps
    m.PRINT = true
    m.addEvent('lock)(1, 10)
    m.addEvent('lock)(1, 20)
    m.addEvent('unlock)(1, 20)
    m.addEvent('unlock)(1, 10)

    m.addEvent('lock)(2, 20)
    m.addEvent('lock)(2, 10)
    m.addEvent('unlock)(2, 10)
    m.addEvent('unlock)(2, 20)
  }
}

object ApplyMonitor2 {
  def main(args: Array[String]) {
    val m = new NoLockCyclesPositional
    //val m = new NoLockCyclesMaps
    m.PRINT = true
    m.addEvent('lock)(1, "l1")
    m.addEvent('lock)(1, "l2")
    m.addEvent('unlock)(1, "l2")
    m.addEvent('unlock)(1, "l1")

    m.addEvent('lock)(2, "l2")
    m.addEvent('lock)(2, "l3")
    m.addEvent('unlock)(2, "l3")
    m.addEvent('unlock)(2, "l2")

    m.addEvent('lock)(3, "l3")
    m.addEvent('lock)(3, "l1")
    m.addEvent('unlock)(3, "l1")
    m.addEvent('unlock)(3, "l3")
  }
}

object ApplyMonitorThreads1 {
  def main(args: Array[String]) {
    val m = new NoLockCyclesThreads
    m.PRINT = true
    m.addMapEvent('lock)('task -> 1, 'lock -> 10)
    m.addMapEvent('lock)('task -> 1, 'lock -> 20)
    m.addMapEvent('unlock)('task -> 1, 'lock -> 20)
    m.addMapEvent('unlock)('task -> 1, 'lock -> 10)

    m.addMapEvent('lock)('task -> 2, 'lock -> 20)
    m.addMapEvent('lock)('task -> 2, 'lock -> 10)
    m.addMapEvent('unlock)('task -> 2, 'lock -> 10)
    m.addMapEvent('unlock)('task -> 2, 'lock -> 20)
  }
}

object ApplyMonitorThreads2 {
  def main(args: Array[String]) {
    val m = new NoLockCyclesThreads
    m.PRINT = true
    m.addMapEvent('lock)('task -> 1, 'lock -> 10)
    m.addMapEvent('lock)('task -> 1, 'lock -> 20)
    m.addMapEvent('unlock)('task -> 1, 'lock -> 20)
    m.addMapEvent('unlock)('task -> 1, 'lock -> 10)

    m.addMapEvent('lock)('task -> 2, 'lock -> 20)
    m.addMapEvent('lock)('task -> 2, 'lock -> 30)
    m.addMapEvent('unlock)('task -> 2, 'lock -> 30)
    m.addMapEvent('unlock)('task -> 2, 'lock -> 20)

    m.addMapEvent('lock)('task -> 3, 'lock -> 30)
    m.addMapEvent('lock)('task -> 3, 'lock -> 10)
    m.addMapEvent('unlock)('task -> 3, 'lock -> 10)
    m.addMapEvent('unlock)('task -> 3, 'lock -> 30)
  }
}
