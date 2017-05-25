package examples.test

import rete._

import scala.language.implicitConversions
import scala.language.reflectiveCalls

class NoLockCycles extends Monitor {
  "r1" -- 'acquire('t,'l) |-> 'Locked('t,'l)
  "r2" -- 'Locked('t,'l) & 'release('t,'l) |-> remove('Locked)
  "r3" -- 'Locked('t,'l1) & 'acquire('t,'l2) |-> 'Edge('l1,'l2)
  "r4" -- 'Edge('l1,'l2) & 'Edge('l2,'l3) & not('Edge('l1,'l3)) |-> 'Edge('l1,'l3)
  "r5" -- 'Edge('l1,'l2) |-> { if (get('l1) == get('l2)) fail() }
}

object Demo1 {
  def main(args: Array[String]) {
    println("check for cycles")
    val m = new NoLockCycles
    m.PRINT = true
    m.addEvent('acquire)(1, "A")
    m.addEvent('acquire)(1, "B")
    m.addEvent('release)(1, "B")
    m.addEvent('release)(1, "A")
    m.addEvent('acquire)(2, "B")
    m.addEvent('acquire)(2, "A")
    m.addEvent('release)(2, "A")
    m.addEvent('release)(2, "B")
    m.addEvent('END)()
    println("check ends")
  }
}
