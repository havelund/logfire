package examples.ex0

import rete._

trait ResourceMonitor extends Monitor {
  val before, request, deny, grant, release, end = event
  val Before, Granted, Deny = fact
}

trait Ordering extends ResourceMonitor {
  "r1" -- before('r1, 'r2) |-> Before('r1, 'r2)
  "r2" -- Before('r1, 'r2) & Before('r2, 'r3) |-> Before('r1, 'r3)
}

trait Release extends ResourceMonitor {
  "r3" -- grant('_, 't, 'r) & not(Granted('t, 'r)) |-> Granted('t, 'r)
  "r4" -- Granted('t, 'r) & release('_, 't, 'r) |-> remove(Granted)
  "r5" -- end() & Granted('t, 'r) |-> fail("missing release")
}

trait NoRelease extends ResourceMonitor {
  "r6" -- release('_, 't, 'r) & not(Granted('t, 'r)) |-> fail("bad release")
}

trait NoGrant extends ResourceMonitor {
  "r7" -- Granted('t, 'r) & grant('_, 't_, 'r) |-> fail("bad double grant")
  "r8" -- Before('r1, 'r2) & Granted('t, 'r2) & grant('_, 't, 'r1) |-> fail("bad grant order")
}

trait Deny extends ResourceMonitor {
  var counter: Int = 0

  "r9" -- Granted('_, 'r) & request('s, 't, 'r) |-> Deny('s, 't, 'r)
  "r10" -- Before('r1, 'r2) & Granted('t, 'r2) & request('s, 't, 'r1) |-> Deny('s, 't, 'r1)
  "r11" -- Deny('s1, 't, 'r) & deny('s2, 't, 'r) |-> {
    ensure(('s2 - 's1) <= 10000 & counter < 3)
    remove(Deny)
    counter += 1
  }
  "r12" -- end() & Deny('s, 't, 'r) |-> fail("missing deny")
}

class ResourceManagement
  extends Ordering
  with Release
  with NoRelease
  with NoGrant
  with Deny

object ApplyResourceManagement {
  def main(args: Array[String]) {
    val m = new ResourceManagement
    m.PRINT = true
    
    m.addEvent('before)('wheel1, 'wheel2)
    m.addEvent('before)('wheel2, 'wheel3)
    m.addEvent('request)(1000, "drive", 'wheel3)
    m.addEvent('grant)(2000, "drive", 'wheel3)
    m.addEvent('request)(3000, "drive", 'wheel1)
    m.addEvent('grant)(4000, "drive", 'wheel1) // NoGrant violated
    m.addEvent('release)(5000, "drive", 'wheel3)
    m.addEvent('release)(6020, "drive", 'wheel1)
    m.addEvent('end)() // Deny violated due to missing deny
  }
}