package examples.cmacsreport

import rete._

class GrantRelease extends Monitor {
  "r1" -- 'grant('t, 'r) |-> 'Granted('t, 'r)
  "r2" -- 'release('t, 'r) & not('Granted('t, 'r)) |-> fail()  
  "r3" -- 'Granted('t, 'r) & 'release('t, 'r) |-> remove('Granted)
  "r4" -- 'Granted('_, 'r) & 'grant('_, 'r) |-> fail()

  hot('Granted)
}

object ApplyResourceManagement {
  def main(args: Array[String]) {
    val m = new GrantRelease
    m.PRINT = true
    
    m.addEvent('grant)("T1", "r1")
    m.addEvent('grant)("T2", "r2")
    m.addEvent('release)("T2", "r2")
    m.addEvent('grant)("T3", "r1")
    m.addEvent('release)("T2", "r2")    
    
    m.terminate()
  }
}