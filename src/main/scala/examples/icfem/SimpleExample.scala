package examples.icfem

import rete._

// alpha(t1, t2, t3) |==  beta(t1, t2)  &&  E3(clk -> t3)
// beta(t1, t2)      |==  E1(clk -> t1)  >>  E2(clk -> t2)

class Verifier extends Monitor {
  "v1" -- 'E1('clk -> 't1) |-> insert('E1Seen('t1))

  "v2" -- 'E2('clk -> 't2) & not('E1Seen('t1)) |-> fail()
  
  "v3" -- 'E2('clk -> 't2) & 'E1Seen('t1) |-> {if('t2-'t1 > 5000) fail()}
}

class Abstracter extends Monitor {
  "catchAll" -- ANY() & not(ANY()) |-> {}

  "a1" -- 'E1('clk -> 't1) |-> insert('E1Seen('t1))

  "a2" -- 'E1Seen('t1) & 'E2('clk -> 't2) |-> { 
    remove('E1Seen); 
    insert('beta('t1, 't2)) 
  }
  
  "a3" -- 'E3('clk -> 't3) |-> insert('E3Seen('t3))
  
  "a4" -- 'beta('t1, 't2) & 'E3Seen('t3) |-> { 
    remove('E3Seen); 
    insert('alpha('t1, 't2, 't3)) 
  }
}

object ApplyMonitor {
  def main(args: Array[String]) {
    //val m = new Abstracter
    val m = new Verifier
    m.PRINT = true
    m.addMapEvent('E1)('clk -> 1023)
    m.addMapEvent('E3)('clk -> 3239)
    m.addMapEvent('E2)('clk -> 7008)
  }
}
