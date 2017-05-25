package examples.rv2016tutorial.velocity

import rete._

/*
The speed of a monitored object should never go beyond a certain value $vmax$.

Original:

 forall $x,$y: G((s = $x ∧ t = $y) →  X(s - $x < vmax · (t - $y))

With events:

 forall x, y : G( e(x,y) => X( exists w,v: e(w,v) and (w-x < vmax · (v - y) ) ) ) 
 
In Ruler:

 observes e(int,int)

 always S {
  e(x,y) => Check(x,y)
 }
 state Check(x,y) {
   e(u,v){:
      (x-u) >= vmax*(v-y) -> Fail
      true   -> Ok
   :}
 }
*/

class Velocity1(vmax: Int) extends Monitor {
  val pos = event
  val Pos = fact

  "r1" -- pos('d, 't) |-> Pos('d, 't)

  "r2" -- Pos('d1, 't1) & pos('d2, 't2) |-> {
    ensure('d2 - 'd1 <= (vmax * ('t2 - 't1)))
    remove(Pos)
  }
}

class Velocity2(vmax: Int) extends Monitor {
  val pos = event
  val Pos = fact

  "r1" -- pos('d, 't) & not(Pos('_, '_)) |-> Pos('d, 't)

  "r2" -- Pos('d1, 't1) & pos('d2, 't2) |-> {
    ensure('d2 - 'd1 <= (vmax * ('t2 - 't1)))
    update(Pos('d2, 't2))
  }
}

class Velocity3(vmax: Int) extends Monitor {
  val pos = event
  
  var d : Int = 0
  var t : Int = 0

  "r" -- pos('d, 't) |-> {
    ensure('d - d <= (vmax * ('t - t)))
    d = 'd
    t = 't
  }
}

object ApplyMonitor {
  def main(args: Array[String]) {
    val m = new Velocity3(20)
    val pos = 'pos

    m.PRINT = true
    m.addEvent(pos)(1, 15)
    m.addEvent(pos)(60, 77)
    m.addEvent(pos)(100, 99)
    m.addEvent(pos)(121, 100)    
  }
}

