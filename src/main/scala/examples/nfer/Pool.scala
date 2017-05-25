package examples.nfer

import rete._

/*
 * Events are added, but transformed into intervals in the pool
 * A :- E1 ; E2
 */


class Pool extends Monitor {
  val E1, E2 = event
  val I1, I2, A = fact

  // --- A: ---

  "r1" -- E1('t1) |-> I1('t1,'t1)
  "r2" -- E2('t1) |-> I2('t1,'t1)

  "r3" -- I1('s1,'e1) & I2('s2,'e2) |-> A('s1,'e2)

  "r4" -- A() |-> {
    println("A generated")
  }
}

object Pool {
  def main(args: Array[String]) {
    val m = new Pool

    m.PRINT = true
    m.addEvent('E1)(15)
    m.addEvent('E2)(20)
    m.addEvent('E3)(25)
    m.addEvent('E4)(30)
  }
}