package examples.nfer

import rete._

/*
 * A :- E1 ; E2
 * B :- E3 ; E4
 * C :- A coincide B
 *
 */


class Coincide extends Monitor {
  val E1, E2, E3, E4 = event
  val I1, I2, I3, I4 = fact
  val A, B, C = fact

  // --- Pool creation ----

  "i1" -- E1('t1) |-> I1('t1,'t1)
  "i2" -- E2('t1) |-> I2('t1,'t1)
  "i3" -- E3('t1) |-> I3('t1,'t1)
  "i4" -- E4('t1) |-> I4('t1,'t1)

  // --- A: ---

  "r1" -- I1('s1,'e1) & I2('s2,'e2) |-> A('s1,'e2)

  // --- B: ---

  "r2" -- I3('s1,'e1) & I4('s2,'e2) |-> B('s1,'e2)

  // --- C: ---

  "r3" -- A('s1,'e1) & B('s2,'e2) |-> {
    if ('s1.i == 's2.i && 'e1.i == 'e2.i) insert(C('s1,'e2))
  }

  "r4" -- C('s1,'e1) |-> {
    println("C generated")
  }
}

class Coincide2 extends Nfer {
  'A :- 'E1 before 'E2 within 5 nomap()
  'B :- 'E3 before 'E4 nomap()
  'C :- 'A coincide 'B nomap()

  //'A :- 'E1 before 'E2 map defaultPhi

  "checkC" -- '__nfer__C('s1,'e1) |-> {
    println("A C was generated")
  }
}

object Coincide {
  def main(args: Array[String]) {
    val m = new Coincide2

    m.PRINT = true
    m.addEvent('E1)(15)
    m.addEvent('E2)(25)
    m.addEvent('E3)(15)
    m.addEvent('E4)(25)
  }
}