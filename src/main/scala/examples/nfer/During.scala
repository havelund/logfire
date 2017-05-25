package examples.nfer

import rete._

/*
 * A :- E2 before E3
 * B :- E1 before E4
 * C :- A during B
 *
 */


class During extends Monitor {
  val E1, E2, E3, E4 = event
  val I1, I2, I3, I4 = fact
  val A, B, C = fact

  // --- Pool creation ----

  "i1" -- E1('t1) |-> I1('t1,'t1)
  "i2" -- E2('t1) |-> I2('t1,'t1)
  "i3" -- E3('t1) |-> I3('t1,'t1)
  "i4" -- E4('t1) |-> I4('t1,'t1)

  // --- A: ---

  "r1" -- I2('s1,'e1) & I3('s2,'e2) |-> A('s1,'e2)

  // --- B: ---

  "r2" -- I1('s1,'e1) & I4('s2,'e2) |-> B('s1,'e2)

  // --- C: ---

  "r5" -- A('s1,'e1) & B('s2,'e2) |-> {
    if ('s1 >= 's2 && 'e1 <= 'e2) insert(C('s1,'e2))
  }

  "r6" -- C('s1,'e1) |-> {
    println("C generated")
  }
}

class During2 extends Nfer {
  'A :- 'E2 before 'E3 within 5 nomap()
  'B :- 'E1 before 'E4 nomap()
  'C :- 'A during 'B nomap()

  //'A :- 'E1 before 'E2 map defaultPhi

  "checkC" -- '__nfer__C('s1,'e1) |-> {
    println("A C was generated")
  }
}

object During {
  def main(args: Array[String]) {
    val m = new During2

    m.PRINT = true
    m.addEvent('E1)(15)
    m.addEvent('E2)(20)
    m.addEvent('E3)(25)
    m.addEvent('E4)(30)
  }
}