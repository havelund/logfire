package examples.nfer

import rete._

/*
 * A :- E1 before E2
 * B :- E3 before E4
 * C :- A before B
 *
 */


class Before extends Monitor {
  val E1, E2, E3, E4 = event
  val I1, I2, I3, I4 = fact
  val A, B, C = fact

  // --- Pool creation ----

  "i1" -- E1('t1) |-> I1('t1,'t1)
  "i2" -- E2('t1) |-> I2('t1,'t1)
  "i3" -- E3('t1) |-> I3('t1,'t1)
  "i4" -- E4('t1) |-> I4('t1,'t1)

  "checkI1" -- I1('s1,'e1) |-> {
    println("An I1 was generated")
  }

  "checkI2" -- I2('s1,'e1) |-> {
    println("An I2 was generated")
  }

  "checkI3" -- I3('s1,'e1) |-> {
    println("An I3 was generated")
  }

  "checkI4" -- I4('s1,'e1) |-> {
    println("An I4 was generated")
  }

  // --- A: ---

  "r1" -- I1('s1,'e1) & I2('s2,'e2) |-> {
    if ('e1 < 's2) {
      insert(A('s1,'e2))
    }
  }

  "checkA" -- A('s1,'e1) |-> {
    println("An A was generated")
  }

  // --- B: ---

  "r2" -- I3('s1,'e1) & I4('s2,'e2) |-> {
    if ('e1 < 's2) {
      insert(B('s1,'e2))
    }
  }

  "checkB" -- B('s1,'e1) |-> {
    println("A B was generated")
  }

  // --- C: ---

  "r3" -- A('s1,'e1) & B('s2,'e2) |-> {
    if ('e1 < 's2) {
      insert(C('s1,'e2))
    }
  }

  "checkC" -- C('s1,'e1) |-> {
    println("A C was generated")
  }
}

class Before2 extends Nfer {
  'A :- 'E1 before 'E2 within 5 nomap()
  'B :- 'E3 before 'E4 nomap()
  'C :- 'A before 'B nomap()

  //'A :- 'E1 before 'E2 map defaultPhi

  "checkC" -- '__nfer__C('s1,'e1) |-> {
    println("A C was generated")
  }
}

object Before {
  def main(args: Array[String]) {
    val m = new Before2

    // there should be 3 C's

    m.PRINT = true
    // A
    m.addEvent('E1)(5)
    m.addEvent('E2)(10)
    // A
    m.addEvent('E1)(15)
    m.addEvent('E2)(20)
    // B
    m.addEvent('E3)(25)
    m.addEvent('E4)(30)
    // A
    m.addEvent('E1)(35)
    m.addEvent('E2)(40)
    // B
    m.addEvent('E3)(45)
    m.addEvent('E4)(50)
  }
}