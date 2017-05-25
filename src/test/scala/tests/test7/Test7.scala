
package tests.test7

import rete._
import scala.language.reflectiveCalls
import org.junit.Test

// Testing events and facts where actual event or fact does not have
// all of the required fields defined.
// Testing '$args feature allowing to bind the entire map to an identifier.

class M1 extends Monitor {
  val A, B = event
  val A_Occurred = fact

  var matchCounter = 0
  var matches1: Set[(Int, Int, Int)] = Set()
  var matches2: Set[(Int, Int, Int)] = Set()

  "r1" -- A('f -> 'x, 'g -> 'y) |-> A_Occurred('x, 'y)

  "r2" -- A_Occurred('x, 'y) & B('i -> 'y, 'j -> 'z) |-> {
    matches1 += (('x, 'y, 'z))
    matchCounter += 1
  }

  "r3" -- B('i -> 'y, 'j -> 'z) & A_Occurred('x, 'y) |-> {
    matches2 += (('x, 'y, 'z))
    matchCounter += 1
  }

  "r4" -- END() |-> {
    val expected1: Set[(Any, Any, Any)] = Set(
      (1, 2, 3))

    val expected2: Set[(Any, Any, Any)] = Set(
      (1, 2, 3))

    if (matches1 != expected1)
      fail(s"actual matches1: $matches1 != expected1 matches: $expected1")

    if (matches2 != expected2)
      fail(s"actual matches2: $matches2 != expected2 matches: $expected2")

    ensure(s"matchCounter: $matchCounter should be 2")(matchCounter == 2)
  }
}

class Test7_1 extends Contract {
  @Test def test() {
    val m = new M1
    setMonitor(m, false)

    add('A('f -> 1, 'g -> 2))
    facts(
      'A_Occurred(1, 2))

    add('B('i -> 2, 'j -> 3))
    facts(
      'A_Occurred(1, 2))

    add('B('i -> 2))
    facts(
      'A_Occurred(1, 2))

    add('B())
    facts(
      'A_Occurred(1, 2))

    add('END())
  }
}

class M2 extends Monitor {
  val A, B, C = event
  val F, G = fact

  "r1" -- A() |-> {
    val args = getArgs(A)
    assert(args('f) == 1 & args('g) == 2)
    insert(F(args))
  }

  "r2" -- B() |-> insert(G(getArgs(B)))

  "r3" -- C() & F('args1) & G('args2) |-> {
    val args0 = getArgs(C)
    val args1 = get[Binding]('args1)
    val args2 = 'args2.b
    assert(args0 == Map('i -> 2, 'kind -> 'C))
    assert(args1 == Map('f -> 1, 'g -> 2, 'kind -> 'A))
    assert(args2 == Map('i -> 2, 'j -> 3, 'kind -> 'B))
  }
}

class Test7_2 extends Contract {
  @Test def test() {
    val m = new M2
    setMonitor(m, false)
    add('A('f -> 1, 'g -> 2))
    facts(
      'F(Map('f -> 1, 'g -> 2, 'kind -> 'A)))

    add('B('i -> 2, 'j -> 3))
    facts(
      'G(Map('i -> 2, 'j -> 3, 'kind -> 'B)),
      'F(Map('f -> 1, 'g -> 2, 'kind -> 'A)))

    add('C('i -> 2))
    facts(
      'G(Map('i -> 2, 'j -> 3, 'kind -> 'B)),
      'F(Map('f -> 1, 'g -> 2, 'kind -> 'A)))
  }
}


