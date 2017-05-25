package examples.syde.g3_rover.g2_commandnesting

import rete._

/**
 * Command nesting.
 *
 * If command with identifier B starts after command with identifier A
 * has started then command B must succeed before command A succeeds.
 * A command can only be started and succeed once.
 */

class CommandNesting extends Monitor {
  "r1" -- 'com('x) |-> insert('Com('x))
  "r2" -- 'Com('x) & 'com('x) |-> fail()
  "r3" -- 'Com('x) & 'suc('x) |-> insert('Suc('x))
  "r4" -- 'suc('x) & not('Com('x)) |-> fail()
  "r5" -- 'Suc('x) & 'suc('x) |-> fail()
  "r6" -- 'Com('x) & not('Suc('x)) & 'com('y) |-> 'Ord('x, 'y)
  "r7" -- 'Ord('x, 'y) & 'suc('y) |-> remove('Ord)
  "r8" -- 'Ord('x, 'y) & 'suc('x) |-> fail()
}

object Evaluate extends MonitorFeeder {
  val com = 'com
  val suc = 'suc

  val A = "A"
  val B = "B"
  val C = "C"

  val trace1 = List(
    com(A),
    com(B),
    suc(B),
    suc(A))

  val trace_1 = List(
    com(A),
    com(B),
    suc(A),
    suc(B))

  val trace_2 = List(
    com(A),
    com(B),
    suc(B),
    suc(A),
    suc(A),
    com(B),
    suc(C))

  def main(args: Array[String]) {
    val m = new CommandNesting
    m.PRINT = true
    trace_2 foreach m.addMapEvent
  }
}
