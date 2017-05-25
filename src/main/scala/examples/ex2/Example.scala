package examples.ex2

import rete._

class TestRuleOrder extends Monitor {
  var order: List[(Int,Int)] = Nil

  "r1" -- 'e('x) |-> { order ::= ('x,1) }
  "r2" -- 'e('x) |-> { order ::= ('x,2) }
  "r3" -- 'e('x) |-> { order ::= ('x,3) }
  "r4" -- 'e('x) |-> { order ::= ('x,4) }
}

object ApplyMonitor {
  def main(args: Array[String]) {
    val m = new TestRuleOrder
    m.PRINT = true
    m.addEvent('e)(1)
    m.addEvent('e)(2)
    m.addEvent('e)(3)
    m.addEvent('e)(4)
    m.order.reverse foreach println
  }
}

