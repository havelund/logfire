package examples.cfg

import rete.Monitor

class TestStack extends Monitor {
  val enter, exit = event
  val Stack, Exit = fact

  def stack(s: Symbol) = get[List[Int]](s)

  "r1" -- enter('x) & Stack('s) |-> {
    update(Stack('x.i :: stack('s)))
  }

  "r2" -- exit('x) & Stack('s) |-> {
    val s = stack('s)
    ensure(!s.isEmpty && 'x.i == s.head)
    if (!s.isEmpty) update(Stack(s.tail))
  }

  "r3" -- END() & Stack('s) |-> {
    ensure(stack('s).isEmpty)
  }

  "r4" -- exit('_) & not(Exit()) |-> insert(Exit())

  "r5" -- enter('_) & Exit() |-> error

  addFact(Stack)(Nil: List[Int])
}

object Demo {
  def main(args: Array[String]) {
    val m = new TestStack
    m.PRINT = true
    m.addEvent('enter)(1)
    m.addEvent('enter)(2)
    m.addEvent('enter)(3)
    m.addEvent('exit)(3)
    m.addEvent('exit)(2)
    m.addEvent('exit)(1)
    m.addEvent('enter)(0)
    m.terminate()
  }
}
