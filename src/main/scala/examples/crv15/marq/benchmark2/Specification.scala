package examples.crv15.marq.benchmark2

import rete._

/*
 * Every call(method_name) to method method_name should be matched by a return(method_name). 
 * Calls should nest. If method m2 is called whilst method m1 is waiting for a return then 
 * m2 should return before m1. Note that method calls are non-recursive i.e. if call(m1) 
 * occurs then call(m1) cannot occur again until there has been a return(m1).
 * 
 * Events:
 * 
 * - call(method_name)
 * - return(method_name) 
 * 
 * Since return is a reserved keyword in Scala, we use retn instead.
 */

class M extends Monitor {
  val call = event
  val retn = 'return // needed since return is keyword in Scala.
  val Before, Top = fact

  "r1" -- call('cm) & Top('tm) |-> {
    update(Top('cm))
    insert(Before('tm, 'cm))
  }

  "r2" -- retn('rm) & Top('tm) & Before('bm, 'tm) |-> {
    update(Top('bm))
    remove(Before)
  }

  "r3" -- retn('rm) & not(Top('rm)) |->
    fail("return out of order")

  "r4" -- call('cm) & Before('_, 'cm) |->
    fail("recursive call")

  "r5" -- retn('rm) & not(Before('_, 'rm)) |->
    fail("return from method not called")

  hot(Before)

  addFact(Top)("main")
}

object Evaluate extends MonitorFeeder {
  val call = 'call
  val retn = 'retn

  val trace1 = List(
    call("A"),
    call("B"),
    call("C"),
    retn("C"),
    retn("B"),
    retn("A"))

  val trace2 = List(
    call("A"),
    call("B"),
    retn("B"),
    call("C"),
    retn("C"),
    retn("A"))

  val trace3 = List(
    call("A"),
    retn("A"),
    call("B"),
    call("A"),
    retn("A"),
    retn("B"))

  val trace_1 = List(
    call("A"),
    call("B"),
    call("C"),
    retn("A"),
    retn("B"),
    retn("C"))

  val trace_2 = List(
    call("A"),
    call("B"),
    retn("B"),
    call("C"),
    retn("A"),
    retn("C"))

  val trace_3 = List(
    call("A"),
    call("B"),
    call("C"),
    call("A"),
    retn("A"),
    retn("B"),
    retn("C"),
    retn("A"))

  val trace_4 = List(
    call("A"),
    call("B"),
    retn("B"),
    call("C"),
    retn("C"))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_4 foreach m.addMapEvent
    m.terminate()
  }
}
