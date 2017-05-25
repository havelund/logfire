package examples.rv2016tutorial.hasnext

import rete._

class HasNext extends Monitor {
  val hasNext, next = event
  val Safe = fact

  "r1" -- hasNext('i, true) |-> insert(Safe('i))
  "r2" -- Safe('i) & next('i) |-> remove(Safe)
  "r3" -- next('i) & not(Safe('i)) |-> fail()
}

object ApplyMonitor1 extends MonitorFeeder {
  val hasNext = 'hasNext
  val next = 'next

  val i1 = 'i1
  val i2 = 'i2

  val trace1 = List(
    hasNext(i1, true),
    hasNext(i2, true),
    next(i1),
    next(i2))

  val trace_1 = List(
    hasNext(i1, true),
    hasNext(i2, true),
    next(i1),
    next(i2),
    hasNext(i1,false),
    next(i1)
  )

  def main(args: Array[String]) {
    val m = new HasNext
    m.PRINT = true
    trace_1 foreach m.addMapEvent
    m.terminate()
  }
}

object ApplyMonitor2 {
  def main(args: Array[String]) {
    val m = new HasNext
 
    val hasNext = 'hasNext
    val next = 'next

    val i1 = 'i1
    val i2 = 'i2
    
    m.PRINT = true
    m.addEvent(hasNext)(i1,true)
    m.addEvent(hasNext)(i2,true)
    m.addEvent(next)(i1)
    m.addEvent(next)(i2)
    m.addEvent(hasNext)(i1,false)
    m.addEvent(next)(i1)
  }
}

