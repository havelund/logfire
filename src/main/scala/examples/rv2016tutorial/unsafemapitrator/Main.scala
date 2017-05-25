package examples.rv2016tutorial.unsafemapitrator

import rete._

class UnsafeMapIterator extends Monitor {
  val create, iterator, use, updatemap = event
  val MapColl, MapIter, UnsafeIter = fact

  "r1" -- create('m, 'c) |-> insert(MapColl('m, 'c))
  "r2" -- MapColl('m, 'c) & iterator('c, 'i) |-> insert(MapIter('m, 'i))
  "r3" -- MapIter('m, 'i) & updatemap('m) |-> insert(UnsafeIter('i))
  "r4" -- UnsafeIter('i) & use('i) |-> fail()
}

trait Events {
  val create = 'create
  val iterator = 'iterator
  val use = 'use
  val updatemap = 'updatemap

  val M1 = 'M1
  val M2 = 'M2
  val I1 = 'I1
  val I2 = 'I2
  val C1 = 'C1
  val C2 = 'C2
}

object ApplyMonitor extends Events {

  def main(args: Array[String]) {
    val m = new UnsafeMapIterator
    m.PRINT = true

    m.addEvent(create)(M1, C1)
    m.addEvent(create)(M2, C2)
    m.addEvent(iterator)(C1, I1)
    m.addEvent(iterator)(C2, I2)
    m.addEvent(use)(I1)
    m.addEvent(updatemap)(M1)
    m.addEvent(use)(I1)
  }
}

object Trace {

  def main(args: Array[String]) {
    val m = new UnsafeMapIterator
    m.PRINT = true

    m.addEvent('create)('M1, 'C1)
    m.addEvent('create)('M1, 'C2)
    m.addEvent('iterator)('C1, 'I1)
    m.addEvent('updatemap)('M1)
    m.addEvent('iterator)('C2, 'I2)
    m.addEvent('use)('I1)
  }
}

