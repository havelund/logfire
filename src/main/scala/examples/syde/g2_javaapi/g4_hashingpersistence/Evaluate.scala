package examples.syde.g2_javaapi.g4_hashingpersistence

import rete._

/**
 * Hashing persistence
 *
 * Objects placed in a hashing structure, such as a HashSet
 * or HashMap, should have persistent hash codes whilst in the structure for the
 * usage to be sound. Otherwise we may have the situation where we add an object
 * and then return false when checking for its presence.
 */

class HashingPersistence1 extends Monitor {
  val addObj, observeObj, removeObj = event
  val Added = fact

  "addobj" -- addObj('s, 'o) |-> {
    println("addobj 1 executed")
    val h = 'o.a.hashCode()
    insert(Added('s, 'o, h))
  }

  "observe" -- Added('s, 'o, 'h) & observeObj('s, 'o) |-> {
    println("observe executed")
    ensure('h.i == 'o.a.hashCode())
  }

  "addobj" -- Added('s, 'o, 'h) & addObj('s, 'o) |-> {
    println("addobj 2 executed")
    ensure('h.i == 'o.a.hashCode())
  }

  "remove" -- Added('s, 'o, 'h) & removeObj('s, 'o) |-> {
    println("remove executed")
    ensure('h.i == 'o.a.hashCode())
    remove(Added)
  }
}

object Evaluate1 extends MonitorFeeder {
  val addObj = 'addObj
  val observeObj = 'observeObj
  val removeObj = 'removeObj

  class A(s: String) {
    var contents: String = s

    override def equals(o: Any) = o match {
      case that: A => contents == that.contents
      case _       => false
    }

    override def hashCode = contents.hashCode()

    override def toString = s"[$contents]"
  }

  val a1 = new A("abc")
  val a2 = new A("def")

  var s: Set[A] = Set()

  def trace1() {
    val m = new HashingPersistence1
    m.PRINT = true

    m.addMapEvent(addObj(s, a1))
    m.addMapEvent(addObj(s, a2))
    m.addMapEvent(observeObj(s, a1))
    m.addMapEvent(observeObj(s, a2))
    m.addMapEvent(addObj(s, a2))
    m.addMapEvent(removeObj(s, a1))
    m.addMapEvent(removeObj(s, a2))
  }

  def trace_1() {
    val m = new HashingPersistence1
    m.PRINT = true

    println(s"a1's hashCode: ${a1.hashCode()}")
    m.addMapEvent(addObj(s, a1))
    m.addMapEvent(addObj(s, a2))
    m.addMapEvent(observeObj(s, a1))
    m.addMapEvent(observeObj(s, a2))
    a1.contents = "it has changed"
    println(s"changed a1's hashCode to: ${a1.hashCode()}")
    m.addMapEvent(observeObj(s, a1))
    m.addMapEvent(observeObj(s, a2))
    m.addMapEvent(addObj(s, a1))
    m.addMapEvent(addObj(s, a2))
    m.addMapEvent(removeObj(s, a1))
    m.addMapEvent(removeObj(s, a2))
  }

  def main(args: Array[String]) {
    trace_1()
  }
}

// Use this:

class HashingPersistence2 extends Monitor {
  "r1" -- 'add('c, 'o, 'h) |-> insert('In('c, 'o, 'h))
  "r2" -- 'In('c, 'o, 'h1) & 'observe('c, 'o, 'h2) |-> ensure('h1.i == 'h2.i)
  "r3" -- 'In('c, 'o, 'h1) & 'add('c, 'o, 'h2) |-> ensure('h1.i == 'h2.i)
  "r4" -- 'In('c, 'o, 'h1) & 'remove('c, 'o, 'h2) |-> {
    ensure('h1.i == 'h2.i)
    remove('In)
  }
}

object Evaluate2 extends MonitorFeeder {
  val addObj = 'add
  val observeObj = 'observe
  val removeObj = 'remove

  val s = "aset"
  val a1 = "a1"
  val a2 = "a2"

  val trace1 = List(
    addObj(s, a1, 1000),
    addObj(s, a2, 2000),
    observeObj(s, a1, 1000),
    observeObj(s, a2, 2000),
    addObj(s, a1, 1000),
    removeObj(s, a1, 1000),
    removeObj(s, a2, 2000))

  val trace_1 = List(
    addObj(s, a1, 1000),
    addObj(s, a2, 2000),
    observeObj(s, a1, 1000),
    observeObj(s, a2, 3000),
    addObj(s, a1, 1000),
    removeObj(s, a1, 1000),
    removeObj(s, a2, 3000))    
    
  def main(args: Array[String]) {
    val m = new HashingPersistence2
    m.PRINT = true
    trace_1 foreach m.addMapEvent
  }
}