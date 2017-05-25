package examples.crv15.rvmonitor.benchmark1

import rete._

/*
 * This property captures a bad Java programming practice: updating a map 
 * (inserting or deleting) while iterating its keyset (or values set). 
 * Violation of the property in Java code may cause a ConcurrentModificationException 
 * at runtime.
 * 
 * Spec: createColl updateMap* createIter useIter* updateMap updateMap* useIter 
 * 
 * Events:
 * 
 * - createColl(m,c)
 * - createIter(c,i)
 * - useIter(i)
 * - updateMap(m)
 */

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  // event, Map, Collection, Iterator

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map();
    val kind = Symbol(record.get(0))
    map += 'kind -> kind
    kind match {
      // updateMap, map, , 
      case 'updateMap =>
        map += 'one -> record.get(1).trim
      // createColl,map, coll, 
      case 'createColl =>
        map += 'one -> record.get(1).trim
        map += 'two -> record.get(2).trim
      // createIter, , coll, iter
      case 'createIter =>
        map += 'one -> record.get(2).trim
        map += 'two -> record.get(3).trim
      // useIter, , , iter 
      case 'useIter =>
        map += 'one -> record.get(3).trim
    }
    addMapEvent(map)
  }
}

// It is hopelessly slow. Only way to do this in competitive
// time is to avoid LogFire rules and code it up in Scala.

class M_Slow extends CSVMonitor {
  val createColl, createIter, useIter, updateMap = event
  val Collection, Iterator, UnsafeIter = fact

  "r1" -- createColl('m, 'c) |-> insert('Collection('m, 'c))

  "r2" -- Collection('m, 'c) & createIter('c, 'i) |->
    insert(Iterator('m, 'i))

  "r3" -- Iterator('m, 'i) & updateMap('m) |->
    insert(UnsafeIter('i))

  "r4" -- UnsafeIter('i) & useIter('i) |->
    fail("iterator used after update to map")
}

// Coded up in Scala:

object Util {
  implicit def updMap(map: Map[String, Set[String]]) = new {
    def +>(a: String, b: String): Map[String, Set[String]] = {
      val set: Set[String] = map.getOrElse(a, Set())
      map + (a -> (set + b))
    }

    def fetch(a: String) = map.getOrElse(a, Set())
  }
}
import Util._

class M extends CSVMonitor {
  val createColl, createIter, useIter, updateMap = event

  var collMap: Map[String, String] = Map()
  var mapIters: Map[String, Set[String]] = Map()
  var unsafeIters: Set[String] = Set()

  def print() {
    if (false) {
      println("---")
      println(s"collMap    : $collMap")
      println(s"mapIters   : $mapIters")
      println(s"unsafeIters: $unsafeIters")
      println("---")
    }
  }

  "r1" -- createColl('m, 'c) |-> {
    collMap += 'c.s -> 'm.s
    //print()
  }

  "r2" -- createIter('c, 'i) |-> {
    if (collMap.contains('c.s)) {
      val map = collMap('c.s)
      mapIters = mapIters +> (map, 'i.s)
      //print()
    }
  }

  "r3" -- updateMap('m) |-> {
    for (it <- mapIters.fetch('m.s)) {
      unsafeIters += it
    }
    //print()
  }

  "r4" -- useIter('i) |-> {
    if (unsafeIters.contains('i.s))
      fail("iterator used after update to map")
  }
}

object Evaluate extends MonitorFeeder {
  val createColl = 'createColl
  val createIter = 'createIter
  val useIter = 'useIter
  val updateMap = 'updateMap

  val trace1 = List(
    updateMap(313540687),
    createColl(313540687, 1019384604),
    createIter(1019384604, 1306324352),
    useIter(1306324352))

  val trace2 = List(
    updateMap(1202683709),
    createColl(1202683709, 173099767),
    createIter(173099767, 166794956),
    updateMap(1202683709))

  val trace3 = List(
    updateMap(1202683709),
    createIter(173099767, 166794956),
    updateMap(1202683709),
    useIter(166794956))

  // update map 306612792 in between the creation and usage of the iterator 1496355635
  val trace_1 = List(
    updateMap(306612792),
    createColl(306612792, 447212746),
    createIter(447212746, 1496355635),
    updateMap(306612792),
    useIter(1496355635))

  // update map 306612792 in between two usages of the iterator 1496355635
  val trace_2 = List(
    updateMap(306612792),
    createColl(306612792, 447212746),
    createIter(447212746, 1496355635),
    useIter(1496355635),
    updateMap(306612792),
    useIter(1496355635))

  // similar to E.G. 2.1, but with multiple updateMap events
  val trace_3 = List(
    updateMap(306612792),
    createColl(306612792, 447212746),
    createIter(447212746, 1496355635),
    updateMap(306612792),
    updateMap(306612792),
    useIter(1496355635))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_3 foreach m.addMapEvent
    m.terminate()
  }
}
