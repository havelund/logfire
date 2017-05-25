package examples.ex1

import rete._

/*
 * In the following is explained how one can use case classes to represent events and facts
 * when writing monitors and submitting to monitors. Let's first illustrate how we would normally do this.
 * We will specify and monitor the property:
 * 
 *   Once a command (identified by a name) has been issued (message = "START"), 
 *   it can not be issued again before it has been confirmed terminated (message = "STOP").
 */

// Using symbols the specification is written as follows:

class OneCommandWithSymbols extends Monitor {
  val EVR = event
  val Commanded = fact

  "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('n)) |-> insert(Commanded('n))
  "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('n) |-> fail("two commands")
  "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('n) |-> remove('Commanded)
}

// We can then apply this monitor as follows:

object RunWithSymbols {
  def main(args: Array[String]) {
    val m = new OneCommandWithSymbols
    m.addMapEvent('EVR)('name -> "reboot", 'esclk -> 1000, 'message -> "START", 'level -> 0)
    m.addMapEvent('EVR)('name -> "reboot", 'esclk -> 2000, 'message -> "STOP", 'level -> 0)
    m.addMapEvent('EVR)('name -> "turn", 'esclk -> 3000, 'message -> "START", 'level -> 0)
    m.addMapEvent('EVR)('name -> "turn", 'esclk -> 4000, 'message -> "START", 'level -> 0)
    m.addMapEvent('EVR)('name -> "turn", 'esclk -> 5000, 'message -> "STOP", 'level -> 0)
  }
}

// Let's now use case classes.

class OneCommandWithClasses extends Monitor {
  // First define the types of events and facts as case classes extending the trait Obs.
  // There is at this point no distinction between events and facts. Extending Obs has no 
  // other purpose than to ensure that only such classes are used in rules.

  // An event type:
  case class EVR(
    name: +[String] = null,
    esclk: +[Long] = null,
    message: +[String] = null,
    level: +[Int] = null) extends Obs

  // A fact type:
  case class Commanded(name: +[String]) extends Obs

  /*
   * In the above definitions, each field, which is intended to hold a value of type T is declared 
   * to have type +[T], which is defined as: type +[T] = Either[T,Symbol]. This type allows for 
   * values of type T as well as variables of type Symbol in the rules. Note that if only values of type T
   * will occur for a field one can use T. Likewise, if only variables of type Symbol occur as "value" for
   * a field in rules, type Symbol can be used, although indicating the intended type might be useful for
   * documentation purposes. It should be noted that types of symbols are not checked statically.
   * 
   * Parameters not mentioned in conditions will have the value defined in the case class
   * declaration. If 'null', such a parameter will be ignored all together in the matching if not mentioned
   * explicitly.
   */

  /*
   * We can now write rules which use instantiations of the above case classes as event and fact conditions
   * as well as facts to be added to fact memory.
   */

  "r1" -- EVR(name = 'n, message = "START") & not(Commanded('n)) |-> insert(Commanded('n))
  "r2" -- EVR(name = 'n, message = "START") & Commanded('n) |-> fail("two commands")
  "r3" -- EVR(name = 'n, message = "STOP") & Commanded('n) |-> remove('Commanded)

  /*
   * Note that one can refer to parameters by name, as for the EVRs, in which case not all parameters need to
   * be mentioned. One can, however, also refer to parameters by position as for the Command fact above. Just
   * as is the case in Scala. 
   */
}

// We can now use the above monitor as follows.

object RunWithClasses {
  /*
   * First we define the events to be submitted as case classes (can be normal classes also).
   * In this case the class does not need to extend Obs. Also, since there will be no pattern matching
   * one will use the actual types, such as String, Long and Int. 
   */

  case class EVR(name: String, esclk: Long, message: String, level: Int)

  /*
   * Events are now submitted as objects with the addObjEvent method defined in the monitor.
   * Note that the field names must be the same as in the specification. One cannot use the same
   * class due to the need in the specification to refer to variables for some fields: those that
   * are bound on matching.
   */

  def main(args: Array[String]) {
    val m = new OneCommandWithClasses
    m.addObjEvent(EVR("reboot", 1000, "START", 0))
    m.addObjEvent(EVR("reboot", 2000, "STOP", 0))
    m.addObjEvent(EVR("turn", 3000, "START", 0))
    m.addObjEvent(EVR("turn", 4000, "START", 0))
    m.addObjEvent(EVR("turn", 5000, "STOP", 0))
  }
}
