package examples.tutorial

import rete._

import scala.language.implicitConversions
import scala.language.reflectiveCalls

// =================
// LogFire Tutorial:
// =================

// This file contains the examples for the LogFire tutorial.
// The examples are meant for demonstration purposes and not
// for test.

// ---------------------------------------------------------
// Example using events and facts with positional arguments.
// ---------------------------------------------------------

// The example also used symbols as event and fact names.

class CommandMonitor1 extends Monitor {
  // - An issued command should eventually succeed without failing first.
  // - A command cannot succeed more than once.
  // - A non-issued command cannot succeed.

  "record command" --
    'COMMAND('name, 'number) |-> insert('Commanded('name, 'number))

  "record success" --
    'Commanded('n, 'x) & 'SUCCESS('n, 'x) |-> replace('Commanded)('Succeeded('n, 'x))

  "record failure" --
    'Commanded('n, 'x) & 'FAIL('n, 'x) |-> {
      remove('Commanded)
      fail(s"command (${'n.s},${'x.i}) failed before success")
    }

  "double succcess" --
    'Succeeded('n, 'x) & 'SUCCESS('n, 'x) |-> fail(s"two successes of (${'n.s},${'x.i}})")

  "success of non-issed command" --
    'SUCCESS('n, 'x) & not('Commanded('n, 'x)) |-> fail(s"success of non-issued command (${'n.s},${'x.i}})")

  "catch non-succeeded commands" --
    'END() & 'Commanded('n, 'x) |-> {
      fail(s"command (${'n.s},${'x.i}}) never succeeded")
    }
}

object Demo1 {
  def main(args: Array[String]) {
    val m = new CommandMonitor1
    m.addEvent('COMMAND)("STOP_DRIVING", 1)
    m.addEvent('COMMAND)("START_CAMERA", 2)
    m.addEvent('COMMAND)("TURN_ANTENNA", 3)
    m.addEvent('FAIL)("STOP_DRIVING", 1)
    m.addEvent('SUCCESS)("START_CAMERA", 2)
    m.addEvent('SUCCESS)("START_CAMERA", 2)
    m.addEvent('SUCCESS)("START_DRIVING", 4)
    m.addEvent('END)()
  }
}

// ----------------------------------------------------------------------------
// Example using events with map arguments and facts with positional arguments.
// ----------------------------------------------------------------------------

// The example uses event and fact names without quotes.

class CommandMonitor2 extends Monitor {
  // - An issued command should eventually succeed without failing first.
  // - A command cannot succeed more than once.
  // - A non-issued command cannot succeed.

  val COMMAND, SUCCESS, FAIL = event
  val Commanded, Succeeded = fact

  "record command" --
    COMMAND('name -> 'x, 'number -> 'y) |-> insert(Commanded('x, 'y))

  "record success" --
    Commanded('n, 'x) & SUCCESS('name -> 'n, 'number -> 'x) |-> replace(Commanded)(Succeeded('n, 'x))

  "record failure" --
    Commanded('n, 'x) & FAIL('name -> 'n, 'number -> 'x) |-> {
      remove(Commanded)
      fail(s"command (${'n.s},${'x.i}) failed before success")
    }

  "double succcess" --
    Succeeded('n, 'x) & SUCCESS('name -> 'n, 'number -> 'x) |-> fail(s"two successes of (${'n.s},${'x.i}})")

  "success of non-issed command" --
    SUCCESS('name -> 'n, 'number -> 'x) & not(Commanded('n, 'x)) |-> fail(s"success of non-issued command (${'n.s},${'x.i}})")

  "catch non-succeeded commands" --
    END() & Commanded('n, 'x) |-> {
      fail(s"command (${'n.s},${'x.i}}) never succeeded")
    }
}

object Demo2 {
  def main(args: Array[String]) {
    val m = new CommandMonitor2
    val trace = List(
      Map('kind -> 'COMMAND, 'name -> "STOP_DRIVING", 'number -> 1),
      Map('kind -> 'COMMAND, 'name -> "START_CAMERA", 'number -> 2),
      Map('kind -> 'COMMAND, 'name -> "TURN_ANTENNA", 'number -> 3),
      Map('kind -> 'FAIL, 'name -> "STOP_DRIVING", 'number -> 1),
      Map('kind -> 'SUCCESS, 'name -> "START_CAMERA", 'number -> 2),
      Map('kind -> 'SUCCESS, 'name -> "START_CAMERA", 'number -> 2),
      Map('kind -> 'SUCCESS, 'name -> "START_DRIVING", 'number -> 4))
    for (event <- trace) m.addMapEvent(event)
    m.terminate()
  }
}

// ---------------------------
// Example using abstractions.
// ---------------------------

// The example illustrates how to define specification patterns
// with parameterized events.

class Observer extends Monitor {
  // [](a -> !b w c)
  def absence(name: String)(a: PC, b: PC, c: PC) {
    val a_seen_sym = newSymbol('unsafe)
    val a_seen = a_seen_sym(a.getVariables: _*)
    "begin" -- a |-> a_seen
    "bad" -- b & a_seen |-> fail(s"$name - ${eval(a)} and thenDoActions ${eval(b)} before ${eval(c)}")
    "end" -- c & a_seen |-> remove(a_seen_sym)
  }

  // [](b -> <.>a)
  def presence(name: String)(a: PC, b: PC) {
    val a_seen_sym = newSymbol('safe)
    val a_seen = a_seen_sym(a.getVariables: _*)
    "observed" -- a |-> a_seen
    "follow" -- b & not(a_seen) |-> fail(s"not ${eval(a)} before ${eval(b)}")
  }

  implicit def absence_syntax(name: String) = new {
    implicit def ---(a: PC) = new {
      def -|(b: PC) = new {
        def |->(c: PC) {
          absence(name)(a, b, c)
        }
      }

      def -->(b: PC) {
        absence(name)(a, 'END(), b)
      }

      def <--(b: PC) {
        presence(name)(a, b)
      }
    }
  }
}

class CommandMonitor3 extends Observer {
  "commands must succeed" ---
    'COMMAND('x, 'y) --> 'SUCCESS('x, 'y)

  "commands must not fail before success" ---
    'COMMAND('x, 'y) -| 'FAIL('x, 'y) |-> 'SUCCESS('x, 'y)

  "commands cannot succeed without having been issued" ---
    'COMMAND('x, 'y) <-- 'SUCCESS('x, 'y)
}

object Demo3 {
  def main(args: Array[String]) {
    val m = new CommandMonitor3
    m.addEvent('COMMAND)("STOP_DRIVING", 1)
    m.addEvent('COMMAND)("START_CAMERA", 2)
    m.addEvent('COMMAND)("TURN_ANTENNA", 3)
    m.addEvent('FAIL)("STOP_DRIVING", 1)
    m.addEvent('SUCCESS)("START_CAMERA", 2)
    m.addEvent('SUCCESS)("START_CAMERA", 2)
    m.addEvent('SUCCESS)("START_DRIVING", 4)
    m.addEvent('END)()
  }
}

// -----------------------------------------------
// Using case classes to specify events and facts.
// -----------------------------------------------

// In the following is explained how one can use case classes to represent events and facts
// when writing monitors and submitting to monitors. 

class CommandMonitor4 extends Monitor {
  // First define the types of events and facts as case classes extending the trait Obs.
  // There is at this point no distinction between events and facts. Extending Obs has no 
  // other purpose than to ensure that only such classes are used in rules. We add a time stamp to
  // events.

  // Event types:
  case class COMMAND(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class SUCCESS(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class FAIL(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case object End extends Obs

  // Fact types:
  case class Commanded(name: +[String], number: +[Int]) extends Obs
  case class Succeeded(name: +[String], number: +[Int]) extends Obs

  // In the above definitions, each field, which is intended to hold a value of type T is declared 
  // to have type +[T], which is defined as: type +[T] = Either[T,Symbol]. This type allows for 
  // values of type T as well as variables of type Symbol in the rules. Note that if only values of type T
  // will occur for a field one can use T. Likewise, if only variables of type Symbol occur as "value" for
  // a field in rules, type Symbol can be used, although indicating the intended type might be useful for
  // documentation purposes. It should be noted that types of symbols are not checked statically.
  //  
  // Parameters not mentioned in conditions will have the default value defined in the case class
  // declaration. If this is 'null', such a parameter will be ignored all together in the condition 
  // matching if not mentioned explicitly.

  // We can now write rules which use instantiations of the above case classes as event and fact conditions
  // as well as facts to be added to fact memory.

  "record command" --
    COMMAND(name = 'x, number = 'y) |-> insert(Commanded('x, 'y))

  "record success" --
    Commanded('n, 'x) & SUCCESS(name = 'n, number = 'x) |-> replace('Commanded)(Succeeded('n, 'x))

  "record failure" --
    Commanded('n, 'x) & FAIL(name = 'n, number = 'x) |-> {
      remove('Commanded)
      fail(s"command (${'n.s},${'x.i}) failed before success")
    }

  "double succcess" --
    Succeeded('n, 'x) & SUCCESS(name = 'n, number = 'x) |-> fail(s"two successes of (${'n.s},${'x.i}})")

  "success of non-issed command" --
    SUCCESS(name = 'n, number = 'x) & not(Commanded('n, 'x)) |-> fail(s"success of non-issued command (${'n.s},${'x.i}})")

  "catch non-succeeded commands" --
    End & Commanded('n, 'x) |-> {
      fail(s"command (${'n.s},${'x.i}}) never succeeded")
    }

  // Note that one can refer to parameters by name in which case not all parameters need to
  // be mentioned. One can, however, also refer to parameters by position as for the Commanded fact above.
}

// We can now use the above monitor as follows.

object Demo4 {

  // First we define the events to be submitted as case classes (can be normal classes also).
  // In this case the class does not need to extend Obs. Also, since there will be no pattern matching
  // one will use the actual types, such as String, Long and Int. 

  case class COMMAND(name: String, number: Int, time: Int)
  case class SUCCESS(name: String, number: Int, time: Int)
  case class FAIL(name: String, number: Int, time: Int)
  case object END

  // Events are now submitted as objects with the addObjEvent method defined in the monitor.
  // Note that the field names must be the same as in the specification. One cannot use the same
  // class due to the need in the specification to refer to variables for some fields: those that
  // are bound on matching.

  def main(args: Array[String]) {
    val m = new CommandMonitor4

    m.addObjEvent(COMMAND("STOP_DRIVING", 1, 1000))
    m.addObjEvent(COMMAND("START_CAMERA", 2, 2000))
    m.addObjEvent(COMMAND("TURN_ANTENNA", 3, 3000))
    m.addObjEvent(FAIL("STOP_DRIVING", 1, 4000))
    m.addObjEvent(SUCCESS("START_CAMERA", 2, 5000))
    m.addObjEvent(SUCCESS("START_CAMERA", 2, 6000))
    m.addObjEvent(SUCCESS("START_DRIVING", 4, 7000))
    m.addObjEvent(END)
  }
}
