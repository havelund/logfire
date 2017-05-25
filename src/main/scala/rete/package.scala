/**
 * '''LogFire''' is a Scala package for analyzing ''event traces''.
 *
 * == Events ==

 * A trace is considered
 * as a sequence of events, where an individual event is a mapping from symbols to values.
 * That is, an event has the type:
 *
 * {{{Map[Symbol, Any]}}}
 *
 * For example, the following is an event:
 *
 * {{{Map('command -> "DRIVE", 'time -> 267833)}}}
 *
 * Events can (__but do not need to__) have a kind, indicated by a
 * the symbol `'kind` mapping to a symbol, as in:
 *
 * {{{Map('kind -> 'EVR, 'command -> "DRIVE", 'time -> 267833)}}}
 *
 * Such events can (__but do not need to__) be written, as we shall see,
 * in the special format:
 *
 * {{{'EVR('command -> "DRIVE", 'time -> 267833)}}}
 *
 * Use of Scala's implicit function definitions under the hood will make all
 * this work. As we shall see, events can also be provided in positional format:
 *
 * {{{'EVR("DRIVE",267833)}}}
 *
 * This has the same meaning as:
 *
 * {{{Map('kind -> 'EVR, 'one -> "DRIVE", 'two -> 267833)}}}
 *
 * Events are short-lived: when submitting an event to a monitor it only survives
 * in one step, enough to evaluate the left-hand sides of active rules.
 *
 *
 * == Facts ==
 *
 * Facts are in essence of the same format as events. They are created during monitoring
 * as rules match and rule right-hand sides are executed. In contrast to events, however,
 * a fact survives in the fact memory until it is removed again explicitly by a rule or
 * by a call of a method designated for fact removal.
 *
 *
 * == Monitors ==
 *
 * === Using positional events ===
 *
 * A monitor is a user-defined subclass of the class `Monitor`, which defines what rules
 * to be applied during event monitoring. Consider the following monitor that monitors
 * three requirements about issued commands and their successes and failures.
 * The conditions of the rules and generated facts are formulated in a positional style in contrast
 * to as maps, which will be shown in a subsequent monitor for the same requirements.
 *
{{{
class CommandMonitor1 extends Monitor {
  // Requirements:
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
}}}
 *
 * This monitor can now be instantiated and subsequently be fed a sequence of events as follows:
 *
{{{
object Test1 {
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
}}}
 *
 * Error messages will be printed on standard out as violations are detected.
 *
 *
 * === Using map events ===
 *
 * The same requirements can be formalized using map-like events. This is done in the following
 * monitor. Note that facts are still specified using positional notation (although also map-based
 * notation is possible for facts). Usually facts have few arguments, and hence a positional style
 * is preferable. Instead of using quoted names for events and facts (kinds), we apply the
 * event and fact functions, which allow us to omit the quotes.
 *
{{{
class CommandMonitor2 extends Monitor {
  // - An issued command should eventually succeed without failing first.
  // - A command cannot succeed more than once.
  // - A non-issued command cannot succeed.

  val COMMAND, SUCCESS, FAIL, END = event
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
}}}
 *
 * This monitor can now be instantiated and fed a sequence of map-events, as follows.
 * We here use a different style, storing first the events in a trace and then processing
 * the trace.
 *
{{{
object Test2 {
  def main(args: Array[String]) {
    val m = new CommandMonitor2
    val trace = List(
      Map('kind -> 'COMMAND, 'name -> "STOP_DRIVING", 'number -> 1),
      Map('kind -> 'COMMAND, 'name -> "START_CAMERA", 'number -> 2),
      Map('kind -> 'COMMAND, 'name -> "TURN_ANTENNA", 'number -> 3),
      Map('kind -> 'FAIL, 'name -> "STOP_DRIVING", 'number -> 1),
      Map('kind -> 'SUCCESS, 'name -> "START_CAMERA", 'number -> 2),
      Map('kind -> 'SUCCESS, 'name -> "START_CAMERA", 'number -> 2),
      Map('kind -> 'SUCCESS, 'name -> "START_DRIVING", 'number -> 4),
      Map('kind -> 'END)
    )
    for (event <- trace) m.addMapEvent(event)
  }
}
}}}
 *
 *
 * == Specification patterns ==
 *
 * LogFire makes it possible to define specification patterns as methods.
 * The following example illustrates the definition of the 3 temporal logic
 * patterns:
 *
{{{
 *   r1 : always(a -> !b weakuntil c)
 *   r2 : always(a -> <>b)
 *   r3 : always(b -> <*>a) - where <*a> means sometime in the past
}}}
 *
 * The syntax for the 3 patterns are:
 *
{{{
 *   "r1" --- a -| b |-> c
 *   "r2" --- a --> b
 *   "r3" --- a <-- b
}}}
 *
 * These patterns are defined in the following class:
 *
{{{
class Observer extends Monitor {
  def absence(name: String)(a: Cond, b: Cond, c: Cond) {
    val a_seen_sym = newSymbol()
    val a_seen = a_seen_sym(a.getVariables: _*)
    "a" -- a |-> a_seen
    "b" -- b & a_seen |-> fail(s"$name - ${eval(a)} and then ${eval(b)} before ${eval(c)}")
    "c" -- c & a_seen |-> remove(a_seen_sym)
  }

  def presence(name: String)(a: Cond, b: Cond) {
    val a_seen_sym = newSymbol()
    val a_seen = a_seen_sym(a.getVariables: _*)
    "a" -- a |-> a_seen
    "b" -- b & not(a_seen) |-> fail(s"not ${eval(a)} before ${eval(b)}")
  }

  implicit def absence_syntax(name: String) = new {
    implicit def ---(a: Cond) = new {
      def -|(b: Cond) = new {
        def |->(c: Cond) {
          absence(name)(a, b, c)
        }
      }

      def -->(b: Cond) {
        absence(name)(a, 'END(), b)
      }

      def <--(b: Cond) {
        presence(name)(a, b)
      }
    }
  }
}
}}}
 *
 * This class allows us for example to write the following monitor (observer):
 *
{{{
class CommandMonitor3 extends Observer {
  "commands must succeed" ---
    'COMMAND('x, 'y) --> 'SUCCESS('x, 'y)

  "commands must not fail before success" ---
    'COMMAND('x, 'y) -| 'FAIL('x, 'y) |-> 'SUCCESS('x, 'y)

  "commands cannot succeed without having been issued" ---
    'COMMAND('x, 'y) <-- 'SUCCESS('x, 'y)
}
}}}
 *
 * Test1 above with `CommandMonitor1` replaced with `CommandMonitor3` yields the following
 * output:
 *
{{{
 *** error: commands must not fail before success - 'COMMAND(STOP_DRIVING,1) and then 'FAIL(STOP_DRIVING,1) before 'SUCCESS(STOP_DRIVING,1)
 *** error: not 'COMMAND(START_DRIVING,4) before 'SUCCESS(START_DRIVING,4)
 *** error: commands must succeed - 'COMMAND(TURN_ANTENNA,3) and then 'END() before 'SUCCESS(TURN_ANTENNA,3)
 *** error: commands must succeed - 'COMMAND(STOP_DRIVING,1) and then 'END() before 'SUCCESS(STOP_DRIVING,1)
}}}
 *
 * == Using case classes to specify events and facts ==
 * 
 * In the following is explained how one can use case classes to represent events and facts
 * when writing monitors and submitting to monitors.
 * 
{{{
class CommandMonitor4 extends Monitor {
  // First define the types of events and facts as case classes extending the trait Obs.
  // There is at this point no distinction between events and facts. Extending Obs has no 
  // other purpose than to ensure that only such classes are used in rules. We add a time stamp to
  // events.

  // Event types:
  case class COMMAND(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class SUCCESS(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class FAIL(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class END extends Obs

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
    END() & Commanded('n, 'x) |-> {
      fail(s"command (${'n.s},${'x.i}}) never succeeded")
    }

  // Note that one can refer to parameters by name in which case not all parameters need to
  // be mentioned. One can, however, also refer to parameters by position as for the Commanded fact above.
}
}}}
 *
 * We can now use the above monitor as follows.
 * 
{{{
object Demo4 {

  // First we define the events to be submitted as case classes (can be normal classes also).
  // In this case the class does not need to extend Obs. Also, since there will be no pattern matching
  // one will use the actual types, such as String and Int. 

  case class COMMAND(name: String, number: Int, time: Int)
  case class SUCCESS(name: String, number: Int, time: Int)
  case class FAIL(name: String, number: Int, time: Int)
  case class END

  // Events are now submitted as objects with the addObjEvent method defined in the monitor.
  // Note that the field names must be the same as in the specification. One cannot use the same
  // classes as used in the specification due to the need in the specification to refer to 
  // variables for some fields: those that are bound on matching, which have type +[T].

  def main(args: Array[String]) {
    val m = new CommandMonitor4

    m.addObjEvent(COMMAND("STOP_DRIVING", 1, 1000))
    m.addObjEvent(COMMAND("START_CAMERA", 2, 2000))
    m.addObjEvent(COMMAND("TURN_ANTENNA", 3, 3000))
    m.addObjEvent(FAIL("STOP_DRIVING", 1, 4000))
    m.addObjEvent(SUCCESS("START_CAMERA", 2, 5000))
    m.addObjEvent(SUCCESS("START_CAMERA", 2, 6000))
    m.addObjEvent(SUCCESS("START_DRIVING", 4, 7000))
    m.addObjEvent(END())
  }
}
}}}
 */

package object rete{}
