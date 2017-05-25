

package tests.test8

import rete._

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.language.postfixOps

// Examples

// ==============
// LogFire Paper:
// ==============

// This file contains the examples for the LogFire conference paper.
// The examples are meant for presentation purposes and not for test.


// ----------------------
// Specification Patterns
// ----------------------

trait LogicUtil extends Monitor {
  var ruleId = 0

  def newRuleId(): String = {
    ruleId += 1
    "r" + ruleId
  }
}

trait TemporalLogic extends LogicUtil {
  private def response(name: String)(pc1: PC, pc2: PC) {
    val unsafe = newSymbol('unsafe)
    val args = pc1.getVariables.reverse
    newRuleId() -- pc1 |-> unsafe(args: _*)
    newRuleId() -- unsafe(args: _*) & pc2 |-> remove(unsafe)
    newRuleId() -- unsafe(args: _*) & 'end() |-> fail(name)
  }

  private def precedence(name: String)(pc1: PC, pc2: PC) {
    val safe = newSymbol('safe)
    val args = pc2.getVariables.reverse
    newRuleId() -- pc2 |-> safe(args: _*)
    newRuleId() -- pc1 & not(safe(args: _*)) |-> fail(name)
  }

  implicit def syntax(name: String) = new {
    def ---(pc1: PC) = new {
      def -->(pc2: PC) = response(name)(pc1, pc2)

      def ==>(pc2: PC) = precedence(name)(pc1, pc2)
    }
  }
}

trait PathExpressions extends LogicUtil {
  type Condition = (Boolean, PC)

  implicit def pcToCondition(pc: PC) = (true, pc)

  def no(pc: PC) = (false, pc)

  def when(rule: String)(lhs: Condition*)(code: => Unit) {
    val initialState = newSymbol('State)
    var currentState = initialState
    var args: List[Any] = Nil

    def delete(state: Symbol) = if (state != initialState) remove(state)

    var negatedObserved: (Symbol, List[Any]) = null

    for ((flag, pc) <- lhs) {
      val currentStateNow = currentState // needed due to lazy eval of right hand sides
      val newState = newSymbol('State)
      val newArgs = args ++ (pc.getVariables.reverse filterNot (args.contains(_)))
      flag match {
        case true =>
          negatedObserved match {
            case (negState, negArgs) =>
              newRuleId() -- pc & currentStateNow(args: _*) & not(negState(negArgs: _*)) |-> {
                delete(currentStateNow)
                insert(newState(newArgs: _*))
              }
              newRuleId() -- pc & currentStateNow(args: _*) & negState(negArgs: _*) |-> {
                delete(currentStateNow)
                delete(negState)
              }
            case _ =>
              newRuleId() -- pc & currentStateNow(args: _*) |-> {
                delete(currentStateNow)
                insert(newState(newArgs: _*))
              }
          }
          currentState = newState
          args = newArgs
        case false =>
          newRuleId() -- pc & currentStateNow(args: _*) |-> {
            insert(newState(newArgs: _*))
          }
          negatedObserved = (newState, newArgs)
      }
    }

    newRuleId() -- currentState(args: _*) |-> {
      delete(currentState)
      code
    }

    addFact(initialState)()
  }
}

trait MonitorLogic extends TemporalLogic with PathExpressions

// -------------------
// --- Time lines: ---
// -------------------

trait TimeLines extends LogicUtil {
  type Condition = (Boolean, PC)

  private def timeLineSemantics(rule: String, lhs: List[Condition], rhs: List[Condition], bound: PC = 'end()) {
    val initialState = newSymbol('State)
    var currentState = initialState
    var args: List[Any] = Nil

    def delete(state: Symbol) = {
      if (state != initialState) remove(state)
    }

    // left hand side:

    var negatedObserved: (Symbol, List[Any]) = null

    for ((flag, pc) <- lhs) {
      val currentStateNow = currentState // needed due to lazy eval of right hand sides
      val newState = newSymbol('State)
      val newArgs = args ++ (pc.getVariables.reverse filterNot (args.contains(_)))
      flag match {
        case true =>
          negatedObserved match {
            case (negState, negArgs) =>
              newRuleId() -- pc & currentStateNow(args: _*) & not(negState(negArgs: _*)) |-> {
                delete(currentStateNow)
                insert(newState(newArgs: _*))
              }
              newRuleId() -- pc & currentStateNow(args: _*) & negState(negArgs: _*) |-> {
                delete(currentStateNow)
                delete(negState)
              }
            case _ =>
              newRuleId() -- pc & currentStateNow(args: _*) |-> {
                delete(currentStateNow)
                insert(newState(newArgs: _*))
              }
          }
          currentState = newState
          args = newArgs
        case false =>
          newRuleId() -- pc & currentStateNow(args: _*) |-> {
            insert(newState(newArgs: _*))
          }
          negatedObserved = (newState, newArgs)
      }
    }

    // right hand side:

    for ((flag, pc) <- rhs) {
      val currentStateNow = currentState // needed due to lazy eval of right hand sides
      flag match {
        case true =>
          val newState = newSymbol('State)
          newRuleId() -- currentStateNow(args: _*) & pc |->
            replace(currentStateNow)(newState(args: _*))
          newRuleId() -- currentStateNow(args: _*) & 'end() |->
            fail( s""""$rule" violated: expected event did not occur before end of log""")
          if (bound != 'end()) {
            newRuleId() -- currentStateNow(args: _*) & bound |->
              fail( s""""$rule" violated: ${eval(pc)} did not occur before bound""")
          }
          currentState = newState
        case false =>
          newRuleId() -- currentStateNow(args: _*) & pc |->
            fail( s""""$rule" violated: ${eval(pc)} occurred in unexpected position""")
          newRuleId() -- currentStateNow(args: _*) & bound |->
            remove(currentStateNow)
      }
    }

    addFact(initialState)()
  }

  def timeLine(rule: String)(lhs: Condition*)(rhs: Condition*) =
    timeLineSemantics(rule, lhs.toList, rhs.toList)

  def timeLineBounded(rule: String)(lhs: Condition*)(rhs: Condition*)(bound: PC) =
    timeLineSemantics(rule, lhs.toList, rhs.toList, bound)

  implicit def pcToCondition(pc: PC) = (true, pc)

  def no(pc: PC) = (false, pc)

  implicit def timeline_syntax(name: String) = new {
    implicit def when(lhs: Condition*) = new {
      def ensure(rhs: Condition*) = timeLineSemantics(name, lhs.toList, rhs.toList)

      def expect(rhs: Condition*) = new {
        def unless(bound: PC) = timeLineSemantics(name, lhs.toList, rhs.toList, bound)
      }

      def execute(code: => Unit) = null
    }
  }
}

trait LogicMonitor extends TemporalLogic with TimeLines


// ----------------------
// Trying Temporal Logic.
// ----------------------

class CommandMonitor1 extends LogicMonitor {
  "commands must succeed" ---
    'COMMAND('x, 'y) --> 'SUCCESS('x, 'y)

  "commands cannot succeed without having been issued" ---
    'SUCCESS('x, 'y) ==> 'COMMAND('x, 'y)
}

class DemoCommandMonitor1 {
  def main(args: Array[String]) {
    val m = new CommandMonitor1

    m.printRules()
    m.PRINT = true

    m.addEvent('COMMAND)("STOP_DRIVING", 1)
    m.addEvent('COMMAND)("START_CAMERA", 2)

    m.addEvent('SUCCESS)("STOP_DRIVING", 1)
    m.addEvent('SUCCESS)("START_CAMERA", 3)

    m.addEvent('end)()
  }
}

// -------------------------
// Trying time line pattern.
// -------------------------

class CommandMonitor2 extends LogicMonitor {
  val COMMAND, FAILDISP, DISPATCH, FAIL, SUCCESS = event
  initialize()

  //  timeLine("command execution")(
  //    COMMAND('x, 'y),
  //    no(FAILDISP('x, 'y)),
  //    DISPATCH('x, 'y)
  //  )(
  //    no(FAIL('x, 'y)),
  //    SUCCESS('x, 'y),
  //    no(SUCCESS('x, 'y))
  //  )

  //  timeLine("double command")(
  //    COMMAND('_, 'y))(
  //    not(COMMAND('_, 'y)))

  "command execution" when(
    COMMAND('x, 'y),
    no(FAILDISP('x, 'y)),
    DISPATCH('x, 'y)
    ) ensure(
    no(FAIL('x, 'y)),
    SUCCESS('x, 'y),
    no(SUCCESS('x, 'y))
    )
}

class DemoCommandMonitor2 {
  def main(args: Array[String]) {
    val m = new CommandMonitor2

    m.printRules()
    m.PRINT = true

    m.addEvent('COMMAND)("DRIVE", 1)
    m.addEvent('DISPATCH)("DRIVE", 1)
    m.addEvent('SUCCESS)("DRIVE", 2)
    m.addEvent('SUCCESS)("DRIVE", 3)
    m.addEvent('COMMAND)("CALLHOME", 1)
    m.addEvent('end)()
  }
}

// ------------------------------------
// Trying time line pattern hand coded.
// ------------------------------------

class CommandMonitor3 extends Monitor {
  val COMMAND, FAILDISP, DISPATCH, FAIL, SUCCESS, end = event
  val State = fact

  "r1" -- COMMAND('x, 'y) |-> State(1, 'x, 'y)
  "r2" -- State(1, 'x, 'y) & FAILDISP('x, 'y) |-> remove(State)
  "r3" -- State(1, 'x, 'y) & DISPATCH('x, 'y) |-> State(2, 'x, 'y)
  "r4" -- State(2, 'x, 'y) & FAIL('x, 'y) |-> fail()
  "r5" -- State(2, 'x, 'y) & SUCCESS('x, 'y) |-> State(3, 'x, 'y)
  "r6" -- State(2, 'x, 'y) & end() |-> fail()
  "r7" -- State(3, 'x, 'y) & SUCCESS('x, 'y) |-> fail()
}

class DemoCommandMonitor3 {
  def main(args: Array[String]) {
    val m = new CommandMonitor3

    m.printRules()

    m.addEvent('COMMAND)("DRIVE", 1)
    m.addEvent('DISPATCH)("DRIVE", 1)
    m.addEvent('SUCCESS)("DRIVE", 2)
    m.addEvent('SUCCESS)("DRIVE", 3)
    m.addEvent('COMMAND)("CALLHOME", 1)
    m.addEvent('end)()
  }
}

/*

------------------------------
Command and Resource Examples.
------------------------------

What we want to demonstrate:
+ event and fact (generating symbols)
+ positional versus map events
+ negation

- actions: insert+, remove+, update, replace+, ensure, fail+, report, symbol operations
- adding facts (in addition to events)

*/


// ======================================================================
// Command Success: An issued command, identified by a name and a running
// job number, shall eventually succeed, and must not fail before then.
// ======================================================================

class CommandSuccess extends Monitor {
  val COMMAND, FAIL, SUCCESS, end = event

  "command" -- COMMAND('name, 'number) |->
    insert('Commanded('name, 'number))

  "failure" -- 'Commanded('n, 'x) & FAIL('n, 'x) |-> {
    remove('Commanded)
    fail(s"command (${'n.s},${'x.i}) failed before success")
  }

  "success" -- 'Commanded('n, 'x) & SUCCESS('n, 'x) |->
    remove('Commanded)

  "no success" -- 'Commanded('n, 'x) & end() |->
    fail(s"command (${'n.s},${'x.i}}) never succeeded")
}

class CommandSuccessTimeLine extends LogicMonitor {
  val COMMAND, FAIL, SUCCESS, end = event
  initialize()

  timeLine("exactly one success")(
    COMMAND('n, 'x)
  )(
    no(FAIL('n, 'x)),
    SUCCESS('n, 'x)
  )
}

// =======================================================================
// Dispatch Success: An issued command, identified by a name and a running
// job number, shall first be dispactched without any dispatch failure,
// then succeed, without any failure, and then succeed exactly once.
// =======================================================================

class ExtendedDispatchSuccessTimeLine extends LogicMonitor {
  val COMMAND, FAILDISPATCH, DISPATCH, FAIL, SUCCESS, end = event
  initialize()

  timeLine("exactly one success")(
    COMMAND('n, 'x)
  )(
    no(FAILDISPATCH('n, 'x)),
    DISPATCH('n, 'x),
    no(FAIL('n, 'x)),
    SUCCESS('n, 'x),
    no(SUCCESS('n, 'x))
  )
}

// =======================================================================
// Conditional Dispatch Success: If a command is issued, and dispatched without
// a dispatch failure,then it shall succeed exactly once, without any failure.
// =======================================================================

class DispatchSuccessTimeLine extends LogicMonitor {
  val COMMAND, FAILDISPATCH, DISPATCH, FAIL, SUCCESS, end = event
  initialize()

  timeLine("exactly one success")(
    COMMAND('n, 'x),
    no(FAILDISPATCH('n, 'x)),
    DISPATCH('n, 'x)
  )(
    no(FAIL('n, 'x)),
    SUCCESS('n, 'x),
    no(SUCCESS('n, 'x))
  )
}

// =================================================================================
// Exactly One Success: An issued command, identified by a name and a running
// job number, shall eventually succeed exactly once, and must not fail before then.
// =================================================================================

class ExactlyOneSuccess extends Monitor {
  val COMMAND, FAIL, SUCCESS, end = event

  "command" -- COMMAND('name, 'number) |->
    insert('Commanded('name, 'number))

  "failure" -- 'Commanded('n, 'x) & FAIL('n, 'x) |-> {
    remove('Commanded)
    fail(s"command (${'n.s},${'x.i}) failed before success")
  }

  "success" -- 'Commanded('n, 'x) & SUCCESS('n, 'x) |->
    replace('Commanded)('Succeeded('n, 'x))

  "success again" -- 'Succeeded('n, 'x) & SUCCESS('n, 'x) |->
    fail(s"two successes of (${'n.s},${'x.i}})")

  "no success" -- 'Commanded('n, 'x) & end() |->
    fail(s"command (${'n.s},${'x.i}}) never succeeded")
}

// It gives many error messages on end. Why is that?

class ExactlyOneSuccessTimeLine extends LogicMonitor {
  val COMMAND, FAIL, SUCCESS, end = event
  initialize()

  timeLine("exactly one success")(
    COMMAND('n, 'x)
  )(
    no(FAIL('n, 'x)),
    SUCCESS('n, 'x),
    no(SUCCESS('n, 'x))
  )
}

class DemoExactlyOneSuccess {
  def main(args: Array[String]) {
    //val m = new ExactlyOneSuccess
    val m = new ExactlyOneSuccessTimeLine
    m.PRINT = true
    m.printRules()

    m.addEvent('COMMAND)("STOP_DRIVING", 1)
    m.addEvent('COMMAND)("START_CAMERA", 2)
    m.addEvent('COMMAND)("TURN_ANTENNA", 3)
    m.addEvent('FAIL)("STOP_DRIVING", 1)
    m.addEvent('SUCCESS)("START_CAMERA", 2)
    m.addEvent('SUCCESS)("START_CAMERA", 2)
    m.addEvent('end)()
  }
}

// =====================================================================================
// Respect Conflicts: A resource should not be granted if it is in conflict with another
// already granted resource.
// =====================================================================================

class RespectConflicts extends Monitor {
  val CONFLICT, GRANT, RELEASE = event
  val Conflict, Granted = fact

  "conflict" -- CONFLICT('r1, 'r2) |-> {
    insert(Conflict('r1, 'r2))
    insert(Conflict('r2, 'r1))
  }
  "grant" -- GRANT('t, 'r) |-> Granted('t, 'r)
  "grant again" -- Granted('t, 'r1) & Conflict('r1, 'r2) & GRANT('_, 'r2) |-> fail(s"${'r2.a} granted while granted")
  "release" -- Granted('t, 'r) & RELEASE('t, 'r) |-> remove(Granted)
}

// This does not quite work since it is not symmetric. We need an 'or'.

class RespectConflictsTimeLine extends LogicMonitor {
  val CONFLICT, GRANT, RELEASE = event
  initialize()

  timeLineBounded("respect conflicts")(
    CONFLICT('r1, 'r2),
    GRANT('t, 'r1)
  )(
    no(GRANT('_, 'r2))
  )(
    RELEASE('t, 'r1)
  )
}


class DemoRespectConflicts {
  def main(args: Array[String]) {
    //val m = new RespectConflicts
    val m = new RespectConflictsTimeLine
    m.PRINT = true
    m.printRules()

    val DriveTask = 1
    val CameraTask = 2

    val Wheels = 1
    val Camera = 2

    m.addEvent('CONFLICT)(Wheels, Camera)
    m.addEvent('GRANT)(CameraTask, Camera)
    m.addEvent('GRANT)(DriveTask, Wheels)
    m.addEvent('RELEASE)(DriveTask, Wheels)
    m.addEvent('RELEASE)(CameraTask, Camera)
  }
}

// ================================================================================================
// Success has a Reason: A command cannot succeed without having been issued and not yet succeeded.
// ================================================================================================

class SuccessHasAReasonTemporalLogic extends LogicMonitor {
  val COMMAND, SUCCESS = event
  initialize()

  "success has a reason" ---
    'SUCCESS('x, 'y) ==> 'COMMAND('x, 'y)
}

class SuccessHasAReason extends Monitor {
  val COMMAND, SUCCESS = event
  val Commanded = fact

  "r1" -- COMMAND('x, 'y) |-> Commanded('x, 'y)
  "r2" -- Commanded('x, 'y) & SUCCESS('x, 'y) |-> remove(Commanded)
  "r3" -- SUCCESS('x, 'y) & not(Commanded('x, 'y)) |-> fail(s"${eval(SUCCESS('x, 'y))} occurred without command")
}


class DemoSuccessHasAReason {
  def main(args: Array[String]) {
    //val m = new SuccessHasAReason
    val m = new SuccessHasAReasonTemporalLogic
    m.PRINT = true

    m.printRules()

    m.addEvent('COMMAND)("STOP_DRIVING", 1)
    m.addEvent('COMMAND)("START_CAMERA", 2)

    m.addEvent('SUCCESS)("STOP_DRIVING", 1)
    m.addEvent('SUCCESS)("START_CAMERA", 1)
  }
}

// ============================================================================
// Release Resource: If a resource is granted during the execution of a command
// then it should be released before the command succeeds.
// ============================================================================

class ReleaseResourceTimeLine extends LogicMonitor {
  val COMMAND, SUCCESS, GRANT, RELEASE = event
  initialize()

  timeLineBounded("release resource")(
    COMMAND('t, 'x),
    no(SUCCESS('t, 'x)),
    GRANT('t, 'r)
  )(
    RELEASE('t, 'r)
  )(
    SUCCESS('t, 'x)
  )
}

class ReleaseResource extends Monitor {
  val COMMAND, SUCCESS, GRANT, RELEASE = event
  val Commanded, Granted = fact

  "command" -- COMMAND('t, 'x) |-> Commanded('t, 'x)
  "grant" -- Commanded('t, 'x) & GRANT('t, 'r) |-> Granted('t, 'r, 'x)
  "release" -- Granted('t, 'r, 'x) & RELEASE('t, 'r) |-> remove(Granted)
  "good success" -- Commanded('t, 'x) & SUCCESS('t, 'x) |-> remove(Commanded)
  "bad success" -- Granted('t, '_, 'x) & SUCCESS('t, 'x) |-> fail()
}

class DemoReleaseResource {
  def main(args: Array[String]) {
    //val m = new ReleaseResource
    val m = new ReleaseResourceTimeLine
    m.PRINT = true

    m.printRules()

    val DriveTask = 1
    val CameraTask = 2

    m.addEvent('COMMAND)(DriveTask, "STOP_DRIVING")
    m.addEvent('COMMAND)(CameraTask, "START_CAMERA")
    m.addEvent('GRANT)(DriveTask, "wheels")
    m.addEvent('GRANT)(CameraTask, "camera")
    m.addEvent('RELEASE)(CameraTask, "camera")
    m.addEvent('SUCCESS)(DriveTask, "STOP_DRIVING")
    m.addEvent('SUCCESS)(CameraTask, "START_CAMERA")
  }
}

// =============================================================
// Increasing Job Numbers: Command numbers must increase by one.
// =============================================================

class IncreasingJobNumbers extends Monitor {
  val COMMAND = event
  val Counter = fact

  "command" -- Counter('n) & COMMAND('_, 'y) |-> {
    ensure('y.i == 'n.i + 1)
    update(Counter('y))
  }

  addFact(Counter)(0)
}

class DemoIncreasingJobNumbers {
  def main(args: Array[String]) {
    val m = new IncreasingJobNumbers
    m.PRINT = true

    m.addEvent('COMMAND)("STOP_DRIVING", 1)
    m.addEvent('COMMAND)("START_CAMERA", 2)
    m.addEvent('COMMAND)("TURN_ANTENNA", 3)
    m.addEvent('FAIL)("STOP_DRIVING", 1)
    m.addEvent('COMMAND)("STOP_DRIVING", 5)
    m.addEvent('SUCCESS)("START_CAMERA", 2)
    m.addEvent('SUCCESS)("TURN_ANTENNA", 3)
  }
}

// ===================================================================================
// Resource Pattern: If a resource is requested by a task then either a grant appears
// or a reject, before a new resource is requested by the same task.
// ===================================================================================

class ResourcePattern extends Monitor {
  val REQUEST, GRANT, REJECT = event
  val Requested = fact

  "good request" -- REQUEST('t, 'r) & not(Requested('t, '_)) |-> Requested('t, 'r)
  "bad request" -- REQUEST('t, 'r) & Requested('t, '_) |-> fail()
  "grant" -- GRANT('t, 'r) & Requested('t, 'r) |-> remove(Requested)
  "reject" -- REJECT('t, 'r) & Requested('t, 'r) |-> remove(Requested)
}

class DemoResourcePattern {
  def main(args: Array[String]) {
    val m = new ResourcePattern

    m.addEvent('REQUEST)(1, "camera")
    m.addEvent('REJECT)(1, "camera")
    m.addEvent('REQUEST)(1, "filesystem")
    m.addEvent('GRANT)(1, "filesystem")
    m.addEvent('REQUEST)(1, "antenna")
  }
}

// =====
// TEST:
// =====

class R1 extends LogicMonitor {
  val BEFORE, GRANT, RELEASE, end = event
  initialize()

  timeLine("never grant")()(
    no(GRANT('_, 't, 'r1))
  )
}

class DemoR1 extends Contract {
  def main(args: Array[String]) {
    val m = new R1
    setMonitor(m, true)
    m.printRules()

    add('GRANT(1040, "A", 'a1))
    add('end())
  }
}

// ---

class R2 extends LogicMonitor {
  val BEFORE, GRANT, RELEASE, end = event
  initialize()

  // does not work due to lack of handling of new parameters on consequence.

  timeLine("do grant twice")()(
    GRANT('_, 't, 'r1),
    GRANT('_, 't, 'r1)
  )
}

class DemoR2 extends Contract {
  def main(args: Array[String]) {
    val m = new R2
    setMonitor(m, true)
    m.printRules()

    add('GRANT(1040, "A", 'a1))
    add('GRANT(1040, "B", 'b1))
    add('end())
  }
}

// =================
// Respect ordering:
// =================

class RespectOrderingLogic extends LogicMonitor {
  val before, grant, release, end = event
  initialize()

  timeLineBounded("respect ordering")(
    before('r1, 'r2),
    grant('_, 't, 'r2)
  )(
    no(grant('_, 't, 'r1))
  )(
    release('_, 't, 'r2)
  )
}

object DemoRespectOrdering {
  def main(args: Array[String]) {
    val m = new RespectOrderingLogic
    m.PRINT = true
    m.printRules()

    m.addEvent('before)('a1, 'a2)
    m.addEvent('before)('b1, 'b2)
    m.addEvent('grant)(1000, "A", 'a2)
    m.addEvent('grant)(1010, "B", 'b2)
    m.addEvent('release)(1020, "B", 'b2)
    m.addEvent('grant)(1030, "B", 'b1)
    m.addEvent('grant)(1040, "A", 'a1)
    m.addEvent('end)()
  }
}

// The problem with this spec above is that it only holds for the first grant.
// One needs a regular expression semantics in order for it to make sense.

// ================
// Release granted:
// ================

class ReleaseGrantedLogic extends LogicMonitor {
  val grant, release, end = event
  initialize()

  timeLine("release granted")(
    grant('_, 't, 'r)
  )(
    release('_, 't, 'r)
  )
}

object DemoReleaseGranted {
  def main(args: Array[String]) {
    val m = new ReleaseGrantedLogic
    m.PRINT = true
    m.printRules()

    m.addEvent('grant)(1000, "A", 'a2)
    m.addEvent('grant)(1010, "B", 'b2)
    m.addEvent('release)(1020, "B", 'b2)
    m.addEvent('end)()
  }
}

// The problem with this propertu is that there are too few events.
// Could perhaps be given as a path expression:
// grant no-release end.

// =====================
// Do not grant granted:
// =====================

class DoNotGrantGranted extends LogicMonitor {
  val grant, release, end = event
  initialize()

  timeLineBounded("do not grant granted")(
    grant('_, 't, 'r)
  )(
    no(grant('_, '_, 'r))
  )(
    release('_, 't, 'r)
  )
}

object DemoDoNotGrantGranted {
  def main(args: Array[String]) {
    val m = new DoNotGrantGranted
    m.PRINT = true
    m.printRules()

    m.addEvent('grant)(1000, "A", 'r1)
    m.addEvent('grant)(1010, "B", 'r2)
    m.addEvent('release)(1020, "B", 'r2)
    m.addEvent('grant)(1030, "C", 'r1)
    m.addEvent('end)()
  }
}

// =====
// Deny:
// =====

class DenyTimeLine extends LogicMonitor {
  val request, grant, release, deny, end = event
  initialize()
  //
  // timeLine("deny")(
  //   grant('_, 't1, 'r),
  //   no(release('_, 't1, 'r)),
  //   request('_, 't2, 'r)
  //  )(
  //   deny('_, 't2, 'r)
  //  )

  "deny" when(
    grant('_, 't1, 'r),
    no(release('_, 't1, 'r)),
    request('_, 't2, 'r)
    ) ensure (
    deny('_, 't2, 'r)
    )
}

object DemoDenyTimeLine {
  def main(args: Array[String]) {
    val m = new DenyTimeLine
    m.PRINT = true
    m.printRules()

    m.addEvent('grant)(1020, "A", 'r1)
    m.addEvent('grant)(1030, "B", 'r2)
    m.addEvent('release)(1040, "B", 'r2)
    m.addEvent('request)(1050, "C", 'r2)
    m.addEvent('grant)(1060, "C", 'r2)
    m.addEvent('request)(1070, "D", 'r1)
    m.addEvent('deny)(1080, "D", 'r1)
    m.addEvent('end)()
  }
}

// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
// ---------------------------- PAPER: ----------------------------
// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// ##############################
//      POSITIONAL EVENTS:
// ##############################

trait ResourceMonitor extends Monitor {
  val before, request, deny, grant, release, end = event
  val Before, Granted, Deny = fact
}

trait Ordering extends ResourceMonitor {
  "r1" -- before('r1, 'r2) |-> Before('r1, 'r2)
  "r2" -- Before('r1, 'r2) & Before('r2, 'r3) |-> Before('r1, 'r3)
}

trait Release extends ResourceMonitor {
  "r3" -- grant('_, 't, 'r) & not(Granted('t, 'r)) |-> Granted('t, 'r)
  "r4" -- Granted('t, 'r) & release('_, 't, 'r) |-> remove(Granted)
  "r5" -- end() & Granted('t, 'r) |-> fail("missing release")
}

trait NoRelease extends ResourceMonitor {
  "r6" -- release('_, 't, 'r) & not(Granted('t, 'r)) |-> fail("bad release")
}

trait NoGrant extends ResourceMonitor {
  "r7" -- Granted('t, 'r) & grant('_, 't_, 'r) |-> fail("bad double grant")
  "r8" -- Before('r1, 'r2) & Granted('t, 'r2) & grant('_, 't, 'r1) |-> fail("bad grant order")
}

trait Deny extends ResourceMonitor {
  val Counter = fact

  "r9" -- Granted('_, 'r) & request('s, 't, 'r) |-> Deny('s, 't, 'r)
  "r10" -- Before('r1, 'r2) & Granted('t, 'r2) & request('s, 't, 'r1) |-> Deny('s, 't, 'r1)
  "r11" -- Deny('s1, 't, 'r) & Counter('n) & deny('s2, 't, 'r) |-> {
    ensure(('s2 - 's1) <= 10000 & 'n < 3)
    remove(Deny)
    update(Counter('n + 1))
  }
  "r12" -- end() & Deny('s, 't, 'r) |-> fail("missing deny")

  addFact(Counter)(0)
}

trait Deny2 extends ResourceMonitor {
  val Counter = fact

  "r9" -- Granted('_, 'r) & request('s, 't, 'r) |-> Deny('s, 't, 'r)
  "r10" -- Before('r1, 'r2) & Granted('t, 'r2) & request('s, 't, 'r1) |-> Deny('s, 't, 'r1)
  "r11" -- Deny('s1, 't, 'r) & Counter('n) & deny('s2, 't, 'r) |-> {
    if (('s2 - 's1) > 10000 || 'n >= 3) fail()
    remove(Deny)
    remove(Counter)
    insert(Counter('n + 1))
  }
  "r12" -- end() & Deny('s, 't, 'r) |-> fail("missing deny")

  addFact(Counter)(0)
}

trait Deny3 extends ResourceMonitor {
  var counter: Int = 0

  "r9" -- Granted('_, 'r) & request('s, 't, 'r) |-> Deny('s, 't, 'r)
  "r10" -- Before('r1, 'r2) & Granted('t, 'r2) & request('s, 't, 'r1) |-> Deny('s, 't, 'r1)
  "r11" -- Deny('s1, 't, 'r) & deny('s2, 't, 'r) |-> {
    ensure(('s2 - 's1) <= 10000 & counter < 3)
    remove(Deny)
    counter += 1
  }
  "r12" -- end() & Deny('s, 't, 'r) |-> fail("missing deny")
}

class ResourceManagement
  extends Ordering
  with Release
  with NoRelease
  with NoGrant
  with Deny

object ApplyResourceManagement {
  def main(args: Array[String]) {
    val m = new ResourceManagement
    m.PRINT = true

    m.addEvent('before)('wheel1, 'wheel2)
    m.addEvent('before)('wheel2, 'wheel3)
    m.addEvent('request)(1000, "drive", 'wheel3)
    m.addEvent('grant)(2000, "drive", 'wheel3)
    m.addEvent('request)(3000, "drive", 'wheel1)
    m.addEvent('grant)(4000, "drive", 'wheel1) // NoGrant violated
    m.addEvent('release)(5000, "drive", 'wheel3)
    m.addEvent('release)(6020, "drive", 'wheel1)
    m.addEvent('end)() // Deny violated due to missing deny
  }
}

// #####################
//     Map Events:
// #####################

trait OrderingMap extends ResourceMonitor {
  "r1" -- before('resource1 -> 'r1, 'resource2 -> 'r2) |-> Before('r1, 'r2)
  "r2" -- Before('r1, 'r2) & Before('r2, 'r3) |-> Before('r1, 'r3)
}

trait ReleaseMap extends ResourceMonitor {
  "r3" -- grant('task -> 't, 'resource -> 'r) & not(Granted('t, 'r)) |-> Granted('t, 'r)
  "r4" -- Granted('t, 'r) & release('task -> 't, 'resource -> 'r) |-> remove(Granted)
  "r5" -- end() & Granted('t, 'r) |-> fail("missing release")
}

trait NoReleaseMap extends ResourceMonitor {
  "r6" -- release('task -> 't, 'resource -> 'r) & not(Granted('t, 'r)) |-> fail("bad release")
}

trait NoGrantMap extends ResourceMonitor {
  "r7" -- Granted('t, 'r) & grant('task -> 't_, 'resource -> 'r) |-> fail("bad double grant")
  "r8" -- Before('r1, 'r2) & Granted('t, 'r2) & grant('task -> 't, 'resource -> 'r1) |-> fail("bad grant order")
}

trait DenyMap extends ResourceMonitor {
  val Counter = fact

  "r9" -- Granted('_, 'r) & request('time -> 's, 'task -> 't, 'resource -> 'r) |-> Deny('s, 't, 'r)
  "r10" -- Before('r1, 'r2) & Granted('t, 'r2) & request('time -> 's, 'task -> 't, 'resource -> 'r1) |-> Deny('s, 't, 'r1)
  "r11" -- Deny('s1, 't, 'r) & Counter('n) & deny('time -> 's2, 'task -> 't, 'resource -> 'r) |-> {
    ensure(('s2 - 's1) <= 10000 & 'n < 3)
    remove(Deny)
    update(Counter('n + 1))
  }
  "r12" -- end() & Deny('s, 't, 'r) |-> fail("missing deny")

  addFact(Counter)(0)
}

class ResourceManagementMap
  extends OrderingMap
  with ReleaseMap
  with NoReleaseMap
  with NoGrantMap
  with DenyMap

object ApplyResourceManagementMap {
  def main(args: Array[String]) {
    val m = new ResourceManagementMap
    m.PRINT = true

    m.addMapEvent('before)('resource1 -> 'wheel1, 'resource2 -> 'wheel2)
    m.addMapEvent('before)('resource1 -> 'wheel2, 'resource2 -> 'wheel3)
    m.addMapEvent('request)('time -> 1000, 'task -> "drive", 'resource -> 'wheel3)
    m.addMapEvent('grant)('time -> 2000, 'task -> "drive", 'resource -> 'wheel3)
    m.addMapEvent('request)('time -> 3000, 'task -> "drive", 'resource -> 'wheel1)
    m.addMapEvent('grant)('time -> 4000, 'task -> "drive", 'resource -> 'wheel1) // NoGrant violated
    m.addMapEvent('release)('time -> 5000, 'task -> "drive", 'resource -> 'wheel3)
    m.addMapEvent('release)('time -> 6020, 'task -> "drive", 'resource -> 'wheel1)
    m.addMapEvent('end)() // Deny violated due to missing deny
  }
}

// PROPERTIES USING TEMPLATES:

// ===========
// Do release:
// ===========

class ReleaseProperties extends TemporalLogic {
  val grant, release = event
  initialize()

  "Release" ---
    grant('_, 't, 'r) --> release('_, 't, 'r)

  "NoRelease" ---
    release('_, 't, 'r) ==> grant('_, 't, 'r)
}

// "Release".---(grant('_, 't, 'r)).-->(release('_, 't, 'r))

object DemoReleaseProperties {
  def main(args: Array[String]) {
    val m = new ReleaseProperties
    m.PRINT = true
    m.printRules()

    m.addEvent('grant)(1000, "A", 'r1)
    m.addEvent('grant)(1010, "B", 'r2)
    m.addEvent('release)(1020, "A", 'r1)
    m.addEvent('release)(1030, "A", 'r2)
    m.addEvent('end)()
  }
}

// =============
// Do not Grant:
// =============


class NoDoubleGrant extends PathExpressions {
  val grant, release = event
  initialize()

  println("running NoDoubleGrant")

  when("double grant")(
    grant('_, 't1, 'r),
    no(release('_, 't1, 'r)),
    grant('_, 't2, 'r)
  ) {
    fail(s"*** resource ${'r.a} acquired twice")
  }

}

object DemoNoDoubleGrant {
  def main(args: Array[String]) {
    val m = new NoDoubleGrant
    m.PRINT = true
    m.printRules()

    m.addEvent('grant)(1020, "A", 'r1)
    m.addEvent('grant)(1030, "B", 'r2)
    m.addEvent('release)(1040, "B", 'r2)
    m.addEvent('grant)(1060, "C", 'r2)
    m.addEvent('grant)(1060, "D", 'r1)
    m.addEvent('release)(1040, "C", 'r2)
    m.addEvent('release)(1040, "D", 'r1)
  }
}

// =============
// Testing Code:
// =============

class TestingCode1 extends Monitor {
  "r1" -- 'grant('_, 't, 'r) |-> 'unsafe__1('t, 'r)
  "r2" -- 'unsafe__1('t, 'r) & 'release('_, 't, 'r) |-> remove('unsafe__1)
  "r3" -- 'unsafe__1('t, 'r) & 'end() |-> fail("Release")
}

class TestingCode2 extends Monitor {
  "r4" -- 'grant('_, 't, 'r) |-> 'safe__2('t, 'r)
  "r5" -- 'release('_, 't, 'r) & not('safe__2('t, 'r)) |-> fail("NoRelease")
}

class TestingCode3 extends Monitor {
  "r1" -- 'grant('_, 't1, 'r) & 'State__1() |-> 'State__2('t1, 'r)
  "r2" -- 'release('_, 't1, 'r) & 'State__2('t1, 'r) |-> 'State__3('t1, 'r)
  "r3" -- 'grant('_, 't2, 'r) & 'State__2('t1, 'r) & not('State__3('t1, 'r)) |-> {
    remove('State__2)
    insert('State__4('t1, 'r, 't2))
  }
  "r4" -- 'grant('_3, 't2, 'r) & 'State__2('t1, 'r) & 'State__3('t1, 'r) |-> {
    remove('State__2)
    remove('State__3)
  }
  "r5" -- 'State__4('t1, 'r, 't2) |-> {
    fail("resource acquired twice")
  }
}

// ================
// DSL Explanation:
// ================

class RespectReadMode extends Monitor {
  val write = event
  val Opened = fact

  "respect mode" -- Opened('file, "read") & write('file, 'data) |-> fail("file")
}

class AST {

  trait Pattern

  case class Variable(s: Symbol) extends Pattern

  case class Constant(s: Any) extends Pattern

  trait Condition

  case class PC(constraints: Map[Symbol, Pattern]) extends Condition

  case class NC(constraints: Map[Symbol, Pattern]) extends Condition

  case class Action(code: Unit => Unit)

  case class Rule(name: String, conditions: List[Condition], action: Action)

  def fail() {}

  val rule =
    Rule(
      "respect mode",
      List(
        PC(Map('kind -> Constant('Opened), 'file -> Variable('f), 'mode -> Constant("read"))),
        PC(Map('kind -> Constant('write), 'file -> Variable('f)))),
      Action((x: Unit) => fail()))
}

class Rete extends AST {
  def addRule(rule: Rule) {}

  def fail(msg: String) {}
}

class LogFire extends Rete {
  implicit def nameToRuleDef(name: String) = new {
    def --(c: Condition) = new RuleDef(name, List(c))
  }

  class RuleDef(name: String, conditions: List[Condition]) {
    def &(c: Condition) = new RuleDef(name, c :: conditions)

    def |->(stmt: => Unit) {
      addRule(Rule(name, conditions.reverse, Action((x: Unit) => stmt)))
    }
  }

  implicit def symbolToCond(kind: Symbol) = new Cond(kind)

  class Cond(kind: Symbol) {
    def apply(args: (Symbol, Any)*): PC = {
      val constraints =
        for ((field, value) <- args.toMap) yield {
          val pattern = value match {
            case symbol: Symbol => Variable(symbol)
            case _ => Constant(value)
          }
          (field -> pattern)
        }
      PC(constraints + ('kind -> Constant(kind)))
    }
  }

  def not(pc: PC): Condition = NC(pc.constraints)
}

class MyMonitor extends LogFire {
  val write = 'write
  val Opened = 'Opened

  "respect mode" -- Opened('file -> 'f, 'mode -> "read") & write('file -> 'f) |-> fail("file")
}

// ======================
// GraphViz Illustration:
// ======================

class GraphViz extends ResourceMonitor {
  val write = 'write
  val Opened = 'Opened

  "respect mode" -- Opened('file -> 'f, 'mode -> "read") & write('file -> 'f) |-> fail("file")

  // "r8" -- Before('r1, 'r2) & Granted('t, 'r2) & grant('_, 't, 'r1) |-> fail("bad grant order")
  // "r10" -- Before('r1, 'r2) & Granted('t, 'r2) & request('s, 't, 'r1) |-> Deny('s, 't, 'r1)


}

object DemoGraphViz {
  def main(args: Array[String]) {
    val m = new GraphViz
    m.draw("/Users/khavelun/Desktop/scalashow.dot")
  }
}

// ==========================
// Illustrating Unit Testing:
// ==========================


class TestResourceManagement extends Contract {
  val m = new ResourceManagement
  setMonitor(m, false)

  add('grant(1, "A", 'motor3))
  facts(
    'Counter(0),
    'Granted("A", 'motor3)
  )

  add('grant(2, "B", 'motor3))
  facts(
    'Counter(0),
    'Granted("A", 'motor3),
    'Granted("B", 'motor3)
  )

  result(
    Report(
      "ERROR bad double grant",
      (1, "r3", 'Granted("A", 'motor3)),
      (2, "r7", 'Fail("ERROR bad double grant")))
  )
}

object TestNL {

  object There {
    def is = new {
      def a = new {
        def cheese {
          println("There is a cheese")
        }
      }
    }
  }

  def main(args: Array[String]) {
    There.is.a.cheese
  }
}

// =======
// Slides:
// =======

class GraphViz2 extends ResourceMonitor {
  "r1" -- grant('t, 'r) & not(Granted('t, 'r)) |-> Granted('t, 'r)

  "r2" -- Granted('t, 'r) & release('t, 'r) |-> remove(Granted)

  "r3" -- Granted('t, 'r) & grant('_, 'r) |-> fail()
}

object DemoGraphViz2 {
  def main(args: Array[String]) {
    val m = new GraphViz2
    m.draw("/Users/khavelun/Desktop/scalashow.dot")
  }
}

class Abstract1 extends Monitor {
  val EVR = event
  val Started, Phase = fact

  "r1" -- EVR('command -> "START", 'name -> 'n, 'time -> 't1) |-> Started('n, 't1)

  "r2" -- EVR('command -> "STOP", 'name -> 'n, 'time -> 't2) & Started('n, 't1) |-> Phase('n, 't1, 't2)

  "r3" -- Phase('n1, 't1, 't2) & Phase('n2, 't3, 't4) |->
    ensure(s"phrases should not overlap ${'t1.i}-${'t2.i}:${'t3.i}-${'t4.i}")(
      'n1.s == 'n2.s || 't2.i < 't3.i || 't4.i < 't1.i)
}

class Abstract2 extends Monitor {
  val EVR = event
  val Started, Phase = fact

  "r1" -- EVR('command -> "START", 'name -> 'n, 'time -> 't1) |-> Started('n, 't1)

  "r2" -- EVR('command -> "STOP", 'name -> 'n, 'time -> 't2) & Started('n, 't1) |-> Phase('n, 't1, 't2)

  "r3" -- Phase('n1, 't1, 't2) & Phase('n2, 't3, 't4) |->
    ensure(s"phrases should not overlap ${'t1.i}-${'t2.i}:${'t3.i}-${'t4.i}")(
      'n1.s == 'n2.s || 't2.i < 't3.i || 't4.i < 't1.i)
}

object DemoAbstract {
  def main(args: Array[String]) {
    val m = new Abstract1
    m.PRINT = true
    m.printRules()

    m.addMapEvent('EVR)('command -> "START", 'name -> "motor1", 'time -> 10)
  }
}

// ================================
// Illistrating DSL Implementation:
// ================================

class Release2 extends Monitor {
  val release = event
  val Granted = fact

  "r4" --
    Granted('task ->'t, 'resource -> 'r) &
    release('task -> 't, 'resource -> 'r) |->
    remove(Granted)
/*
  def R(s:String) = nameToRuleDefinition(s)
  def C(s:Symbol) = kindToConditionArguments(s)

  R("r4").--(
    C('Granted).apply(('task,'t), ('resource,'r))
   ).&(
     C('release).apply(('task,'t), ('resource,'r))
    ).|-> (
    remove(Granted)
  )
*/
}

object DemoRelease2 {
  def main(args: Array[String]) {
    val m = new Release2
    m.draw("/Users/khavelun/Desktop/scalashow.dot")
  }
}