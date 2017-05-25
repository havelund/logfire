package examples.syde.g4_concurrency.g1_lockedactions

import rete._

/**
 * Locked actions.
 *
 * This property captures the correct guarding of an action by a single lock. The
 * behaviour of this lock is given by the following two properties:
 * 1. A thread has to successfully call lock before calling an action
 * 2. A call to lock only returns true if no thread is currently holding the lock.
 * A thread must also call run before calling any actions. There are therefore three
 * valid states a thread can be in:
 *
 * 1. Not having yet called run, it cannot perform actions
 * 2. Having called run and holding the lock, it can perform actions
 * 3. Having called run and not holding the lock, it cannot perform actions
 *
 * Events run and action are paremterised by a thread and the event lock is
 * parameterised by a thread and a success condition.
 * 
 * DISCSSION:
 * - there is only one lock for all threads it seems.
 * - there is no stop run.
 */

class LockedActions extends Monitor {
  val run, lock, unlock, action = event
  val Running, HasLock = fact

  "r1" -- run('t) & not(Running('t)) & not(HasLock('t)) |-> insert(Running('t))

  "r2" -- Running('t) & run('t) |-> fail()

  "r3" -- HasLock('t) & run('t) |-> fail()

  "r4" -- Running('t) & lock('t) |-> {
    remove(Running)
    insert(HasLock('t))
  }

  "r5" -- lock('t) & not(Running('t)) |-> fail()

  "r6" -- HasLock('t) & unlock('t) |-> {
    remove(HasLock)
    insert(Running('t))
  }

  "r7" -- unlock('t) & not(HasLock('t)) |-> fail()

  "r8" -- HasLock('_) & lock('_) |-> fail()

  "r9" -- action('t) & not(HasLock('t)) |-> fail()
}

object Evaluate extends MonitorFeeder {
  val run = 'run
  val lock = 'lock
  val unlock = 'unlock
  val action = 'action

  val trace1 = List(
    run(1),
    lock(1),
    run(2),
    action(1),
    action(1),
    unlock(1),
    lock(2),
    action(2),
    unlock(2),
    lock(1),
    action(1),
    unlock(1))

  val trace_1 = List(
    run(1),
    lock(2),
    unlock(1),
    lock(1),
    run(2),
    action(2),
    action(1),
    action(1),
    unlock(1),
    action(1),
    lock(2),
    action(2),
    unlock(2),
    lock(1),
    action(1),
    unlock(1))    
    
  def main(args: Array[String]) {
    val m = new LockedActions
    m.PRINT = true
    trace1 foreach m.addMapEvent
  }
}
