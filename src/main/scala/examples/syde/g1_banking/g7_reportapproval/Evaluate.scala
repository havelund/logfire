package examples.syde.g1_banking.g7_reportapproval

import rete._

/**
 * Report approval.
 *
 * The property represents an approval policy for publishing
 * business reports within a company. The property requires that any report must
 * be approved prior to its publication. Furthermore, the property asks that the
 * person who publishes the report must be an accountant and the person who
 * approves the publication must be the accountants manager. Finally, the approval
 * must happen within at most 10 days before the publication.
 */

// Use this:

// Use this:

class ReportApproval1 extends Monitor {
  "r1" -- 'mgr_S('m, 'a) |-> 'Mgr('m, 'a)
  "r2" -- 'Mgr('m, 'a) & 'mgr_F('m, 'a) |-> remove('Mgr)
  "r3" -- 'acc_S('a) |-> 'Acc('a)
  "r4" -- 'Acc('a) & 'acc_F('a) |-> remove('Acc)

  "r5" -- 'approve('m, 'f, 'ts) & 'Mgr('m, 'a) & not('Appr('a, 'f, '_)) |-> insert('Appr('a, 'f, 'ts))
  "r6" -- 'Appr('a, 'f, '_) & 'approve('m, 'f, 'ts) & 'Mgr('m, 'a) |-> update('Appr('a, 'f, 'ts))

  "r7" -- 'publish('a, '_, '_) & not('Acc('a)) |-> fail()
  "r8" -- 'publish('a, 'f, '_) & not('Appr('a, 'f, '_)) |-> fail()
  "r9" -- 'Appr('a, 'f, 'ts1) & 'publish('a, 'f, 'ts2) |-> ensure('ts2 - 'ts1 <= 10)
}

class ReportApproval2 extends Monitor {
  // Events:
  val approve, publish, mgr_S, mgr_F, acc_S, acc_F = event

  // Types making variable declarations easier to read:
  type Manager = String
  type Accountant = String
  type Publication = String
  type Time = Int

  // Variable declarations:
  var managerOf: Map[Manager, Set[Accountant]] = Map()
  var accountants: Set[Accountant] = Set()
  var approved: Map[Publication, Set[(Accountant, Time)]] = Map()

  // Print function to see what's going on:

  def print() {
    println("managerOf: " + managerOf)
    println("accountants: " + accountants)
    println("approved: " + approved)
  }

  // Rules:

  "mgr_S" -- mgr_S('m, 'a) |-> {
    managerOf = managerOf + ('m.s -> (managerOf.getOrElse('m.s, Set()) + 'a.s))
    print()
  }

  "mgr_F" -- mgr_F('m, 'a) |-> {
    managerOf = managerOf + ('m.s -> (managerOf('m.s) - 'a.s))
    print()
  }

  "acc_S" -- acc_S('a) |-> {
    accountants += 'a.s
    print()
  }

  "acc_F" -- acc_F('a) |-> {
    accountants -= 'a.s
    print()
  }

  "approve" -- approve('m, 'f, 'ts) |-> {
    approved += ('f.s -> (for (a <- managerOf('m.s)) yield (a, 'ts.i)))
    print()
  }

  "publish" -- publish('a, 'f, 'ts2) |-> {
    ensure(accountants contains 'a.s)
    ensure(approved('f.s).exists {
      case (a, ts1) => a == 'a.s && 'ts2 - ts1 <= 10
    })
    print()
  }

}

object Evaluate extends MonitorFeeder {
  val approve = 'approve
  val publish = 'publish
  val mgr_S = 'mgr_S
  val mgr_F = 'mgr_F
  val acc_S = 'acc_S
  val acc_F = 'acc_F

  val trace1 = List(
    mgr_S("John", "Mike"),
    mgr_S("Marie", "Peter"),
    acc_S("Mike"),
    acc_S("Peter"),
    approve("John", "TR-41", 10),
    publish("Mike", "TR-41", 15),
    mgr_F("Marie", "Peter"),
    acc_F("Peter"))

  val trace_1 = List(
    mgr_S("John", "Mike"),
    mgr_S("Marie", "Peter"),
    acc_S("Mike"),
    acc_S("Peter"),
    approve("John", "TR-41", 10),
    publish("Mike", "TR-41", 30),
    mgr_F("Marie", "Peter"),
    acc_F("Peter"))

  val trace_2 = List(
    mgr_S("John", "Mike"),
    mgr_S("Marie", "Peter"),
    acc_S("Peter"),
    approve("John", "TR-41", 10),
    publish("Mike", "TR-41", 15),
    mgr_F("Marie", "Peter"),
    acc_F("Peter"))

  val trace_3 = List(
    mgr_S("John", "Mike"),
    acc_S("Mike"),
    approve("John", "TR-41", 10),
    mgr_F("John", "Mike"),
    mgr_S("James", "Mike"),
    approve("James", "TR-41", 100),
    publish("Mike", "TR-41", 105))

  def main(args: Array[String]) {
    val m = new ReportApproval1
    m.PRINT = true
    trace_3 foreach m.addMapEvent
  }
}
