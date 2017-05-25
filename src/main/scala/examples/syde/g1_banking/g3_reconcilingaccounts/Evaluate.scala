package examples.syde.g1_banking.g3_reconcilingaccounts

import rete._

/**
 * Reconciling accounts.
 *
 * The administrator must reconcile accounts every 1000
 * attempted external money transfers or an aggregate total of one million dollars
 * in attempted external transfers (attempted transfers include transfers requested
 * which never took place due to lack of funds).
 */

/*
 * Questions:
 * 1. In the LARVA spec there is also a user id.
 */

class ReconcilingAccounts1 extends Monitor {
  "r1" -- 'Sums('_, '_) & 'reconcile() |-> update('Sums(0, 0))
  "r2" -- 'Sums('count, 'total) & 'transfer('a) |-> {
    if ('count + 1 > 1000 || 'total + 'a > 1000000)
      fail()
    else
      update('Sums('count + 1, 'total + 'a))
  }

  addFact('Sums)(0, 0)
}

class ReconcilingAccounts2 extends Monitor {
  var count: Int = 0
  var total: Int = 0

  def print() {
    println(s"count=$count total=$total")
  }
  
  "r1" -- 'reconcile() |-> {
    count = 0; total = 0
    print()
  }
  "r2" -- 'transfer('a) |-> {
    if (count + 1 > 1000 || total + 'a > 1000000)
      fail()
    else {
      count += 1; total += 'a
    }
    print()
  }
}

object Evaluate extends MonitorFeeder {
  val reconcile = 'reconcile
  val transfer = 'transfer

  val trace1 = List(transfer(200000), transfer(100000), reconcile(), transfer(800000), transfer(200000), reconcile())
  val trace2 = for (i <- 1 to 1000) yield transfer(1)
  val trace_1 = List(transfer(900000), transfer(200000))
  val trace_2 = List(transfer(200000), transfer(100000), reconcile(), transfer(800000), transfer(200000), transfer(1))
  val trace_3 = for (i <- 1 to 1001) yield transfer(1)

  def main(args: Array[String]) {
    val m = new ReconcilingAccounts2
    m.PRINT = true
    trace_3 foreach m.addMapEvent
  }
}
