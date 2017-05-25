package examples.syde.g1_banking.g4_maximalwithdrawals

import rete._

/**
 * The number of withdrawal operations performed within 10 minutes
 * before customer logs off is less than or equal to the allowed
 * limit (assumed to be 3). It is assumed that a trace records the
 * activities of a single user.
 */

/*
 * Using a global variable holding time stamps since last logoff.
 * This essetially corresponds to storing part of the trace
 * and analyze it backwards on a logoff event.
 */

class MaximalWithdrawals1 extends Monitor {
  val withdraw, logoff = event

  var withdrawals: List[Int] = Nil

  "withdraw" -- withdraw('time) |-> {
    withdrawals ::= 'time
  }

  "logoff" -- logoff('time) |-> {
    ensure(withdrawals.takeWhile(t => 'time - t <= 10).size <= 3)
    withdrawals = Nil
  }
}

/*
 * Using facts only and testing on fact memory using getFacts.
 */

class MaximalWithdrawals2 extends Monitor {
  val withdraw, logoff = event
  val Withdrawal = fact

  "withdraw" -- withdraw('time) |-> insert(Withdrawal('time))

  "logoff" -- logoff('time) |-> {
    val withDrawals = getMaps('time.i - _('one).asInstanceOf[Int] <= 10)
    ensure(withDrawals.size <= 3)
    clearFacts()
  }

  "keep" -- Withdrawal('_) & 'neveroccurs() |-> {}
}

/**
 * Using facts only and testing on facts on left hand-side of logoff rule.
 */

class MaximalWithdrawals3 extends Monitor {
  val withdraw, logoff = event
  val Withdrawal = fact

  "withdraw" -- withdraw('time) |-> insert(Withdrawal('time))

  "logoff" -- Withdrawal('time1) & Withdrawal('time2) & Withdrawal('time3) & Withdrawal('time4) & logoff('time) |-> {
    if ('time1 < 'time2 & 'time2 < 'time3 & 'time3 < 'time4 & 'time - 'time1 <= 10) fail()
    clearFacts()
  }
}

/*
 * Using non-determinism (as Giles' QEA spec).
 */

class MaximalWithdrawals4 extends Monitor {
  val withdraw, logoff = event
  val Withdraw, LogOffDanger = fact

  "withdraw init" -- withdraw('time) |-> insert(Withdraw('time, 1))

  "withdraw next" -- Withdraw('time, 'count) & withdraw('time2) |-> {
    if ('time2 - 'time <= 10) {
      if ('count < 3)
        update(Withdraw('time, 'count + 1))
      else
        replace(Withdraw)(LogOffDanger('time))
    } else
      remove(Withdraw)
  }

  "logoff" -- LogOffDanger('time) & logoff('time2) |-> {
    remove(LogOffDanger)
    if ('time2 - 'time <= 10) fail()
  }

  "withraw final" -- LogOffDanger('time) & withdraw('time2) |-> {
    if ('time2 - 'time > 10) remove(LogOffDanger)
  }
}

/*
 * Using non-determinism, but without a LogOffDanger fact.
 */

class MaximalWithdrawals5 extends Monitor {
  "r1" -- 'withdraw('time) |-> insert('Count('time, 1))

  "r2" -- 'Count('time, 'count) & 'withdraw('time2) |-> {
    if ('time2 - 'time <= 10)
      update('Count('time, 'count + 1))
    else
      remove('Count)
  }

  "r3" -- 'Count('time, 'count) & 'logoff('time2) |-> {
    ensure('time2 - 'time > 10 || 'count <= 3)
    remove('Count)
  }
}

object Evaluate extends MonitorFeeder {
  val withdraw = 'withdraw
  val logoff = 'logoff

  val trace1 = List(withdraw(1), withdraw(11), withdraw(13), withdraw(14), logoff(15), withdraw(16))
  val trace_1 = List(withdraw(1), withdraw(2), withdraw(3), withdraw(4), logoff(5))

  def main(args: Array[String]) {
    val m = new MaximalWithdrawals5
    m.PRINT = true
    trace_1 foreach m.addMapEvent
  }
}
