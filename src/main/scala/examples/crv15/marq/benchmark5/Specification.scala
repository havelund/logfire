package examples.crv15.marq.benchmark5

import rete._

/*
 * A client has accounts, recorded by createAccount(client,account). 
 * Managers are assigned to accounts, recorded by assign(account,manager). 
 * A manager can be assigned to multiple accounts and an account can be assigned 
 * multiple managers. The property is that every client has two managers with 
 * combined oversight of its accounts. This means that for every client there 
 * exists two managers who, between them, are assigned to all accounts belonging 
 * to that client. 
 * 
 * Events:
 * 
 * - createAccount(client,account)
 * - assign(account,manager)
 */

class M extends Monitor {
  val createAccount, assign = event
  val CliAcc, AccMan, CliMan, CliManPair, End = fact

  "r1" -- createAccount('client, 'account) |->
    insert(CliAcc('client, 'account))

  "r2" -- assign('account, 'manager) |->
    insert(AccMan('account, 'manager))

  "r3" -- CliAcc('client, 'account) & AccMan('account, 'manager) |->
    insert(CliMan('client, 'manager))

  "r4" -- CliMan('client, 'manager1) & CliMan('client, 'manager2) |-> {
    if ('manager1.s <= 'manager2.s) { // avoid duplicate information
      insert(CliManPair('client, 'manager1, 'manager2))
    }
  }

  "r5" -- END() |-> End()

  "r6" -- End() & CliAcc('client, 'account) & CliManPair('client, 'manager1, 'manager2) &
    not(AccMan('account, 'manager1)) & not(AccMan('account, 'manager2)) |->
    remove(CliManPair)

  "r7" -- End() & CliAcc('client, '_) & not(CliManPair('client, '_, '_)) |->
    fail("Client does not have 1-2 managers managing all accounts")
}

object Evaluate extends MonitorFeeder {
  val createAccount = 'createAccount
  val assign = 'assign

  val trace1 = List(
    createAccount("C", "A"),
    createAccount("C", "B"),
    assign("A", "M"),
    assign("B", "M"))

  val trace2 = List(
    createAccount("C", "A"),
    createAccount("C", "B"),
    createAccount("C", "D"),
    assign("A", "M"),
    assign("B", "N"),
    assign("D", "M"))

  val trace3 = List(
    createAccount("C", "A"),
    createAccount("C", "B"),
    assign("A", "M"),
    assign("B", "M"),
    assign("B", "N"),
    assign("B", "K"))

  val trace_1 = List(
    createAccount("C", "A"),
    createAccount("C", "B"),
    assign("A", "M"))

  val trace_2 = List(
    createAccount("C", "A"),
    createAccount("C", "B"),
    createAccount("C", "D"),
    assign("A", "M"),
    assign("B", "N"),
    assign("D", "K"))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_2 foreach m.addMapEvent
    m.terminate()
  }
}
