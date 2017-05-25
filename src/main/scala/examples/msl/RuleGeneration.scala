package examples.msl

import rete._

// === DSL: =========================================================

class OneCommand1 extends Monitor {
  val EVR = event
  val Commanded = fact

  "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
  "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
  "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)
}

// === addRule version 1: ===========================================

class OneCommand2 extends Monitor {
  // "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
  addRule(
    "r1",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
      (false, 'Commanded, Map('name -> 'n), null)),
    List(
      ('Commanded, Map('name -> 'n))),
    Nil)

  // "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
  addRule(
    "r2",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
      (true, 'Commanded, Map('name -> 'n), null)),
    Nil,
    Nil,
    "two commands")

  // "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)   
  addRule(
    "r3",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "STOP"), null),
      (true, 'Commanded, Map('name -> 'n), 'l)),
    Nil,
    List('l))
}

// === addRule version 2: ===========================================

class OneCommand3 extends Monitor {
  // "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
  val code1: Unit => Any = {
    case _ =>
      insert('Commanded('name -> 'n))
  }
  addRule(
    "r1",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
      (false, 'Commanded, Map('name -> 'n), null)),
    code1)

  // "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
  val code2: Unit => Any = {
    case _ =>
      fail(s"two ${'n.s} commands")
  }
  addRule(
    "r2",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
      (true, 'Commanded, Map('name -> 'n), null)),
    code2)

  // "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)   
  val code3: Unit => Any = {
    case _ =>
      remove('l)
  }
  addRule(
    "r3",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "STOP"), null),
      (true, 'Commanded, Map('name -> 'n), 'l)),
    code3)
}

// === addRule version 2 inlined: ===================================

class OneCommand3Inlined extends Monitor {
  // "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
  addRule(
    "r1",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
      (false, 'Commanded, Map('name -> 'n), null)),
    {
      case _ =>
        insert('Commanded('name -> 'n))
    })

  // "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
  addRule(
    "r2",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
      (true, 'Commanded, Map('name -> 'n), null)),
    {
      case _ =>
        fail(s"two ${'n.s} commands")
    })

  // "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)   
  addRule(
    "r3",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "STOP"), null),
      (true, 'Commanded, Map('name -> 'n), 'l)),
    {
      case _ =>
        remove('l)
    })
}

// === Running previous versions: ===================================

object RunIt1 {
  def main(args: Array[String]) {
    val m = new OneCommand3Inlined
    m.PRINT = true
    m.printRules
    m.addMapEvent('EVR)('name -> "reboot", 'message -> "START") // 1
    m.addMapEvent('EVR)('name -> "reboot", 'message -> "STOP") // 2
    m.addMapEvent('EVR)('name -> "turn", 'message -> "START") // 3 start of turn
    m.addMapEvent('EVR)('name -> "turn", 'message -> "START") // 4 error: start again of turn
    m.addMapEvent('EVR)('name -> "turn", 'message -> "STOP") // 5
  }
}

// === addRule version 2 - with time:  ==============================

class OneCommand4 extends Monitor {
  // "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
  val code1: Unit => Any = {
    case _ =>
      insert('Commanded('name -> 'n, 'time -> 't))
  }
  addRule(
    "r1",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START", 'time -> 't), null),
      (false, 'Commanded, Map('name -> 'n), null)),
    code1)

  // "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
  val code2: Unit => Any = {
    case _ =>
      fail(s"two ${'n.s} commands")
  }
  addRule(
    "r2",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
      (true, 'Commanded, Map('name -> 'n), null)),
    code2)

  // "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)
  val code3: Unit => Any = {
    case _ =>
      remove('l)
      if (('t.i - 't_commanded.i) > 500) {
        fail(s"${'n.s} stopped only after ${'t.i - 't_commanded.i} seconds")
      }
  }
  addRule(
    "r3",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "STOP", 'time -> 't), null),
      (true, 'Commanded, Map('name -> 'n, 'time -> 't_commanded), 'l)),
    code3)
}

// === addRule version 2 - inlining code:  ==========================

class OneCommand5 extends Monitor {
  // "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
  addRule(
    "r1",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START", 'time -> 't), null),
      (false, 'Commanded, Map('name -> 'n), null)),
    {
      case _ =>
        insert('Commanded('name -> 'n, 'time -> 't))
    })

  // "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
  addRule(
    "r2",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
      (true, 'Commanded, Map('name -> 'n), null)),
    {
      case _ =>
        fail(s"two ${'n.s} commands")
    })

  // "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)
  addRule(
    "r3",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "STOP", 'time -> 't), null),
      (true, 'Commanded, Map('name -> 'n, 'time -> 't_commanded), 'l)),
    {
      case _ =>
        remove('l)
        if (('t.i - 't_commanded.i) > 500) {
          fail(s"${'n.s} stopped only after ${'t.i - 't_commanded.i} seconds")
        }
    })
}

// === addRule version 2 - using  getArgs:  =========================

class OneCommand6 extends Monitor {
  // "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
  addRule(
    "r1",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START", 'time -> 't), null),
      (false, 'Commanded, Map('name -> 'n), null)),
    {
      case _ =>
        insert('Commanded('name -> 'n, 'time -> 't))
    })

  // "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
  addRule(
    "r2",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "START"), 'evr),
      (true, 'Commanded, Map('name -> 'n), null)),
    {
      case _ =>
        val evrMap: Map[Symbol, Any] = getArgs('evr)
        val command = evrMap('name)
        fail(s"two $command commands")
    })

  // "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)
  addRule(
    "r3",
    List(
      (true, 'EVR, Map('name -> 'n, 'message -> "STOP", 'time -> 't), 'evr),
      (true, 'Commanded, Map('name -> 'n, 'time -> 't_commanded), 'com)),
    {
      case _ =>
        remove('com)
        val evrMap: Map[Symbol, Any] = getArgs('evr)
        val comMap: Map[Symbol, Any] = getArgs('com)
        val timeEvr = evrMap('time).asInstanceOf[Int]
        val timeCom = comMap('time).asInstanceOf[Int]
        if ((timeEvr - timeCom) > 500) {
          fail(s"${'n.s} stopped only after ${'t.i - 't_commanded.i} seconds")
        }
    })
}

// === Running previous versions: ===================================

object RunIt2 {
  def main(args: Array[String]) {
    val m = new OneCommand6
    m.PRINT = true
    m.printRules
    m.addMapEvent('EVR)('name -> "reboot", 'message -> "START", 'time -> 1000) // 1
    m.addMapEvent('EVR)('name -> "reboot", 'message -> "STOP", 'time -> 2000) // 2
    m.addMapEvent('EVR)('name -> "turn", 'message -> "START", 'time -> 3000) // 3 start of turn
    m.addMapEvent('EVR)('name -> "turn", 'message -> "START", 'time -> 4000) // 4 error: start again of turn
    m.addMapEvent('EVR)('name -> "turn", 'message -> "STOP", 'time -> 5000) // 5
  }
}