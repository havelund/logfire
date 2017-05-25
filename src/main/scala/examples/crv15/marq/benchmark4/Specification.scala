package examples.crv15.marq.benchmark4

import rete._

/*
 * This specification is from the setting of SQL queries being taken as 
 * input to a server and then used on a database. To prevent malicious 
 * behaviour such as a query string ’; DROP TABLE table; it is necessary 
 * to sanitise these query strings before use. It is also necessary to 
 * ensure that no query strings derived from the original query string are 
 * used before sanitisation. The property we want to monitor is that every 
 * string s2 derived from an input string s1 is sanitised before use. Note 
 * that once a string has been sanitised then all strings derived from it 
 * are safe to use. We abstractly capture the input of a string as input(string), 
 * the derivation of a string as derive(string,string), the use of a string as 
 * use(string) and the sanitisation of a string as sanitise(string). As we want 
 * to capture an arbitrary number of derivations, it is not possible to quantify 
 * over the used string in QEA. We capture the property in the following 
 * unquantified QEA, which uses non-determinism to track derived strings.
 * 
 * Events:
 * 
 * - input(string)
 * - derive(string,string)
 * - sanitise(string)
 * - use(string)
 * 
 * Note: strings derived from a dirty string stay dirty forever with the spec below. 
 * This should (most likely) be changed.
 */

class M extends Monitor {
  val input, derive, sanitise, use = event
  val Dirty = fact
  
  "r1" -- input('s) |-> insert(Dirty('s))

  "r2" -- Dirty('s1) & derive('s1,'s2) |-> insert(Dirty('s2))

  "r3" -- Dirty('s) & sanitise('s) |-> remove(Dirty)

  "r4" -- Dirty('s) & use('s) |-> fail("unsanitised string used")
}

object Evaluate extends MonitorFeeder {
  val input = 'input
  val derive = 'derive
  val sanitise = 'sanitise
  val use = 'use

  val trace1 = List(
    input("A"),
    sanitise("A"),
    use("A"))

  val trace2 = List(
    input("A"),
    derive("A", "B"),
    derive("A", "C"),
    sanitise("C"),
    use("C"))

  val trace3 = List(
    input("A"),
    derive("A", "B"),
    derive("A", "C"),
    derive("C", "D"),
    derive("D", "E"))

  val trace_1 = List(
    input("C"),
    use("C"))

  val trace_2 = List(
    input("A"),
    derive("A", "B"),
    derive("B", "C"),
    sanitise("B"),
    use("C"))

  val trace_3 = List(
    input("A"),
    derive("A", "B"),
    derive("B", "C"),
    sanitise("C"),
    input("C"),
    use("C"))

  val trace_4 = List(
    input("A"),
    derive("A", "B"),
    sanitise("A"),
    derive("B", "C"),
    derive("C", "D"),
    sanitise("B"),
    use("D"))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_4 foreach m.addMapEvent
    m.terminate()
  }
}
