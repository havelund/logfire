package examples.crv15.breach.benchmark4

import rete._
import math._

// Same spec as benchmark5

/*
 * phi := 
 *   ev_[0, 50] 
 *     (
 *       (abs(yaw[t])< 5) -- p 
 *          and 
 *       (
 *         (
 *           ((abs(lws[t] + rws[t]) < 10) and (rws[t] > 10)) 
 *             or 
 *           ((abs(lws[t] + rws[t]) < 10) and (lws[t] > 10))
 *         ) -- q
 *           until_[1, 50] 
 *         (
 *           (abs(yaw[t])< 5) -- r = p
 *             and
 *           (
 *             (
 *               ((abs(lws[t] + rws[t]) < 10) and (rws[t] > 10)) 
 *                 or 
 *               ((abs(lws[t] + rws[t]) < 10) and (lws[t] > 10))
 *             ) -- s  = q
 *               until_[1, 50] 
 *             (abs(yaw[t])< 5) -- t = p
 *           )
 *         )
 *       )
 *  )
 * 
 *   
 *  ev_[0, 50] (p and (q U[1,50] (p and (q U[1,50] p))))
 * 
 */

// time
// x
// y,
// z
// pitch
// roll
// yaw
// dist
// angle
// ax
// ay,
// az
// bump_l
// bump_r
// drop_l
// drop_r
// drop_f
// cliff_l
// cliff_fl
// cliff_fr
// cliff_r
// lws
// rws
// state

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map();
    map += 'kind -> 'evt
    map += 'time -> record.get("time").toFloat
    map += 'yaw -> record.get("yaw").toFloat
    map += 'lws -> record.get("lws").toFloat
    map += 'rws -> record.get("rws").toFloat
    addMapEvent(map)
  }
}

// This spec does not find the bug in Benchmark 4.

class M extends CSVMonitor {
  val evt = event
  val I1, I2, Ok = fact

  def p(yaw: Float): Boolean = abs(yaw) < 5

  def q(lws: Float, rws: Float): Boolean =
    (abs(lws + rws) < 10 && rws > 10) ||
      (abs(lws + rws) < 10 && lws > 10)

  "r1" -- evt('time -> 't, 'yaw -> 'y, 'lws -> 'l, 'rws -> 'r) |-> {
    if ('t.float <= 50 & p('y.float) & q('l.float, 'r.float))
      insert(I1('t))
  }

  "r2" -- evt('time -> 't, 'lws -> 'l, 'rws -> 'r) & I1('t1) |-> {
    if ('t.float - 't1.float > 50 || !q('l.float, 'r.float))
      remove(I1)
  }

  "r3" -- evt('time -> 't, 'yaw -> 'y, 'lws -> 'l, 'rws -> 'r) & I1('t1) |-> {
    if ('t.float - 't1.float <= 50 & q('l.float, 'r.float) & p('y.float))
      insert(I2('t))
  }

  "r4" -- evt('time -> 't, 'lws -> 'l, 'rws -> 'r) & I2('t2) |-> {
    if ('t.float - 't2.float > 50 || !q('l.float, 'r.float))
      remove(I2)
  }

  "r5" -- evt('time -> 't, 'yaw -> 'y) & I2('t2) |-> {
    if ('t.float - 't2.float <= 50 & p('y.float))
      insert(Ok())
  }

  "r6" -- END() & not(Ok()) |-> fail("property not satisfied")
}

object Evaluate extends MonitorFeeder {
  val P = 4f // abs(yaw) < 5
  val Q = 15f // abs(lws + rws) < 10 && rws > 10

  val trace1 = List(
    Map('kind -> 'evt, 'time -> 0f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 20f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 30f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 40f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(40)
    Map('kind -> 'evt, 'time -> 50f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 60f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 70f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 80f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(80)
    Map('kind -> 'evt, 'time -> 90f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 100f, 'yaw -> P, 'lws -> -10f, 'rws -> 10f), // P(100)
    Map('kind -> 'evt, 'time -> 110f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 120f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 130f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 140f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 150f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 160f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f))

  val trace2 = List(
    Map('kind -> 'evt, 'time -> 0f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 20f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(20)
    Map('kind -> 'evt, 'time -> 30f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 40f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f), // not Q(40)
    Map('kind -> 'evt, 'time -> 50f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(50)
    Map('kind -> 'evt, 'time -> 60f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 70f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 80f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(80)
    Map('kind -> 'evt, 'time -> 90f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 100f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(100)
    Map('kind -> 'evt, 'time -> 110f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 120f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 130f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 140f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 150f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 160f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f))

  val trace3 = List(
    Map('kind -> 'evt, 'time -> 0f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 20f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 30f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 40f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(40)
    Map('kind -> 'evt, 'time -> 80f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(80)
    Map('kind -> 'evt, 'time -> 100f, 'yaw -> P, 'lws -> -10f, 'rws -> 10f), // P(100)
    Map('kind -> 'evt, 'time -> 110f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 120f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 130f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 140f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 150f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 160f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f))

  val trace_1 = List(
    Map('kind -> 'evt, 'time -> 0f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 20f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 30f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 40f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(40)
    Map('kind -> 'evt, 'time -> 50f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f), // not Q(50)
    Map('kind -> 'evt, 'time -> 60f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 70f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 80f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(80) -- too late
    Map('kind -> 'evt, 'time -> 90f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 100f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(100)
    Map('kind -> 'evt, 'time -> 110f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 120f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 130f, 'yaw -> P, 'lws -> -10f, 'rws -> 10f), // P(130)
    Map('kind -> 'evt, 'time -> 140f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 150f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 160f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f))

  val trace_2 = List(
    Map('kind -> 'evt, 'time -> 0f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 20f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(20)
    Map('kind -> 'evt, 'time -> 30f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 40f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f), // not Q(40)
    Map('kind -> 'evt, 'time -> 50f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(50)
    Map('kind -> 'evt, 'time -> 60f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 70f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 80f, 'yaw -> P, 'lws -> -10f, 'rws -> 10f), // P(80) but not Q
    Map('kind -> 'evt, 'time -> 90f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 100f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(100)
    Map('kind -> 'evt, 'time -> 110f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 120f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 130f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 140f, 'yaw -> 10f, 'lws -> -10f, 'rws -> Q),
    Map('kind -> 'evt, 'time -> 150f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 160f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f))

  val trace_3 = List(
    Map('kind -> 'evt, 'time -> 0f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 20f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 30f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 40f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(40)
    Map('kind -> 'evt, 'time -> 80f, 'yaw -> P, 'lws -> -10f, 'rws -> Q), // P(80)
    Map('kind -> 'evt, 'time -> 100f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 110f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 120f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 130f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 140f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f),
    Map('kind -> 'evt, 'time -> 150f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f), // P(150) too late
    Map('kind -> 'evt, 'time -> 160f, 'yaw -> 10f, 'lws -> -10f, 'rws -> 10f))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_3 foreach m.addMapEvent
    m.terminate()
  }
}

    