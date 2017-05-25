package examples.crv15.breach.benchmark2

import rete._
import math._

// Exact same spec as benchmark3

/*
 * 
 * Events:
 * 
 * - evt(time,cliff_l,cliff_fl,cliff_fr,cliff_r,lws,rws)
 *  
 * phi := 
 *   ev_[0, 50] 
 *     (
 *       ((lws[t] > 10) and (abs(rws[t]-lws[t]) < 1)) 
 *       until 
 *       (ev_[0, 50] 
 *         (
 *           ((cliff_l[t] > .5) or 
 *           (cliff_r[t] > .5)) or 
 *           ((cliff_fl[t] > .5) or 
 *           (cliff_fr[t] > .5))
 *         )
 *       )
 *     )
 *     
 * This property can be reformulated to:
 *  
 * phi := ev_[0, 50] (p until (ev_[0, 50] q))
 *     
 * which states the following property:
 * 
 * "within 50 time units we reach a state where p is continuously true
 *  until a point is reached where there is max 50 time units until q
 *  becomes true".
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

// Map('kind -> 'evt, 'time -> 1f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map();
    map += 'kind -> 'evt
    map += 'time -> record.get("time").toFloat
    map += 'lws -> record.get("lws").toFloat
    map += 'rws -> record.get("rws").toFloat
    map += 'cliff_l -> record.get("cliff_l").toFloat
    map += 'cliff_fl -> record.get("cliff_fl").toFloat
    map += 'cliff_fr -> record.get("cliff_fr").toFloat
    map += 'cliff_r -> record.get("cliff_r").toFloat
    addMapEvent(map)
  }
}

class M extends CSVMonitor {
  val evt = event
  val P, Q = fact

  // Predicates:

  def p(lws: Float, rws: Float): Boolean =
    lws > 10 && abs(rws - lws) < 1

  def q(cliff_l: Float, cliff_fl: Float, cliff_fr: Float, cliff_r: Float): Boolean =
    cliff_l > 0.5 || cliff_r > 0.5 || cliff_fl > 0.5 || cliff_fr > 0.5

  //  The P(time,trueSoFar) predicate:

  "r1" -- evt('time -> 't, 'lws -> 'l, 'rws -> 'r) & not(P('_, '_)) |-> {
    if ('t.float <= 50 & p('l.float, 'r.float))
      insert(P('t, true))
  }

  "r2" -- evt('time -> 't, 'lws -> 'l, 'rws -> 'r) & P('_, 'trueSoFar) |-> {
    if (p('l.float, 'r.float) && ('t.float <= 50 || 'trueSoFar)) {
      update(P('t, true))
    }
  }

  "r3" -- evt('time -> 't, 'lws -> 'l, 'rws -> 'r) & P('tp, true) |-> {
    if (!p('l.float, 'r.float))
      update(P('tp, false))
  }

  //  The Q(time) predicate:

  "r4" -- evt('time -> 't, 'cliff_l -> 'c_l, 'cliff_fl -> 'c_fl, 'cliff_fr -> 'c_fr, 'cliff_r -> 'c_r) & not(Q('_)) |-> {
    if (q('c_l.float, 'c_fl.float, 'c_fr.float, 'c_r.float))
      insert(Q('t))
  }

  "r5" -- Q('tq) & not(P('_, '_)) |->
    ensure("q without a p can only occur within 100 tu")('tq.float <= 100)

  "r6" -- Q('tq) & P('tp, '_) |->
    ensure("q must be true no later than 50 tu since p was last true")('tq.float <= 100 || 'tq.float - 'tp.float <= 50)

  // End is reached:  

  "r7" -- END() & not(Q('_)) |-> fail()
}

object Evaluate extends MonitorFeeder {

  // p : lws > 10 && abs(rws - lws) < 1
  // q :  cliff_l > 0.5 || cliff_r > 0.5 || cliff_fl > 0.5 || cliff_fr > 0.5  

  val trace1 = List(
    Map('kind -> 'evt, 'time -> 1f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 3f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 10f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 11f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 12f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 13f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 20f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 30f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(30)
    Map('kind -> 'evt, 'time -> 35f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(35)
    Map('kind -> 'evt, 'time -> 45f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(45)
    Map('kind -> 'evt, 'time -> 50f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 51f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 55f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 60f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(60)
    Map('kind -> 'evt, 'time -> 65f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(65)
    Map('kind -> 'evt, 'time -> 70f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 80f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.6f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // q(80)
    Map('kind -> 'evt, 'time -> 85f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f))

  val trace2 = List(
    Map('kind -> 'evt, 'time -> 1f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 3f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 10f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 11f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 12f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 13f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 20f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 30f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(30)
    Map('kind -> 'evt, 'time -> 35f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(35)
    Map('kind -> 'evt, 'time -> 45f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(45)
    Map('kind -> 'evt, 'time -> 60f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(60)
    Map('kind -> 'evt, 'time -> 300f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(300)
    Map('kind -> 'evt, 'time -> 340f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 350f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.6f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // q(350)
    Map('kind -> 'evt, 'time -> 400f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f))

  val trace3 = List(
    Map('kind -> 'evt, 'time -> 1f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 3f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 10f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 11f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 12f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 13f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 20f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 30f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 35f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 45f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 50f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 51f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 55f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 60f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 65f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 70f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 100f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.6f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // q(100)
    Map('kind -> 'evt, 'time -> 110f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f))

  val trace_1 = List(
    Map('kind -> 'evt, 'time -> 1f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 3f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 10f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 11f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 12f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 13f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 20f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 30f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(30)
    Map('kind -> 'evt, 'time -> 35f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(35)
    Map('kind -> 'evt, 'time -> 45f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(45)
    Map('kind -> 'evt, 'time -> 50f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 51f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 55f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 60f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(60)
    Map('kind -> 'evt, 'time -> 65f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(65)
    Map('kind -> 'evt, 'time -> 70f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 80f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 85f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f)) // no Q

  val trace_2 = List(
    Map('kind -> 'evt, 'time -> 1f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 3f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 10f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 11f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 12f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 13f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 20f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 30f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(30)
    Map('kind -> 'evt, 'time -> 35f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(35)
    Map('kind -> 'evt, 'time -> 45f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(45)
    Map('kind -> 'evt, 'time -> 60f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(60)
    Map('kind -> 'evt, 'time -> 300f, 'lws -> 11f, 'rws -> 11.5f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // p(300)
    Map('kind -> 'evt, 'time -> 340f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 360f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.6f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // q(360)
    Map('kind -> 'evt, 'time -> 400f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f))

  val trace_3 = List(
    Map('kind -> 'evt, 'time -> 1f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 3f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 10f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 11f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 12f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 13f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 20f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 30f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 35f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 45f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 50f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 51f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 55f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 60f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 65f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 70f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f),
    Map('kind -> 'evt, 'time -> 101f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.6f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f), // q(101)
    Map('kind -> 'evt, 'time -> 102f, 'lws -> 10f, 'rws -> 20f, 'cliff_l -> 0.1f, 'cliff_fl -> 0.1f, 'cliff_fr -> 0.1f, 'cliff_r -> 0.1f))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_3 foreach m.addMapEvent
    m.terminate()
  }
}

  