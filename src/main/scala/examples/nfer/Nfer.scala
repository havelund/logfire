package examples.nfer

import rete._
import scala.language.dynamics

/**
 * Created by seanmk on 11/7/16.
 */
trait Nfer extends Monitor {
  type Clock = Int
  type Op = (Clock,Clock,Clock,Clock) => Option[(Clock,Clock)]
  type M = Int
  type Phi = (M,M) => Option[M]

  val BEFORE:Op = (s1:Clock, e1:Clock, s2:Clock, e2:Clock) =>
    if (e1 < s2) Some((s1,e2)) else None

  val MEET:Op = (s1:Clock, e1:Clock, s2:Clock, e2:Clock) =>
    if (e1 == s2) Some((s1,e2)) else None

  val DURING:Op = (s1:Clock, e1:Clock, s2:Clock, e2:Clock) =>
    if (s1 >= s2 && e1 <= e2) Some(s2,e2) else None

  val COINCIDE:Op = (s1:Clock, e1:Clock, s2:Clock, e2:Clock) =>
    if (s1 == s2 && e1 == e2) Some(s1,e1) else None

  val START:Op = (s1:Clock, e1:Clock, s2:Clock, e2:Clock) =>
    if (s1 == s2) Some(s1, Math.max(e1,e2)) else None

  val FINISH:Op = (s1:Clock, e1:Clock, s2:Clock, e2:Clock) =>
    if (e1 == e2) Some(Math.min(s1,s2), e1) else None

  val OVERLAP:Op = (s1:Clock, e1:Clock, s2:Clock, e2:Clock) =>
    if (s1 < e2 && s2 < e1) Some(Math.min(s1,s2), Math.max(e1,e2)) else None

  val SLICE:Op = (s1:Clock, e1:Clock, s2:Clock, e2:Clock) =>
    if (s1 < e2 && s2 < e1) Some(Math.max(s1,s2), Math.min(e1,e2)) else None

  private def evaluate(eta:Symbol, eta1:Symbol, eta2:Symbol, op:Op, phi:Phi, within:Option[Clock] = None):Unit = {
    val result = eventToPool(eta)
    // insert the rule to generate an eta
    newRuleId() -- eventToPool(eta1)('s1,'e1) & eventToPool(eta2)('s2,'e2) |-> {
      for ((s, e) <- op('s1.i, 'e1.i, 's2.i, 'e2.i)) {
        for (m <- phi(0, 0)) {
          within match {
            case Some(limit) => if (e - s <= limit) insert(result(s, e))
            case None => insert(result(s, e))
          }
        }
      }
    }
  }

  var knownEvents = Set.empty[Symbol]

  private def eventToPool(event:Symbol):Symbol = {
    val result = Symbol(s"__nfer__${event.toString().substring(1)}")
    // add a rule to make sure we get it in fact form with two timestamps
    // use the knownEvents set to make sure this rule is only added once
    synchronized {
      if (!knownEvents.contains(event)) {
        knownEvents = knownEvents + event
        newRuleId() -- event('t) |-> result('t, 't)
      }
    }
    result
  }

  implicit def toNferRule(eta:Symbol) = new {
    def :-(eta1:Symbol) = new {
      def before(eta2:Symbol) = new NferRule(eta, eta1, eta2, BEFORE)
      def meet(eta2:Symbol) = new NferRule(eta, eta1, eta2, MEET)
      def during(eta2:Symbol) = new NferRule(eta, eta1, eta2, DURING)
      def coincide(eta2:Symbol) = new NferRule(eta, eta1, eta2, COINCIDE)
      def start(eta2:Symbol) = new NferRule(eta, eta1, eta2, START)
      def finish(eta2:Symbol) = new NferRule(eta, eta1, eta2, FINISH)
      def overlap(eta2:Symbol) = new NferRule(eta, eta1, eta2, OVERLAP)
      def slice(eta2:Symbol) = new NferRule(eta, eta1, eta2, SLICE)
    }
  }

  class NferRule(val eta:Symbol, val eta1:Symbol, val eta2:Symbol, val op:Op) {
    val defaultPhi:Phi = (m1:M,m2:M) => Some(0)
    var limit:Option[Clock] = None

    def map(phi:Phi) = evaluate(eta, eta1, eta2, op, phi, limit)
    def within(limit:Clock):NferRule = {
      this.limit = Some(limit)
      this
    }
    def nomap() = evaluate(eta, eta1, eta2, op, defaultPhi, limit)
  }

  private def newRuleId():String = {
    synchronized {
      Nfer.currentId += 1
      s"nferRule${Nfer.currentId}"
    }
  }
}

object Nfer {
  var currentId = 0
}