package rete

import Util._

private[rete] object PP {
  def quote(x: Any): Any =
    if (x.isInstanceOf[String]) "\"" + x + "\"" else x

  def bindingToString(binding: Map[Symbol, Any]): String = {
    val positions = positionalArguments
    var result = ""
    val domain = binding.keySet.filterNot(_ == 'kind)
    var sep = ""
    if (domain subsetOf positions.values.toSet) {
      //--\
      if (binding contains 'kind)
        result += binding('kind) + "("
      else
        result += 'ANY + "("
      //--
      //result += binding('kind) + "("
      //--/
      for (i <- 1 to domain.size) {
        result += sep + quote(binding(positions(i)))
        sep = ","
      }
      result += ")"
    } else {
      if (binding contains 'kind)
        result += binding('kind) + "("
      else
        result += "fact("
      for (field <- domain) {
        result += sep + field + "->" + quote(binding(field))
        sep = ","
      }
      result += ")"
    }
    result
  }
}

private[rete] trait Pattern

private[rete] case class Variable(s: Symbol) extends Pattern {
  override def toString: String = s.toString()
}

private[rete] case class Constant(s: Any) extends Pattern {
  override def toString: String = s.toString
}

private[rete] trait Condition {
  private[rete] def getEffectiveBinding: Map[Symbol, Symbol]
}

private[rete] trait BasicCondition extends Condition {
  private[rete] val constraints: Map[Symbol, Pattern]
}

case class PC(
               private[rete] val constraints: Map[Symbol, Pattern],
               private[rete] val label: Symbol = null) extends BasicCondition {
  private[rete] def getEffectiveBinding: Map[Symbol, Symbol] =
    for ((field, Variable(v)) <- constraints) yield (field -> v)

  /**
   * Returns the variables occurring in this PC.
   *
   * @return the variables occurring in this PC.
   */
  def getVariables: List[Symbol] =
    for ((_, Variable(v)) <- constraints.toList if !v.toString.startsWith("'_")) yield v

  override def toString: String = PP.bindingToString(constraints)
}

//private[rete] case class PC(constraints: Map[Symbol, Pattern], label: Symbol = null) extends BasicCondition {
//  def getEffectiveBinding: Map[Symbol, Symbol] =
//    for ((field, Variable(v)) <- constraints) yield (field -> v)
//
//  override def toString: String =
//    "exists" + constraints.mkString("(", ",", ")")
//}

private[rete] case class NC(constraints: Map[Symbol, Pattern]) extends BasicCondition {
  def getEffectiveBinding: Map[Symbol, Symbol] = Map()

  override def toString: String =
    "not(" + PP.bindingToString(constraints) + ")"
}

private[rete] case class NCC(conditions: List[Condition]) extends Condition {
  def getEffectiveBinding: Map[Symbol, Symbol] = Map()

  override def toString: String =
    "not(" + conditions.mkString(",") + ")"
}

private[rete] trait Action {
  def getFreeVariables: Set[Symbol]
}

private[rete] case class Print(string: String) extends Action {
  def getFreeVariables: Set[Symbol] = Set()

  override def toString: String =
    "print(\"" + string + "\")"
}

private[rete] case class Code(code: Unit => Any) extends Action {
  def getFreeVariables: Set[Symbol] = Set()

  override def toString: String =
    "{...}"
}

private[rete] case class Add(constraints: Map[Symbol, Pattern]) extends Action {
  def getFreeVariables: Set[Symbol] =
    for ((_, Variable(v)) <- constraints.toSet) yield v

  override def toString: String =
    PP.bindingToString(constraints)
}

private[rete] case class Del(constraints: Map[Symbol, Pattern]) extends Action {
  def getFreeVariables: Set[Symbol] =
    for ((_, Variable(v)) <- constraints.toSet) yield v

  override def toString: String =
    "del(" + PP.bindingToString(constraints) + ")"
}

private[rete] case class Rem(label: Symbol) extends Action {
  def getFreeVariables: Set[Symbol] = Set()

  override def toString: String =
    "rem(" + label + ")"
}

private[rete] case class And(action1: Action, action2: Action) extends Action {
  def getFreeVariables: Set[Symbol] =
    action1.getFreeVariables union action2.getFreeVariables

  override def toString: String =
    action1.toString + " and " + action2.toString
}

private[rete] case class Production(name: String, conditions: List[Condition], action: Action) {
  assert(conditions.length > 0)

  override def toString: String =
    "\"" + name + "\"" + " -- " + conditions.mkString("", " & ", "") + " |-> " + action
}
