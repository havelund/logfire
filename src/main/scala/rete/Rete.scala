package rete

import graphviz._

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.language.reflectiveCalls

import collection.mutable.{ Map => MutMap, Set => MutSet }
import collection.mutable
//import org.scalatest.events.TestStarting

// =============
// === Util: ===
// =============

private[rete] object Util {
  type FieldToTest = Symbol
  type Value = Any
  type Binding = Map[FieldToTest, Value]

  val positionalArguments = Map(
    1 -> 'one, 2 -> 'two, 3 -> 'three, 4 -> 'four,
    5 -> 'five, 6 -> 'six, 7 -> 'seven, 8 -> 'eight,
    9 -> 'nine, 10 -> 'ten, 11 -> 'elleven, 12 -> 'twelve,
    13 -> 'thirteen, 14 -> 'fourteen, 15 -> 'fifteen, 16 -> 'sixteen,
    17 -> 'seventeen, 18 -> 'eighteen, 19 -> 'nineteen, 20 -> 'twenty)

  val positionalArguments_DONOTUSE = Map(
    1 -> '_1, 2 -> '_2, 3 -> '_3, 4 -> '_4,
    5 -> '_5, 6 -> '_6, 7 -> '_7, 8 -> '_8,
    9 -> '_9, 10 -> '_10, 11 -> '_11, 12 -> '_12,
    13 -> '_13, 14 -> '_14, 15 -> '_15, 16 -> '_16,
    17 -> '_17, 18 -> '_18, 19 -> '_19, 20 -> '_20)

  implicit def convColl[T](c: Iterable[T]) = new {
    def somematch(param_stmt: PartialFunction[T, Unit]) = new {
      def otherwise(stmt: => Unit) {
        (c find (param_stmt.isDefinedAt(_))) match {
          case Some(v) => param_stmt(v)
          case None    => stmt
        }
      }
    }
  }

  implicit def reverseMap[A, B](m: Map[A, B]) = new {
    def reverse: Map[B, A] = m map {
      case (x, y) => (y, x)
    }

    def valueSet: Set[B] = m.values.toSet

    def filterValues(p: B => Boolean): Map[A, B] =
      m filter {
        case (_, y) => p(y)
      }
  }

  case class ListOps[T](list: List[T]) {
    def -(x: T) = list filterNot (_ == x)

    def remove(p: T => Boolean) = list.filterNot(p)
  }

  implicit def liftListToListOps[T](list: List[T]) =
    ListOps(list)

  val dollar: Char = "$".head

  implicit def moreListOps(str: String) = new {
    def stripFromDollar: String = str.takeWhile(_ != dollar)
  }
}

import Util._

private[rete] object Debug {
  val DEBUG = false
  val SHOW_ACTIONS = true

  def DEBUG(obj: Any) {
    if (DEBUG)
      println("> " + (if (obj == null) "null" else obj.toString))
  }

  def DEBUG(string: String, obj: Any) {
    if (DEBUG)
      println("[" + string + "]>\n" + (if (obj == null) "null" else obj.toString))
  }

  def SHOW_ACTION(action: Action) {
    if (SHOW_ACTIONS)
      println("[action] " + action)
  }

  def TEST(v: Any) {
    println("--- " + v)
  }

  def TEST(msg: String, v: Any) {
    println("--- " + msg + ":" + v)
  }
}

import Debug._

//import Debug._

private[rete] object PointerBank {
  var last = 0

  var pointers: List[(AnyRef, Int)] = Nil

  trait Output

  case class Old(x: Int) extends Output

  case class New(x: Int) extends Output

  def getOutput(value: AnyRef): Output = {
    val pair = pointers find {
      case (v, i) => v eq value
    }
    pair match {
      case None =>
        last += 1
        pointers = (value, last) :: pointers
        New(last)
      case Some((v, i)) => Old(i)
    }
  }

  def initialize() {
    last = 0
    pointers = Nil
  }

}

import PointerBank._

private[rete] object Printing {
  val nl = "\n"
  val tab = " "
  var indentCount: Int = 0

  def indent: String = {
    var result = ""
    for (i <- 1 to indentCount) {
      result += "|" + tab
    }
    result
  }

  type Field = (String, Any)

  def show(which: AnyRef, what: String, data: Field*): String = {
    var result: String = ""

    def addName(what: String) {
      result += what + ":" + nl
      result += indent + ("-" * (what.length + 1)) + nl
    }

    def addEntry(entry: Field) {
      val (fieldName, fieldValue) = entry
      result += indent + fieldName + ":" + nl
      indentCount += 1
      val str =
        fieldValue match {
          case null => indent + "null"
          case it: Iterable[_] =>
            val kind =
              it match {
                case _: List[_]      => "List"
                case _: Set[_]       => "Set"
                case _: Map[_, _]    => "Map"
                case _: MutMap[_, _] => "MutMap"
                case _: MutSet[_]    => "MutSet"
              }
            if (it.isEmpty) indent + kind + "()"
            else {
              var result1: String = indent + kind + "("
              indentCount += 1
              for (e <- it) result1 += nl + indent + e.toString
              indentCount -= 1
              result1 += nl + indent + ")"
              result1
            }
          case Some(x) => indent + x.toString
          case _       => indent + fieldValue.toString
        }
      indentCount -= 1
      result += str
    }

    getOutput(which) match {
      case Old(pointer) =>
        which.getClass.getSimpleName + "@" + pointer.toString
      case New(pointer) =>
        addName(what + "@" + pointer)
        indentCount += 1
        for (entry <- data take (data.length - 1)) {
          addEntry(entry)
          result += nl
        }
        addEntry(data last)
        indentCount -= 1
        result
    }
  }
}

import Printing._

// -----------------------
// --- Generator util: ---
// -----------------------

// from http://notes.langdale.com.au/Generators_in_Scala.html
// from https://gist.github.com/574873

//import scala.util.continuations._
//
//class Generator[A] extends Iterator[A] with (A => Unit @suspendable) {
//  private var a: Option[A] = None
//  private var k: Option[Unit => Unit] = None
//
//  def next = {
//    val a0 = a.get
//    val k0 = k.get
//    a = None
//    k = None
//    k0()
//    a0
//  }
//
//  def hasNext = k.isDefined
//
//  def apply(a0: A): Unit @suspendable = {
//    a = Some(a0)
//    shift { k0: (Unit => Unit) => k = Some(k0) }
//  }
//}
//
//object Generator {
//  def generator[A](f: (A => Unit @suspendable) => Unit @suspendable): Iterator[A] = {
//    val g = new Generator[A]
//    reset { f(g) }
//    g
//  }
//
//  trait SuspendableForeach[A] { def foreach(f: A => Unit @suspendable): Unit @suspendable }
//
//  def suspendable[A](ible: Iterable[A]) = new SuspendableForeach[A] {
//    def foreach(f: A => Unit @suspendable): Unit @suspendable = {
//      val i = ible.iterator
//      while (i.hasNext) f(i.next)
//    }
//  }
//}
//import Generator._

// ============
// === Wme: ===
// ============

/**
 * This class represents a fact that has been added to the fact memory with
 * the <code>addFact</code> method or one of the <code>addMapFact</code> methods.
 * Each of these methods return the fact they add. The main purpose of holding
 * on to a fact is to later be able to remove it with the <code>remove()</code>
 * method.
 */

trait Fact {
  /**
   * The mapping from field names (symbols) to values that the fact represents.
   *
   * @return the mapping from fields to values.
   */
  def fields: Map[Symbol, Any]

  /**
   * Removes the fact from the fact memory.
   */
  def remove()
}

private[rete] case class Wme(fields: Binding) extends Indexable[FieldToTest] with Visual with Fact {
  var alphaMemories: List[AlphaMemory] = List()
  // the ones containing this WME
  var tokens: List[Token] = List()
  // the ones with wme = this WME
  var negativeJoinResults: List[NegativeJoinResult] = Nil
  // used for removal
  var history: History = null

  def le(other: Wme): Boolean = {
    val symbol1 = fields.getOrElse('kind, 'ANY)
    val symbol2 = other.fields.getOrElse('kind, 'ANY)
    symbol1.toString <= symbol2.toString
  }

  def apply(field: FieldToTest): Value = fields(field)

  def lookup(field: FieldToTest): Option[Value] =
    fields.get(field)

  def matches(field: FieldToTest, value: Value): Boolean = // not in paper. Makes test simpler
    (fields contains field) && (fields(field) == value)

  def remove() {
    for (am <- alphaMemories) {
      am.items.rem(this)
      if (am.items.isEmpty) {
        for (node <- am.successors) {
          if (node.isInstanceOf[JoinNode]) {
            // don't left-unlink negative nodes
            val jnode = node.asInstanceOf[JoinNode]
            jnode.parent.children = jnode.parent.children - jnode
            jnode.leftUnlinked = true
          }
        }
      }
    }
    tokens foreach (_.remove())
    tokens = Nil // this is a while loop on page 30
    for (jr <- negativeJoinResults) {
      jr.owner.joinResults = jr.owner.joinResults - jr
      if (jr.owner.joinResults == Nil) {
        for (child <- jr.owner.node.children)
          child.asInstanceOf[JoinerTarget].activationLeft(jr.owner, null)
      }
    }
  }

  def toGraph(config: Config) {
    config.colorNode(this, "orange")
    config.addFields(this, fields.toList)
    //config.addEdges(this, 'alphaMemories, alphaMemories)
    //config.addEdges(this, 'tokens, tokens)
    config.addEdges(this, 'negativeJoinResults, negativeJoinResults)
  }

  override def toString: String =
    show(this, "Wme",
      "fields" -> fields,
      "alphaMemories" -> alphaMemories,
      "tokens" -> tokens,
      "negativeJoinResults" -> negativeJoinResults)

  def toStringSimple: String = PP.bindingToString(fields)
}

//object Wme {
//  def apply(v1: Value, v2: Value, v3: Value): Wme =
//    new Wme(Map('identifier -> v1, 'attribute -> v2, 'value -> v3))
//}

// ==============
// === Token: ===
// ==============

private[rete] case class Token(parent: Token, wme: Wme) extends Indexable[(Int, FieldToTest)] with Visual {
  var node: JoinerTarget = null
  // points to the memory this token is in
  var children: List[Token] = Nil
  // ones with parent=this token
  // The next 3 fields should perhaps be part of a subclass to save memory
  var joinResults: List[NegativeJoinResult] = Nil
  // used only on tokens in negative nodes
  var nccResults: List[Token] = Nil
  // similar to join-results but for NCC nodes
  var owner: Token = null // initialized to null to make type check 
  // on tokens in NCC partners: token in whose local memory this result resides

  def remove() {
    removeDescendents()
    if (!(node.isInstanceOf[NccPartnerNode]))
      node.items.rem(this)
    if (wme != null) // we need this check for negative conditions
      wme.tokens = wme.tokens - this
    parent.children = parent.children - this
    if (node.isInstanceOf[BetaMemory]) {
      if (node.items.isEmpty) {
        for (child <- node.children) {
          // right unlink it
          val joinNode = child.asInstanceOf[JoinNode]
          joinNode.alphaMemory.successors = joinNode.alphaMemory.successors - joinNode
          joinNode.rightUnlinked = true
        }
      }
    }
    if (node.isInstanceOf[NegativeNode]) {
      val negNode = node.asInstanceOf[NegativeNode]
      if (negNode.items.isEmpty) {
        // right unlink it
        negNode.alphaMemory.successors = negNode.alphaMemory.successors - negNode
        negNode.rightUnlinked = true
      }
      for (jr <- joinResults) {
        jr.wme.negativeJoinResults = jr.wme.negativeJoinResults - jr
      }
      joinResults = Nil
    }
    if (node.isInstanceOf[NccNode]) {
      for (result_tok <- nccResults) {
        result_tok.wme.tokens = result_tok.wme.tokens - result_tok
        result_tok.parent.children = result_tok.parent.children - result_tok
      }
      nccResults = Nil
    }
    if (node.isInstanceOf[NccPartnerNode]) {
      owner.nccResults = owner.nccResults - this
      if (owner.nccResults == Nil) {
        for (child <- node.asInstanceOf[NccPartnerNode].nccNode.children) {
          child.asInstanceOf[JoinerTarget].activationLeft(owner, null)
        }
      }
    }
  }

  def removeDescendents() {
    children foreach (_.remove())
    children = Nil // a while loop on page 31
  }

  def lookup(field: (Int, FieldToTest)): Option[Value] =
    getIthWme(field._1).lookup(field._2)

  def getIthWme(nr: Int): Wme = {
    var token_ : Token = this
    for (i <- 0 until nr) token_ = token_.parent
    token_.wme
  }

  def getIthToken(nr: Int): Token = {
    var token_ : Token = this
    for (i <- 0 until nr) token_ = token_.parent
    token_
  }

  def toGraph(config: Config) {
    config.colorNode(this, "brown")
    config.addEdge(this, 'wme, wme)
    config.addEdges(this, 'children, children)
    config.addEdges(this, 'joinResults, joinResults)
    config.addEdges(this, 'nccResults, nccResults)
  }

  override def toString: String =
    show(this, "Token",
      "parent" -> parent,
      "wme" -> wme,
      "node" -> node,
      "children" -> children,
      "joinResults" -> joinResults,
      "nccResults" -> nccResults,
      "owner" -> owner)
}

private[rete] case class NegativeJoinResult(
  owner: Token, // the token in whose local memory this result resides
  wme: Wme // the WME that matches owner
  ) extends Visual {

  def toGraph(config: Config) {
    config.colorNode(this, "goldenrod")
    config.addEdge(this, 'owner, owner)
    config.addEdge(this, 'wme, wme)
  }

  override def toString: String =
    show(this, "NegativeJoinResult",
      "owner" -> owner,
      "wme" -> wme)
}

// =====================
// ===  AlphaMemory: ===
// =====================

private trait Indexable[Index] extends Visual {
  def lookup(index: Index): Option[Value]

  def extractValues(indexes: List[Index]): Option[List[Value]] = {
    var values: List[Value] = Nil
    for (index <- indexes) {
      lookup(index) match {
        case None => return None
        case Some(value) =>
          values = values :+ value
      }
    }
    Some(values)
  }
}

private class DoubleIndex[FieldIndex, IndexedElement <: Indexable[FieldIndex]] extends Visual {
  private var index: MutMap[List[FieldIndex], MutMap[List[Value], MutSet[IndexedElement]]] = MutMap()
  private var elementSetCounter = 0
  private var wasEmptyBeforePut: Boolean = true
  private val EMPTY_ELEMENT_SET = MutSet[IndexedElement]()

  def addFields(indexes: List[FieldIndex]) {
    index.get(indexes) match {
      case None => // has not yet been initialized, due to dummy Token
        index += (indexes -> MutMap())
      case Some(valuemap) => // has already been initialized
    }
  }

  def put(element: IndexedElement) {
    wasEmptyBeforePut = (elementSetCounter == 0)
    for (fieldIndexes <- index.keySet) {
      element.extractValues(fieldIndexes) match {
        case None =>
        case Some(values) =>
          val valueMap = index(fieldIndexes)
          val elementSet = valueMap.get(values) match {
            case None =>
              elementSetCounter += 1
              val newElementSet = MutSet[IndexedElement]()
              valueMap.put(values, newElementSet)
              newElementSet
            case Some(oldElementSet) =>
              oldElementSet
          }
          elementSet.add(element)
      }
    }
  }

  def rem(element: IndexedElement) {
    for (fieldIndexes <- index.keySet) {
      element.extractValues(fieldIndexes) match {
        case None =>
        case Some(values) =>
          val valueMap = index(fieldIndexes)
          valueMap.get(values) match {
            case None =>
            case Some(elementSet) =>
              elementSet.remove(element)
              if (elementSet.isEmpty) {
                valueMap.remove(values)
                elementSetCounter -= 1
              }
          }
      }
    }
  }

  def get(fieldIndexes: List[FieldIndex], values: List[Value]): MutSet[IndexedElement] =
    index(fieldIndexes).get(values) match {
      case None             => EMPTY_ELEMENT_SET // never updated, used for iteration
      case Some(elementSet) => elementSet
    }

  def isEmpty: Boolean = elementSetCounter == 0

  def wasEmpty: Boolean = wasEmptyBeforePut

  def getAllElements: Set[IndexedElement] = {
    var result: Set[IndexedElement] = Set()
    for (
      (fieldIndexes, valueMap) <- index;
      (values, elementSet) <- valueMap;
      element <- elementSet
    ) {
      result += element
    }
    result
  }

  def toGraph(config: Config) {
    config.colorNode(this, "green")
    config.addFields(this, 'elementSetCounter -> elementSetCounter)
    config.addFields(this, 'indexKeySet -> index.keySet.mkString("[", ",", "]"))
    for ((fieldIndexes, valueMap) <- index; (values, elementSet) <- valueMap; element <- elementSet) {
      val doubleIndex = fieldIndexes.zip(values).mkString("[", ",", "]")
      config.addEdge(this, element, "label" -> ("\"" + doubleIndex + "\""))
    }
  }

  override def toString: String =
    show(this, "DoubleIndex",
      "elementSetCounter" -> elementSetCounter,
      "index" -> index)
}

private[rete] class AlphaMemory extends Visual {
  val items = new DoubleIndex[FieldToTest, Wme]()
  var successors: List[Joiner] = List()
  var referenceCount: Int = 0

  def activation(w: Wme) {
    items.put(w)
    w.alphaMemories = this :: w.alphaMemories // for tree-based removal
    for (joinNode <- successors) joinNode.activationRight(w)
  }

  def relink(joiner: Joiner) {
    // Follow links up from node, find first ancestor that's linked.
    var ancestor: Joiner = joiner.nearestAncestorWithSameAmem
    while (ancestor != null && ancestor.rightUnlinked) {
      ancestor = ancestor.nearestAncestorWithSameAmem
    }
    // Now splice in the node in the right place
    if (ancestor != null) {
      // insert joiner into the list successors immediately before ancestor
      val index = successors.indexOf(ancestor)
      val (left, right) = successors.splitAt(index)
      successors = left ++ List(joiner) ++ right
      // The above procedure is of course slow.
    } else {
      successors = successors :+ joiner
    }
    joiner.rightUnlinked = false
  }

  def toGraph(config: Config) {
    config.colorNode(this, "blue")
    config.addEdge(this, 'items, items)
    config.addEdges(this, 'successors, successors)
  }

  override def toString: String =
    show(this, "AlphaMemory",
      "items" -> items,
      "successors" -> successors,
      "referenceCount" -> referenceCount)
}

/*
  To avoid duplicate tokens we make sure that the alpha memory's list of join
  nodes is appropriately ordered: if J1 and J2 are on the list and 
  J1 is a descendent of J2, thenDoActions J1 must come earlier in the list than J2.
 */

// =====================
// === AlphaNetwork: ===
// =====================

private[rete] class AlphaNetwork extends Visual {
  // Section 2.2.3 Exhaustive Hash Table Lookup modified to arbitrary number of fields.

  def build_or_share_alpha_memory(condition: BasicCondition): AlphaMemory = {
    val binding: Binding = for ((symbol, Constant(const)) <- condition.constraints) yield (symbol -> const)
    table.get(binding) match {
      case None =>
        bindingDomains += binding.keySet
        val newAlphaMemory = new AlphaMemory
        table += (binding -> newAlphaMemory)
        for ((oldBinding, oldAlphaMemory) <- table) {
          val commonDomain = binding.keySet.intersect(oldBinding.keySet)
          if (commonDomain forall (id => binding(id) == oldBinding(id))) {
            val newDomain = binding.keySet -- commonDomain
            for (wme <- oldAlphaMemory.items.getAllElements) {
              if (newDomain forall (id => wme.matches(id, binding(id)))) {
                newAlphaMemory.activation(wme)
              }
            }
          }
        }
        newAlphaMemory
      case Some(oldAlphaMemory) =>
        oldAlphaMemory
    }
  }

  def delete_alpha_memory(am: AlphaMemory) {
    table filterNot {
      case (_, alphaMemory) => am == alphaMemory
    }
  }

  def activation(w: Wme) {
    val binding = w.fields
    for (domain <- bindingDomains) {
      projectOnDomain(binding, domain) match {
        case None =>
        case Some(subBinding) =>
          table.get(subBinding) match {
            case None =>
            case Some(alphaMemory) =>
              alphaMemory.activation(w)
          }
      }
    }
  }

  // --- private: ---

  private type BindingDomain = Set[FieldToTest]

  private var table: Map[Binding, AlphaMemory] = Map()
  private var bindingDomains: Set[BindingDomain] = Set()

  private def projectOnDomain(bindingOfWme: Binding, domain: BindingDomain): Option[Binding] = {
    var binding: Binding = Map()
    for (key <- domain) {
      bindingOfWme.get(key) match {
        case None        => return None
        case Some(value) => binding += (key -> value)
      }
    }
    Some(binding)
  }

  def getAllWmes: Set[Wme] = {
    var result: Set[Wme] = Set()
    for ((index, alphaMemory) <- table; wme <- alphaMemory.items.getAllElements) {
      result += wme
    }
    result
  }

  def toGraph(config: Config) {
    config.colorNode(this, "blue")
    config.addFields(this, 'bindingDomains -> bindingDomains.toString)
    for ((binding, alphaMemory) <- table) {
      config.addEdge(this, 'table, alphaMemory,
        "label" -> ("\"" + binding.mkString("[", ",", "]") + "\""),
        "color" -> "blue")
    }
  }

  override def toString: String =
    show(this, "AlhpaNetwork",
      "bindingDomains" -> bindingDomains,
      "table" -> table)

}

// =====================
// === JoinerTarget: ===
// =====================

private[rete] trait ReteNode extends Visual {
  val parent: ReteNode
  var children: List[ReteNode] = Nil

  def toGraph(config: Config) {
    config.addEdges(this, 'children, children)
  }
}

private[rete] trait JoinerTarget extends ReteNode {
  val items = new DoubleIndex[(Int, FieldToTest), Token]()

  def makeToken(parent: Token, w: Wme): Token = {
    val token = Token(parent, w)
    // for tree-based removal begin:
    token.node = this
    if (parent != null) parent.children = token :: parent.children // check for null for top token 
    if (w != null) // we need this check for negative conditions ... and top token
      w.tokens = token :: w.tokens
    // for tree-based removal end
    token
  }

  def activationLeft(t: Token, w: Wme)

  override def toGraph(config: Config) {
    super.toGraph(config)
    config.addEdge(this, 'items, items)
  }

  override def toString: String = "items: " + items.toString
}

// ===================
// === BetaMemory: ===
// ===================

private[rete] case class BetaMemory(parent: ReteNode) extends JoinerTarget {
  var allChildren: List[ReteNode] = Nil

  def addDummyToken() {
    items.addFields(List())
    items.put(makeToken(null, null))
  }

  def activationLeft(t: Token, w: Wme) {
    val newToken = makeToken(t, w)
    items.put(newToken)
    for (join <- children)
      join.asInstanceOf[JoinNode].activationLeft(newToken)
  }

  def relink(joinNode: JoinNode) {
    // maydo: I had to add test to avoid multiple links. Seems wrong.
    if (!(children contains joinNode)) {
      children = joinNode :: children
      joinNode.leftUnlinked = false
    }
  }

  override def toGraph(config: Config) {
    super.toGraph(config)
    config.colorNode(this, "red")
  }

  override def toString: String =
    show(this, "BetaMemory",
      "items" -> items,
      "children" -> children,
      "allChildren" -> allChildren)
}

/*
We always keep a single dummy top token in the dummy top
node, just so there will be one thing to iterate over 
in the join-node-right-activation procedure.
*/

private[rete] case class PNode(rete: Rete, parent: ReteNode, production: Production) extends JoinerTarget {
  val tokenIndex: Map[Symbol, Int] = computeLabels
  val bindingInstructions: List[BindingInstruction] = computeBindingInstructions
  val bindingAllInstructions: List[BindingInstruction] = computeBindingAllInstructions

  def computeLabels: Map[Symbol, Int] = {
    var labels_ : Map[Symbol, Int] = Map()
    val conditions = production.conditions.reverse
    var wmeIndex = 0
    for (condition <- conditions) {
      condition match {
        case PC(constraints, label) =>
          val theLabel: Symbol =
            if (label != null) label
            else {
              constraints.get('kind) match {
                case Some(Constant(symbol)) => symbol.asInstanceOf[Symbol]
                case _                      => null
              }
            }
          if (theLabel != null)
            labels_ += (theLabel -> wmeIndex)
        case _ =>
      }
      wmeIndex += 1
    }
    labels_
  }

  case class BindingInstruction(
    conditionNumber: Int,
    fields: Map[Symbol, FieldToTest]) {

    override def toString: String =
      show(this, "BindingInstruction",
        "conditionNumber" -> conditionNumber,
        "fields" -> fields)
  }

  def computeBindingInstructions: List[BindingInstruction] = {
    var bindingInstructions: List[BindingInstruction] = Nil
    var conditions: List[Condition] = production.conditions.reverse
    var freeVariables: Set[Symbol] = production.action.getFreeVariables
    while (!freeVariables.isEmpty) {
      var counter = 0
      var condition: Condition = conditions.head
      conditions = conditions.tail
      var variablesBoundHere: Map[FieldToTest, Symbol] =
        condition.getEffectiveBinding filterValues freeVariables
      while (variablesBoundHere.isEmpty) {
        counter += 1
        condition = conditions.head
        conditions = conditions.tail
        variablesBoundHere = condition.getEffectiveBinding filterValues freeVariables
      }
      bindingInstructions = bindingInstructions :+
        BindingInstruction(counter, variablesBoundHere reverse)
      freeVariables = freeVariables -- variablesBoundHere.values
    }
    bindingInstructions
  }

  def computeBindingAllInstructions: List[BindingInstruction] = {
    var bindingInstructions: List[BindingInstruction] = Nil
    val conditions: List[Condition] = production.conditions.reverse
    var boundVariables: Set[Symbol] = Set()
    var counter = 0
    for (condition <- conditions) {
      condition match {
        case PC(_, _) =>
          val variablesBoundHere = condition.getEffectiveBinding filterValues (x => !(boundVariables contains x))
          if (!variablesBoundHere.isEmpty) {
            bindingInstructions = bindingInstructions :+
              BindingInstruction(counter, variablesBoundHere reverse)
            boundVariables = boundVariables ++ variablesBoundHere.values
            counter = 0
          } else counter += 1
        case _ => counter += 1
      }
    }
    bindingInstructions
  }

  def mkBindingGeneric(instructions: List[BindingInstruction])(token: Token) = {
    var binding: Map[Symbol, Value] = Map()
    var currentToken: Token = token
    for (bindingInstruction <- instructions) {
      val Token(parent, wme) = currentToken.getIthToken(bindingInstruction.conditionNumber)
      for ((variable, field) <- bindingInstruction.fields) {
        binding += (variable -> wme(field))
      }
      currentToken = parent
    }
    binding
  }

  def mkBinding(token: Token) = mkBindingGeneric(bindingInstructions)(token)
  def mkBindingAll(token: Token) = mkBindingGeneric(bindingAllInstructions)(token)

  def mkWme(token: Token, constraints: Map[FieldToTest, Pattern]): Wme = {
    val binding: Map[Symbol, Value] = mkBinding(token)
    val fields: Map[FieldToTest, Value] = constraints map {
      case (field, pattern) =>
        pattern match {
          case Constant(c) => (field -> c)
          case Variable(v) => (field -> binding(v))
        }
    }
    Wme(fields)
  }

  def execute(action: Action, token: Token) {
    action match {
      case Print(str) => // deprecate
        rete.addAction(println(str))
      case Code(stmt) =>
        rete.addAction {
          rete.executingBinding = mkBindingAll(token) // gives statement access to variables
          rete.executingTokenIndex = tokenIndex // gives statement access to condition labels for removal
          rete.executingToken = token // for removal via labels
          rete.executingRule = production // COST
          stmt()
        }
      case Add(constraints) =>
        val wme = mkWme(token, constraints)
        if (rete.HISTORY) wme.history = // COST
          rete.computeHistory(rete.executingEventNumber, rete.executingEvent, production,
            token, wme.fields)
        rete.addAction(rete.addWme(wme))
      case Del(constraints) =>
        throw new RuntimeException("del(...) not implemented") // should not be used.
      // Do something about this - perhaps:
      // From Dave Ray:
      // You're correct about removing WMEs. I don't remember exactly what I
      // did, but I think the calling code had to hold onto the WME objects and
      // use those to remove them later. It seems like you could use the alpha
      // memory hashing logic on the raw slot values to find the wme quicker
      // and remove it. Either that, or maintain a map from slot values
      // (tuples) to WMEs and look them up that way.
      //
      // Soar has a similar problem in that the RHS of rules will add/remove
      // facts using just a triple. The way it's handled there is, for each
      // identifier (the first slot in a triple) a list of all WMEs whose first
      // slot is that id is maintained. So when the rule says "remove WME (A B
      // C), the Soar code just finds id "A" and thenDoActions does a linear search for
      // the WME (A B C). Seems to work pretty well :)
      case Rem(label) =>
        val index = tokenIndex(label)
        val Token(_, wme) = token.getIthToken(index)
        rete.addAction(wme.remove())
      case And(action1, action2) =>
        execute(action1, token)
        execute(action2, token)
    }
  }

  def activationLeft(t: Token, w: Wme) {
    val newToken = makeToken(t, w)
    items.put(newToken)
    execute(production.action, newToken)
  }

  override def toGraph(config: Config) {
    super.toGraph(config)
    config.colorNode(this, "black")
    config.addFields(this, 'rule -> production.name)
  }

  override def toString: String =
    show(this, "PNode",
      "items" -> items,
      "production" -> production,
      "labels" -> tokenIndex,
      "bindingInstructions" -> bindingInstructions)
}

// ===============
// === Joiner: ===
// ===============

private[rete] trait Joiner extends ReteNode {
  val alphaMemory: AlphaMemory
  var nearestAncestorWithSameAmem: Joiner = null
  // initialized to make it type check
  var leftUnlinked: Boolean = false
  // variable not modeled in paper.
  var rightUnlinked: Boolean = false // variable not modeled in paper.

  val tests: List[TestAtJoinNode]
  val tokenFieldsToTest: List[(Int, FieldToTest)] =
    for (test <- tests) yield (test.relativeConditionNumberInToken, test.fieldOfTokenWme)
  val wmeFieldsToTest: List[FieldToTest] =
    for (test <- tests) yield test.fieldOfWme

  val indexHolder: JoinerTarget =
    if (this.isInstanceOf[NegativeNode])
      this.asInstanceOf[JoinerTarget]
    else
      parent.asInstanceOf[JoinerTarget]
  indexHolder.items.addFields(tokenFieldsToTest)
  //parent.asInstanceOf[JoinerTarget].items.addFields(tokenFieldsToTest)

  alphaMemory.items.addFields(wmeFieldsToTest)

  def performJoinTests_NOT_USED(tests: List[TestAtJoinNode], t: Token, w: Wme): Boolean = {
    for (test <- tests) {
      val valueOfWme = w(test.fieldOfWme)
      val tokenWme = t.getIthWme(test.relativeConditionNumberInToken)
      val valueOfTokenWme = tokenWme(test.fieldOfTokenWme)
      if (valueOfWme != valueOfTokenWme) return false
    }
    true
  }

  def activationRight(w: Wme)

  override def toGraph(config: Config) {
    super.toGraph(config)
    config.shapeNode(this, "Mrecord")
    //config.addFields(this, 'leftUnlinked -> leftUnlinked)
    //config.addFields(this, 'rightUnlinked -> rightUnlinked)
    config.addFields(this,
      'tokenFieldsToTest -> tokenFieldsToTest,
      'wmeFieldsToTest -> wmeFieldsToTest)
    if (!parent.children.contains(this)) config.addEdge(parent, 'children, this, "style" -> "dashed")
    if (!alphaMemory.successors.contains(this)) config.addEdge(alphaMemory, 'successors, this, "style" -> "dashed")
    config.addEdge(this, 'nearestAncestorWithSameAmem, nearestAncestorWithSameAmem)
  }
}

// =================
// === JoinNode: ===
// =================

private[rete] case class JoinNode(
  parent: BetaMemory,
  alphaMemory: AlphaMemory,
  tests: List[TestAtJoinNode],
  domain: Set[Symbol]) extends Joiner {
  def activationLeft(t: Token) {
    assert(parent.items.wasEmpty == rightUnlinked) // See comment below.
    if (parent.items.wasEmpty) {
      // Should test rightUnlinked (p. 88 fn. 5).
      alphaMemory.relink(this)
      if (alphaMemory.items.isEmpty) {
        parent.children = parent.children - this
        leftUnlinked = true
      }
    }
    val values = t.extractValues(tokenFieldsToTest).get
    for (w <- alphaMemory.items.get(wmeFieldsToTest, values); child <- children) {
      child.asInstanceOf[JoinerTarget].activationLeft(t, w)
    }
  }

  def activationRight(w: Wme) {
    if (alphaMemory.items.wasEmpty) {
      parent.relink(this)
      if (parent.items.isEmpty) {
        alphaMemory.successors = alphaMemory.successors - this
        rightUnlinked = true
      }
    }
    if (domain subsetOf w.fields.keySet) {
      // COST
      val values = w.extractValues(wmeFieldsToTest).get
      for (t <- parent.items.get(tokenFieldsToTest, values); child <- children) {
        child.asInstanceOf[JoinerTarget].activationLeft(t, w)
      }
    }
  }

  override def toGraph(config: Config) {
    super.toGraph(config) // the Joiner method
    //toGraphReteNode(config) // necessary due to diamond of death - no more necessary
    //config.addEdge(this, 'alphaMemory, alphaMemory)
    config.addEdges(this, 'tests, tests)
    config.colorNode(this, "magenta")
  }

  override def toString: String =
    show(this, "JoinNode",
      "children" -> children,
      "alphaMemory" -> alphaMemory,
      "tests" -> tests,
      "nearestAncestorWithSameAmem" -> nearestAncestorWithSameAmem)
}

private[rete] case class TestAtJoinNode(
  fieldOfWme: FieldToTest, // of wme in AlphaMemory
  relativeConditionNumberInToken: Int, // of token in BetaMemory, relative
  fieldOfTokenWme: FieldToTest // of wme in token
  ) extends Visual {

  def toGraph(config: Config) {
    config.colorNode(this, "dodgerblue4")
    config.addFields(this,
      'fieldOfWme -> fieldOfWme,
      'relCondNoInToken -> relativeConditionNumberInToken,
      'fieldOfTokenWme -> fieldOfTokenWme)
  }

  override def toString: String =
    show(this, "TestAtJoinNode",
      "fieldOfWme" -> fieldOfWme,
      "relCondNoInToken" -> relativeConditionNumberInToken,
      "fieldOfTokenWme" -> fieldOfTokenWme)
}

// ======================
// === Negative Node: ===
// ======================

private[rete] case class NegativeNode(
  parent: ReteNode,
  alphaMemory: AlphaMemory,
  tests: List[TestAtJoinNode])
  extends JoinerTarget with Joiner {

  def activationLeft(t: Token, w: Wme) {
    if (items.isEmpty) alphaMemory.relink(this)
    // build and store a new token, just like a beta memory would
    val newToken = makeToken(t, w)
    items.put(newToken)
    // compute the join results
    val values = newToken.extractValues(tokenFieldsToTest).get
    for (wme <- alphaMemory.items.get(wmeFieldsToTest, values)) {
      val jr = NegativeJoinResult(newToken, wme) // Thesis page 42 says 'w', but I think it is wrong
      newToken.joinResults = jr :: newToken.joinResults
      wme.negativeJoinResults = jr :: wme.negativeJoinResults // Thesis page 42 says 'w', but I think it is wrong
    }
    // If join results is empty, thenDoActions inform children
    if (newToken.joinResults == Nil) {
      for (child <- children)
        child.asInstanceOf[JoinerTarget].activationLeft(newToken, null)
    }
  }

  def activationRight(w: Wme) {
    val values = w.extractValues(wmeFieldsToTest).get
    for (t <- indexHolder.items.get(tokenFieldsToTest, values)) {
      if (t.joinResults == Nil) t.removeDescendents()
      val jr = NegativeJoinResult(t, w)
      t.joinResults = jr :: t.joinResults
      w.negativeJoinResults = jr :: w.negativeJoinResults
    }
  }

  //  def activationRight(w: Wme) {
  //    val values = w.extractValues(wmeFieldsToTest).get
  //    for (t <- parent.asInstanceOf[JoinerTarget].items.get(tokenFieldsToTest, values)) {
  //      if (t.joinResults == Nil) t.removeDescendents()
  //      val jr = NegativeJoinResult(t, w)
  //      t.joinResults = jr :: t.joinResults
  //      w.negativeJoinResults = jr :: w.negativeJoinResults
  //    }
  //  }

  override def toGraph(config: Config) {
    super.toGraph(config) // Joiner's method
    config.addEdges(this, 'tests, tests)
    config.colorNode(this, "yellow4")
  }

  override def toString: String =
    show(this, "NegativeNode",
      "items" -> items,
      "alphaMemory" -> alphaMemory,
      "nearestAncestorWithSameAmem" -> nearestAncestorWithSameAmem,
      "children" -> children,
      "tests" -> tests)
}

// ================
// === NccNode: ===
// ================

private[rete] case class NccNode(parent: ReteNode) extends JoinerTarget {
  var partner: NccPartnerNode = null // points to the corresponding NCC partner node

  def activationLeft(t: Token, w: Wme) {
    val newToken = makeToken(t, w)
    items.put(newToken)
    // maydo: It seems as if the original algorithm has a bug here. Tokens need to match:
    // get initial ncc results
    for (result <- partner.newResultBuffer) {
      newToken.nccResults = result :: newToken.nccResults
      result.owner = newToken
    }
    partner.newResultBuffer = Nil // instead of deleting one by one

    if (newToken.nccResults == Nil) {
      // No ncc results, so inform children
      for (child <- children)
        child.asInstanceOf[JoinerTarget].activationLeft(newToken, null)
    }
  }

  override def toString: String =
    show(this, "NccNode",
      "children" -> children,
      "items" -> items,
      "partner" -> partner)
}

private[rete] case class NccPartnerNode(parent: ReteNode, numberOfConjuncts: Int) extends JoinerTarget {
  // Note that 'items' is not needed.
  // We needed to extend JoinNodeTarget to make this typecheck.
  // The OO typing starts getting too restrictive.
  var nccNode: JoinerTarget = null
  // points to the corresponding NCC node
  var newResultBuffer: List[Token] = Nil // results for the match the NCC node hasn't heard about

  def activationLeft(t: Token, w: Wme) {
    val newResult = makeToken(t, w)

    // Find the appropriate owner token (into whose local memory we should put this result)

    var owners_t: Token = t
    var owners_w: Wme = w
    for (i <- 1 to numberOfConjuncts) {
      owners_w = owners_t.wme
      owners_t = owners_t.parent
    }

    // Look for this owner in the NCC node's memory. If we find it, add newResult to its
    // local memory, and propagate (deletions) to the NCC node's children. 

    // maydo: We need a different kind of index for this: from (parent,wme) to (parent,wme).
    nccNode.items.getAllElements somematch {
      case owner @ Token(parentToken, wme) if parentToken == owners_t && wme == owners_w =>
        owner.nccResults = newResult :: owner.nccResults
        newResult.owner = owner
        owner.removeDescendents()
    } otherwise {
      // We didn't find an appropriate owner token already in the NCC node's memory,
      // so we just stuff the result in our own temporary buffer.
      newResultBuffer = newResult :: newResultBuffer
    }
  }

  override def toString: String =
    show(this, "NccPartnerNode",
      "numberOfConjuncts" -> numberOfConjuncts,
      "children" -> children,
      "nccNode" -> nccNode,
      "newResultBuffer" -> newResultBuffer)
}

// =============
// === Rete: ===
// =============

/**
 * This class offers the LogFire options that a user can set. Each
 * option is a variable that (1) has a default value, and (2) can be
 * re-assigned with an assignment statment. All options are spelled with
 * capital letters. The <code>Monitor</code> class extends the <code>Options</code>
 * class. Hence these option variables are accessible on a monitor object as public
 * variables.
 */

class Options {
  /**
   * When set to true: will cause printing of consumed events and user-added facts. 
   * Is by default set to false. 
   */
  var PRINT_EVENTS = false

  /**
   * When set to true: will cause printing of fact memory after each event/fact processed.
   * It is by default set to false.
   */
  var PRINT_FACTS = false

  /**
   * When set to true: will have the same effect as setting both PRINT_EVENTS 
   * and PRINT_FACTS to true.
   */
  var PRINT = false  
  
  /**
   * When set to true: will cause printing of error messages, and other progress information,
   * on standard out. The flag is by default set to true. It can be set to false in cases,
   * where results are processed in other ways automatically, for example in tests.
   */
  var PRINT_PROGRESS = true  
    
  /**
   * When set to true: various messages will be printed out in colors.
   * If the output medium does not support color printing, color characters will appear anyway,
   * which will be confusing. Therefore the default value is false.
   */
  var COLOR = false

  /**
   * When set to true: will for each generated fact store the set of events and facts
   * that led to the generation of this fact. This history information will for example be printed as
   * error traces when errors are reported. The flag is by default set to true. It is recommended to
   * set it to false in case time and memory usage becomes a problem.
   */
  var HISTORY = true

  /**
   * When set to true: will cause the monitor to stop after the first error in the trace has
   * been encountered. If false, the monitor will continue monitoring the trace after a fail.
   * The flag is by default set to false.
   */
  var STOP_AFTER_FAIL = false
}

/**
 * This class represents the creation of a fact during monitoring, recorded as a line
 * in a history (i.e. an error trace). It indicates what event was the cause, what rule was fired, and what fact
 * was created.
 *
 * @param eventNumber position of event in input trace.
 * @param event the event causing the rule to fire.
 * @param rule the rule that fired.
 * @param fact the generated fact.
 */

case class Line(eventNumber: Int, event: Map[Symbol, Any], rule: String, fact: Map[Symbol, Any]) {
  override def toString: String = {
    val E = PP.bindingToString(event)
    val F = PP.bindingToString(fact)
    s"[$eventNumber] $E --> $F\n        rule: $rule"
  }
}

/**
 * This class represents the history of a created fact. The history outlines what event lead to
 * the creation of the fact, what rule created it; and finally other relevant events, rules fired,
 * and facts created (the ''cone of influence'').
 *
 * @param fact the generated fact.
 * @param eventNumber position of triggering event in input trace.
 * @param event the event causing the rule to fire.
 * @param rule the rule that fired.
 * @param histories the histories of the previous events, rules, and facts.
 * @param message an error message in case the history was created due to a failure.
 */

case class History(fact: Map[Symbol, Any], eventNumber: Int, event: Map[Symbol, Any], rule: String, histories: List[History], message: String) {
  /**
   * Returns the individual lines of the history, sorted according to line numbers, and
   * without duplicates. Note that duplicates can occur in a history.
   *
   * @return the lines of the history, sorted and without duplicates.
   */
  def getLines: List[Line] = {
    var lines: Set[Line] = Set(Line(eventNumber, event, rule, fact))
    for (history <- histories)
      lines ++= history.getLines
    lines.toList.sortWith((line1, line2) => line1.eventNumber < line2.eventNumber)
  }

  override def toString: String = {
    val msg: String = "*** " + (if (message != null) message else "Recorded trace")
    val line = "-" * msg.length
    "\n" + line + "\n" + msg + "\n" + line + "\n\n" + getLines.mkString("", "\n\n", "")
  }
}

private[rete] class Rete extends Options with Visual {
  private val alphaNetwork = new AlphaNetwork
  private val dummyTopNode = BetaMemory(null)
  private var pNodeOfProduction: Map[Production, PNode] = Map()
  // not in paper, used for production removal
  private var productions: List[Production] = Nil // not in paper, used for printing

  // Variables needed by 'code' actions.
  // The variables will be assigned to before the code actions are executed.

  private[rete] var executingBinding: Binding = null
  // for referring to bound variables
  private[rete] var executingTokenIndex: Map[Symbol, Int] = null
  // for removing facts
  private[rete] var executingToken: Token = null // for removing facts via labels

  // number of event currently being processed
  private[rete] var executingEventNumber: Int = 0
  // the event currentlt being processed
  private[rete] var executingEvent: Wme = new Wme(Map('kind -> 'START_OF_SESSION))
  // name of rule currently being evaluated
  private[rete] var executingRule: Production = null

  private var finalizedActions: List[Unit => Any] = List()

  private[rete] def isEmpty: Boolean = finalizedActions == Nil

  private[rete] def addAction(stmt: => Any) {
    finalizedActions :+= ((u: Unit) => stmt)
  }

  // public but not in API doc. Used for FirePlace only.
  def getBinding(sym: Symbol): Any = executingBinding(sym)

  // public but not in API doc. Used for FirePlace only.
  def putBinding(sym: Symbol, value: Any) {
    executingBinding += (sym -> value)
  }

  private[rete] def executeActions() {
    val factions = finalizedActions
    finalizedActions = Nil
    for (faction <- factions) faction()
  }

  dummyTopNode.addDummyToken()

  /**
   * Prints all the rules entered as part of the definition of this monitor.
   */

  def printRules() {
    val className = this.getClass.getSimpleName
    val title = s"=== $className Rules: ==="
    println()
    println(title)
    for (production <- productions) {
      println()
      println("  " + production)
    }
    println()
    println("=" * title.length)
  }

  private[rete] def toGraph(config: Config) {
    config.colorNode(this, "green")
    config.addEdge(this, 'alpha, alphaNetwork)
    config.addEdge(this, 'beta, dummyTopNode)
  }

  override def toString: String = {
    //PointerBank.reset()
    "\n\n--- RETE NETWORK: ---\n\n" +
      show(this, "Rete",
        "dummyTopNode" -> dummyTopNode,
        "alphaNetwork" -> alphaNetwork,
        "pNodeOfProduction" -> pNodeOfProduction)
  }

  private[rete] def toStringWmes: String = {
    var result = ""
    var nl = ""
    val wmes = alphaNetwork.getAllWmes.toList.sortWith((wme1,wme2) => wme1.le(wme2))
    for (wme <- wmes) {
      result += nl + wme.toStringSimple
      nl = "\n"
    }
    result
  }

  private[rete] def computeHistory(eventNumber: Int, event: Wme, rule: Production, token: Token, fact: Binding, message: String = null): History = {
    var histories: List[History] = Nil
    if (HISTORY) {
      var nextToken = token
      while (nextToken != null) {
        val wme = nextToken.wme
        if (wme != null) {
          if (wme.history != null)
            histories ::= wme.history
        }
        nextToken = nextToken.parent
      }
    }
    History(fact, eventNumber, event.fields, rule.toString, histories, message)
  }

  private[rete] def addWme(w: Wme) {
    alphaNetwork.activation(w)
  }

  private[rete] def removeWme(w: Wme) {
    w.remove()
  }

  private[rete] def getAllWmes: Set[Wme] = alphaNetwork.getAllWmes

  private def build_or_share_beta_memory_node(parent: ReteNode): BetaMemory = {
    for (child <- parent.children if child.isInstanceOf[BetaMemory])
      return child.asInstanceOf[BetaMemory]
    val betamem = new BetaMemory(parent)
    parent.children = betamem :: parent.children // maydo: this is not in the paper
    update_new_node_with_matches_from_above(betamem)
    betamem
  }

  private def build_or_share_join_node(bm: BetaMemory, am: AlphaMemory, tests: List[TestAtJoinNode], domain: Set[Symbol]): JoinNode = {
    for (child <- bm.allChildren if child.isInstanceOf[JoinNode]) {
      val joinNode = child.asInstanceOf[JoinNode]
      if (joinNode.alphaMemory.eq(am) && joinNode.tests == tests && joinNode.domain == domain) return joinNode
    }
    val joinNode = new JoinNode(bm, am, tests, domain)
    bm.children = joinNode :: bm.children
    //insert new at the head of the list parent.all-children
    bm.allChildren = joinNode :: bm.allChildren
    am.successors = joinNode :: am.successors
    joinNode.rightUnlinked = false
    am.referenceCount += 1
    joinNode.nearestAncestorWithSameAmem = findNearestAncestorWithSameAmem(bm, am)
    // Unlink right away if either memory is empty
    if (bm.items.isEmpty) {
      am.successors = am.successors - joinNode
      joinNode.rightUnlinked = true
    } else if (am.items.isEmpty) bm.children = bm.children - joinNode
    joinNode
  }

  private def build_or_share_negative_node(parent: ReteNode, am: AlphaMemory,
                                           tests: List[TestAtJoinNode]): NegativeNode = {
    // look for an existing node to share
    for (child <- parent.children if child.isInstanceOf[NegativeNode]) {
      val negNode = child.asInstanceOf[NegativeNode]
      if (negNode.alphaMemory.eq(am) && negNode.tests == tests) return negNode
    }
    val negNode = NegativeNode(parent, am, tests)
    parent.children = negNode :: parent.children
    am.successors = negNode :: am.successors
    negNode.rightUnlinked = false
    am.referenceCount += 1
    negNode.nearestAncestorWithSameAmem = findNearestAncestorWithSameAmem(parent, am)
    update_new_node_with_matches_from_above(negNode)
    // Right-unlink the node if it has no tokens
    if (negNode.items.isEmpty) {
      am.successors = am.successors - negNode
      negNode.rightUnlinked = true
    }
    negNode
  }

  private def build_or_share_ncc_nodes(parent: ReteNode, c: NCC, earlierConds: List[Condition]): NccNode = {
    val bottomOfSubnetwork = build_or_share_network_for_conditions(parent, c.conditions, earlierConds)
    for (child <- parent.children if child.isInstanceOf[NccNode]) {
      // look for an existing node to share
      val nccNode = child.asInstanceOf[NccNode]
      if (nccNode.partner.parent eq bottomOfSubnetwork) return nccNode
    }
    val nccNode = NccNode(parent)
    val nccPartner = NccPartnerNode(bottomOfSubnetwork, c.conditions.length)
    nccNode.partner = nccPartner
    nccPartner.nccNode = nccNode
    parent.children = parent.children :+ nccNode // so the subnetwork comes first
    bottomOfSubnetwork.children = nccPartner :: bottomOfSubnetwork.children
    // We have to inform nccNode of existing matches before informing the partner,
    // otherwise lots of matches would all get mixed together in the newResultBuffer.
    update_new_node_with_matches_from_above(nccNode)
    update_new_node_with_matches_from_above(nccPartner)
    nccNode
  }

  //   if current-node != top-dummy-node
  //      current-node <- build-or-share-beta-memory-node (current-node)

  private def build_or_share_network_for_conditions(
    parent: ReteNode, conds: List[Condition], earlierConds: List[Condition]): ReteNode = {
    var currentNode: ReteNode = parent
    var condsHigherUp: List[Condition] = earlierConds
    for (c <- conds) {
      c match {
        case PC(constraints, _) =>
          val pc = c.asInstanceOf[PC]
          if (!(currentNode eq dummyTopNode)) // this test does not appear in paper
            currentNode = build_or_share_beta_memory_node(currentNode)
          val tests = get_join_tests_from_condition(pc, condsHigherUp)
          val am = alphaNetwork.build_or_share_alpha_memory(pc)
          currentNode = build_or_share_join_node(currentNode.asInstanceOf[BetaMemory], am, tests, constraints.keySet)
        case NC(constraints) =>
          val nc = c.asInstanceOf[NC]
          val tests = get_join_tests_from_condition(nc, condsHigherUp)
          val am = alphaNetwork.build_or_share_alpha_memory(nc)
          currentNode = build_or_share_negative_node(currentNode, am, tests)
        case NCC(conditions) =>
          val ncc = c.asInstanceOf[NCC]
          currentNode = build_or_share_ncc_nodes(currentNode, ncc, condsHigherUp)
      }
      condsHigherUp = condsHigherUp :+ c
    }
    currentNode
  }

  private def get_join_tests_from_condition(condition: BasicCondition, earlierConditions: List[Condition]): List[TestAtJoinNode] = {
    var result: List[TestAtJoinNode] = Nil
    for ((field, Variable(v)) <- condition.constraints) {
      occurs_anywhere_in_positive(v, earlierConditions) match {
        case None =>
        case Some((field2, index)) =>
          result = TestAtJoinNode(field, index, field2) :: result
      }
    }
    result
  }

  private def occurs_anywhere_in_positive(symbol: Symbol, conditions: List[Condition]): Option[(FieldToTest, Int)] = {
    var index = 0
    for (condition <- conditions.reverse) {
      // reversed compared to first version
      if (condition.isInstanceOf[PC]) {
        val PC(constraints, _) = condition
        for ((field, Variable(`symbol`)) <- constraints) return Some(field, index)
      }
      index += 1
    }
    None
  }

  private def findNearestAncestorWithSameAmem(node: ReteNode, am: AlphaMemory): Joiner = {
    if (node eq dummyTopNode) return null
    if (node.isInstanceOf[Joiner]) {
      val joiner = node.asInstanceOf[Joiner]
      if (joiner.alphaMemory eq am) return joiner
    }
    if (node.isInstanceOf[NccNode]) {
      val nccnode = node.asInstanceOf[NccNode]
      findNearestAncestorWithSameAmem(nccnode.partner.parent, am)
    } else {
      findNearestAncestorWithSameAmem(node.parent, am)
    }
  }

  private def build_or_share_production_node(rete: Rete, node: ReteNode, p: Production): PNode = {
    val pNode = new PNode(rete, node, p)
    node.children = pNode :: node.children
    pNodeOfProduction = pNodeOfProduction + (p -> pNode)
    pNode
  }

  private def update_new_node_with_matches_from_above(newNode: ReteNode) {
    val parent = newNode.parent
    parent match {
      case parentBetaMemory: BetaMemory =>
        val newJoinNode = newNode.asInstanceOf[JoinNode] // it must be a JoinNode .. I think
        for (token <- parentBetaMemory.items.getAllElements) newJoinNode.activationLeft(token)
      case parentJoinNode: JoinNode =>
        val saved_list_of_children = parentJoinNode.children
        parentJoinNode.children = List(newNode)
        for (wme <- parentJoinNode.alphaMemory.items.getAllElements) parentJoinNode.activationRight(wme)
        parentJoinNode.children = saved_list_of_children
      case parentNegativeNode: NegativeNode =>
        val newJoinNodeTarget = newNode.asInstanceOf[JoinerTarget]
        for (token <- parentNegativeNode.items.getAllElements if token.joinResults == Nil)
          newJoinNodeTarget.activationLeft(token, null)
      case parentNccNode: NccNode =>
        val newJoinNodeTarget = newNode.asInstanceOf[JoinerTarget]
        for (token <- parentNccNode.items.getAllElements if token.nccResults == Nil)
          newJoinNodeTarget.activationLeft(token, null)
    }
  }

  private[rete] def addProduction(production: Production) {
    productions :+= production
    val currentNode = build_or_share_network_for_conditions(dummyTopNode, production.conditions, Nil)
    val pNode = build_or_share_production_node(this, currentNode, production)
    update_new_node_with_matches_from_above(pNode)
  }

  // Perhaps one should use the name of a production instead.

  private[rete] def removeProduction(prod: Production) {
    delete_node_and_any_unused_ancestors(pNodeOfProduction(prod))
  }

  private def delete_node_and_any_unused_ancestors(node: ReteNode) {

    // For NCC nodes, delete the partner node too

    if (node.isInstanceOf[NccNode]) {
      val nccNode = node.asInstanceOf[NccNode]
      delete_node_and_any_unused_ancestors(nccNode.partner)
    }

    // Clean up any tokens the node contains

    if (node.isInstanceOf[BetaMemory] || node.isInstanceOf[NegativeNode] || node.isInstanceOf[NccNode]) {
      val nodeWithItems = node.asInstanceOf[JoinerTarget]
      for (token <- nodeWithItems.items.getAllElements) token.remove()
      // maydo: I think the Nil assignment is not necessary due to garbage collection.
      // nodeWithItems.items = Nil
    }
    if (node.isInstanceOf[NccPartnerNode]) {
      val partner = node.asInstanceOf[NccPartnerNode]
      for (token <- partner.newResultBuffer) token.remove()
      partner.newResultBuffer = Nil
    }

    // Deal with the alpha memory

    if (node.isInstanceOf[JoinNode] || node.isInstanceOf[NegativeNode]) {
      val joiner = node.asInstanceOf[Joiner]
      if (!joiner.rightUnlinked) {
        joiner.alphaMemory.successors = joiner.alphaMemory.successors - joiner
      }
      joiner.alphaMemory.referenceCount -= 1
      if (joiner.alphaMemory.referenceCount == 0)
        alphaNetwork.delete_alpha_memory(joiner.alphaMemory)
    }

    // Deal with the parent

    val parent = node.parent
    // if (node is not left-unlinked) -- does not seem necessary, removal is rare
    parent.children = parent.children - node
    if (node.isInstanceOf[JoinNode]) {
      // remove node from the list node.parent.all-children
      val betaMemory = parent.asInstanceOf[BetaMemory]
      betaMemory.allChildren = betaMemory.allChildren - node
      if (betaMemory.allChildren == Nil)
        delete_node_and_any_unused_ancestors(betaMemory)
    } else if (parent.children == Nil)
      delete_node_and_any_unused_ancestors(parent)
    // deallocate memory for node
  }
}
