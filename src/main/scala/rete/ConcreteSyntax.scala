package rete

import Util._
import scala.language.implicitConversions
import scala.util.control.Breaks._
import org.apache.commons.csv._

/**
 * This trait offers methods for timing the execution of code fragments.
 * It is useful for studying the efficiency of an implementation.
 */

trait Profiling {
  private var timings: List[String] = Nil

  /**
   * Executes its argument and returns the time it takes.
   *
   * @param f the code to be executed and timed.
   * @return the time in milliseconds it takes to execute the code.
   */

  def time(f: => Unit) = {
    val startTime = System.currentTimeMillis
    f
    val endTime = System.currentTimeMillis
    val duration = endTime - startTime
    duration
  }

  /**
   * Executes its argument and returns the time it takes.
   * In addition, it prints out the time in a format divided
   * into minutes, seconds and milliseconds. For each application
   * of this method all previous timings are printed as well so
   * they all can be seen together for each call.
   *
   * @param text printed when timing is printed, to identify code fragment in question.
   * @param f the code to be executed and timed.
   * @return the time in milliseconds it takes to execute the code.
   */

  def time(text: String)(f: => Unit) = {
    val startTime = System.currentTimeMillis
    f
    val endTime = System.currentTimeMillis
    val milliseconds = endTime - startTime
    val seconds = milliseconds / 1000
    val ms = milliseconds % 1000
    val min = seconds / 60
    val sec = seconds % 60
    timings :+= "--- timing of %s: %d ms = %d min %d sec %d ms".format(text, milliseconds, min, sec, ms)
    for (timing <- timings) println(timing)
    milliseconds
  }
}

private object Serialization {

  import java.io._

  def serialize(file: String, obj: Any) {
    try {
      val fileOut: FileOutputStream = new FileOutputStream(file)
      val out: ObjectOutputStream = new ObjectOutputStream(fileOut)
      out.writeObject(obj)
      out.close()
      fileOut.close()
    } catch {
      case i: IOException => i.printStackTrace()
    }
  }

  def unserialize(file: String): Any = {
    var result: Any = null
    try {
      val fileIn: FileInputStream = new FileInputStream(file)
      val in: ObjectInputStream = new ObjectInputStream(fileIn)
      result = in.readObject()
      in.close()
      fileIn.close()
    } catch {
      case i: IOException =>
        i.printStackTrace()
        return
      case c: ClassNotFoundException =>
        println("class not found")
        c.printStackTrace()
        return
    }
    result
  }
}

private class Database extends Serializable {
  private var facts: Set[Binding] = Set()

  def addFact(fact: Binding) {
    facts += fact
  }

  def getFacts: Set[Binding] = facts
}

/**
 * This class contains the results of a trace analysis as a list of messages
 * (of type `String`). If the `HISTORY` option is set to true, the result will
 * also be recorded as a list of `History` objects.
 */

class MonitorResult {
  private var histories: List[History] = Nil

  private[rete] def putHistory(history: History) {
    histories +:= history
  }

  /**
   * Returns the list of recorded `History` objects, each representing a property violation
   * due to a call of `fail`, or other important event signaled by a call of `reportResult`.
   *
   * @return the list of `History` objects.
   */
  def getHistories: List[History] = histories

  /**
   * Returns true if there are recorded `History` objects, representing possible errors.
   */
  def errorsReported: Boolean = !histories.isEmpty

  override def toString: String = {
    val resultText = "LogFire Smoke:"
    val line = "=" * resultText.length
    val resultBanner = line + "\n" + resultText + "\n" + line
    val summary = histories.length match {
      case 0 =>
        "No smoke!"
      case 1 =>
        "There was 1 issue detected!"
      case n =>
        s"There were $n issues detected!"
    }
    "\n\n" + resultBanner + "\n\n" +
      summary + "\n" + histories.mkString("\n\n") + "\n\n" +
      "--- end of smoke ---"
  }
}

/**
 * This class offers the DSL (Domain Specific Language) for writing rules. It is extended by class
 * <code>Monitor</code> and is therefore not meant to be used on its own.
 */

class Logic extends Rete {

  //=========
  // Results:
  //=========

  protected[rete] val monitorResult = new MonitorResult

  /**
   * Returns a `MonitorResult` object representing the trace analysis results.
   *
   * @return the result of the trace analysis as a `MonitorResult` object.
   */

  def getMonitorResult: MonitorResult = monitorResult

  // =====================
  // Condition Evaluation:
  // =====================

  /**
   * Evaluates a PC (Positive Condition) to a nicely formatted string, where all variables are
   * replaced with the values they are bound to.
   *
   * @param pc the positive condition to be converted to a string.
   * @return the string representation of the positive condition.
   */

  protected def eval(pc: PC): String = {
    var result: Map[Symbol, Any] = Map()
    for ((field, pattern) <- pc.constraints) {
      pattern match {
        case Variable(x) =>
          result += (field -> get[Any](x))
        case Constant(c) =>
          result += (field -> c)
      }
    }
    PP.bindingToString(result)
  }

  // ===========
  // Exceptions:
  // ===========

  /**
   * An exception of this class is thrown in case a syntax error in the rule specification
   * is detected.
   *
   * @param msg message explaining the syntax violation.
   */

  class SyntaxException(msg: String) extends RuntimeException("!!! " + msg)

  // =======
  // Colors:
  // =======

  private def color(color: String, text: String): String = if (COLOR) color + text + Console.RESET else text

  private def red(text: String): String = color(Console.RED, text)

  //private def blue(text: String): String = color(Console.BLUE, text)
  //private def green(text: String): String = color(Console.GREEN, text)

  // =======================================
  // Creating Unique Symbols for Fact Kinds:
  // =======================================

  private var symbolCounter = 0

  /**
   * Returns a fresh unique symbol, which will not collide with other symbols used for
   * internals fact kinds. The returned symbol has the form <code>'internalFact&#95;&#95;</code>''<number>''.
   *
   * @return the fresh symbol.
   */

  protected def newSymbol(): Symbol = newSymbol('internalFact)

  /**
   * Returns a fresh unique symbol, which will not collide with other symbols used for
   * internals fact kinds. The returned symbol has the form ''kind''<code>&#95;&#95;</code>''<number>''.
   *
   * @param kind
   * @return the fresh symbol.
   */

  protected def newSymbol(kind: Symbol): Symbol = {
    symbolCounter += 1
    Symbol(kind.toString.substring(1) + "__" + symbolCounter)
  }

  // ==========================
  // Accessing Bound Variables:
  // ==========================

  /**
   * Reads the value that a symbol has been bound to on the left-hand side of a rule.
   * The function can be called on the right-hand side of the rule to access the value.
   * The type parameter must be provided to indicate the type of the value.
   *
   * @param v the symbol being bound to a value.
   * @tparam T the expected type of the value.
   * @return the value that the symbol is bound to.
   */

  protected def get[T](v: Symbol): T = executingBinding(v).asInstanceOf[T] // a generic function

  /**
   * Lifts a symbol to the integer value that the symbol has been
   * bound to on the left-hand side of a rule. This implicit function allows to use a
   * symbol in a context where an integer is expected. Note that it may be safer
   * to explicitly access the value by using: <code>'x.i</code> or
   * <code>get[Int]('x)</code>, to avoid implicit conversion confusion.
   *
   * @param x the symbol being bound to an integer.
   * @return the integer value that the symbol is bound to.
   */

  implicit def symbolToInt(x: Symbol): Int = get[Int](x)

  /**
   * Lifts a symbol to the Boolean value that the symbol has been
   * bound to on the left-hand side of a rule. This implicit function allows to use a
   * symbol in a context where a Boolean is expected. Note that it may be safer
   * to explicitly access the value by using
   * <code>get[Boolean]('x)</code>, to avoid implicit conversion confusion.
   *
   * @param x the symbol being bound to a Boolean.
   * @return the Boolean value that the symbol is bound to.
   */

  implicit def symbolToBoolean(x: Symbol) = get[Boolean](x)

  /**
   * Lifts a symbol to an object, which provides methods on the symbol for accessing
   * the value, that the symbol has been bound to on the left-hand side of a rule. There
   * is a method (named with single letter names) for each possible (typical) type.
   *
   * @param symbol the symbol to be lifted.
   * @return the object providing value accessor methods.
   */

  implicit def symbolToSymbolOperations(symbol: Symbol) = new SymbolOperations(symbol)

  /**
   * This class provides operations on a symbol for accessing the value it has been
   * bound to on the left-hand side of a rule.
   *
   * @param symbol the symbol that has been bound to a value on the left-hand side of a rule.
   */

  class SymbolOperations(symbol: Symbol) {
    /**
     * Looks up the integer value that the symbol has been bound to.
     * This is the same value as <code>get[Int](symbol)</code>.
     *
     * @return the integer value the symbol is bound to.
     */
    def i = get[Int](symbol)

    /**
     * Looks up the integer value that the symbol has been bound to.
     * This is the same value as <code>get[Int](symbol)</code>.
     *
     * @return the integer value the symbol is bound to.
     */
    def int = get[Int](symbol)

    /**
     * Looks up the float value that the symbol has been bound to.
     * This is the same value as <code>get[Float](symbol)</code>.
     *
     * @return the float value the symbol is bound to.
     */
    def f = get[Float](symbol)

    /**
     * Looks up the float value that the symbol has been bound to.
     * This is the same value as <code>get[Float](symbol)</code>.
     *
     * @return the float value the symbol is bound to.
     */
    def float = get[Float](symbol)

    /**
     * Looks up the double value that the symbol has been bound to.
     * This is the same value as <code>get[Double](symbol)</code>.
     *
     * @return the double value the symbol is bound to.
     */
    def d = get[Double](symbol)

    /**
     * Looks up the double value that the symbol has been bound to.
     * This is the same value as <code>get[Double](symbol)</code>.
     *
     * @return the double value the symbol is bound to.
     */
    def double = get[Double](symbol)

    /**
     * Looks up the string value that the symbol has been bound to.
     * This is the same value as <code>get[String](symbol)</code>.
     *
     * @return the string value the symbol is bound to.
     */
    def s = get[String](symbol)

    /**
     * Looks up the string value that the symbol has been bound to.
     * This is the same value as <code>get[String](symbol)</code>.
     *
     * @return the string value the symbol is bound to.
     */
    def string = get[String](symbol)

    /**
     * Looks up the Boolean value that the symbol has been bound to.
     * This is the same value as <code>get[Boolean](symbol)</code>.
     *
     * @return the Boolean value the symbol is bound to.
     */
    def bool = get[Boolean](symbol)

    /**
     * Looks up the ''untyped'' value that the symbol has been bound to.
     * This is the same value as <code>get[Any](symbol)</code>.
     *
     * @return the value the symbol is bound to.
     */
    def a = get[Any](symbol)

    /**
     * Looks up the ''untyped'' value that the symbol has been bound to.
     * This is the same value as <code>get[Any](symbol)</code>.
     *
     * @return the value the symbol is bound to.
     */
    def any = get[Any](symbol)

    /**
     * Looks up the binding that the symbol has been bound to.
     * This is the same value as <code>get[Binding](symbol)</code>.
     *
     * @return the binding the symbol is bound to.
     */
    def b = get[Binding](symbol)

    /**
     * Looks up the set of ''untyped'' values that the symbol has been bound to.
     * This is the same value as <code>get[Set[Any]](symbol)</code>.
     *
     * @return the value the symbol is bound to.
     */
    def set = get[Set[Any]](symbol)
  }

  // =================
  // Defining Symbols:
  // =================

  private val UNDEFINED_SYMBOL = 'undefinedSymbol

  /**
   * Defines a symbol.
   * Note that the symbol must be defined in the monitor or in a '''trait''' that the monitor extends.
   * It cannot be defined in a '''class''' that the monitor extends.
   *
   * @return the symbol.
   */

  def symbol = UNDEFINED_SYMBOL

  /**
   * Defines an event kind.
   * Note that the symbol must be defined in the monitor or in a '''trait''' that the monitor extends.
   * It cannot be defined in a '''class''' that the monitor extends.
   *
   * @return event kind.
   */
  def event = UNDEFINED_SYMBOL

  /**
   * Defines a fact kind.
   * Note that the symbol must be defined in the monitor or in a '''trait''' that the monitor extends.
   * It cannot be defined in a '''class''' that the monitor extends.
   *
   * @return fact kind.
   */
  def fact = UNDEFINED_SYMBOL

  private[rete] var kindsInitialized = false // theory: not needed anymore

  /**
   * This function should be called from user-defined patterns as the first thing
   * to initialize symbols defined with one of the functions `event`, `fact` or `symbol`.
   * It only works correctly, however, if all the PC arguments to the pattern are evaluated
   * after the call of this function.
   */

  def initialize() {
    val fields = this.getClass.getDeclaredFields
    for (field <- fields) {
      field.setAccessible(true)
      if (field.get(this) == UNDEFINED_SYMBOL)
        field.set(this, Symbol(field.getName))
    }
  }

  // ===========
  // Conditions:
  // ===========

  // private[rete] val nameOfPosition: Map[Int, Symbol] = positionalArguments

  /**
   * Mapping from integers to symbols: Map(1 -> 'one, 2 -> 'two, 3 -> 'three, ...).
   * Used for representing positional arguments in facts. Can be used when creating
   * such fact maps manually, for example when reading events from a file.
   */
  val nameOfPosition: Map[Int, Symbol] = positionalArguments

  private val dontCare: Map[Int, Symbol] =
    Map(1 -> '_1, 2 -> '_2, 3 -> '_3,
      4 -> '_4, 5 -> '_5, 6 -> '_6,
      7 -> '_7, 8 -> '_8, 9 -> '_9,
      10 -> '_10, 11 -> '_11, 12 -> '_12,
      13 -> '_13, 14 -> '_14, 15 -> '_15,
      16 -> '_16, 17 -> '_17, 18 -> '_18,
      19 -> '_19, 20 -> '_20)

  private var dontCareCounter = 0

  /**
   * Symbol that will match any kind of symbol occurring as <code>'kind</code> symbol in
   * an event or fact. For example <code>ANY()</code> will match any event or fact.
   * In case the condition has parameters, as in <code>ANY('x,'y)</code> the event or fact that it is matched
   * against will only match if it has at least two parameters. Note that
   * <code>ANY(... some pattern ...)</code> will match any events or facts, not just events.
   */

  val ANY = 'ANY

  /**
   * Lifts a kind-name to an object allowing to define the arguments to a condition on the left-hand side
   * of a rule or a fact on the right-hand side of a rule. For example, Consider the following condition
   * on the left-hand side of a rule:
   *
   * <code>
   * "my rule" -- 'COMMAND('name,'number) |-> ...
   * </code>
   *
   * The implicit conversion will here lift <code>'COMMAND</code> to an instance of the form:
   * <code>new ConditionArguments('COMMAND)</code>.
   *
   * @param kind the event/fact kind to be lifted.
   * @return the object allowing to define the arguments to the condition/fact.
   */

  implicit def kindToConditionArguments(kind: Symbol) = new ConditionArguments(kind)

  /**
   * The class defines an <code>apply</code> method, which processes the arguments provided to
   * it, together with the kind, to form a condition/fact. See the implicit function
   * <code>kindToConditionArguments</code>.
   *
   * @param kind the kind of the condition/fact.
   */

  class ConditionArguments(kind: Symbol) {
    if (kind == UNDEFINED_SYMBOL)
      throw new SyntaxException("Event or fact kind defined in class different from monitor with call to 'event' or 'fact'")

    /**
     * This function processes the arguments provided to it, together with the kind,
     * to form a condition/fact. See the implicit function
     * <code>kindToConditionArguments</code>.
     *
     * @param args the arguments to the condition/fact.
     * @return the condition/fact.
     */

    def apply(args: Any*): PC = {
      def tag(value: Any): Pattern =
        value match {
          case '_ =>
            dontCareCounter += 1
            Variable(dontCare(dontCareCounter))
          case variable: Symbol => Variable(variable)
          case _                => Constant(value)
        }
      val arguments = args.toList
      var position: Int = 1
      var constraints: List[(Symbol, Pattern)] = Nil
      for (argument <- arguments) {
        constraints +:=
          (argument match {
            case (field: Symbol, value) => (field -> tag(value))
            case _                      => (nameOfPosition(position) -> tag(argument))
          })
        position += 1
      }
      if (kind == ANY)
        PC(constraints.toMap)
      else
        PC(constraints.toMap + ('kind -> Constant(kind)))
    }
  }

  /**
   * Negates one or more conditions. For example <code>not('Commanded('name))</code>
   * represents the condition that <code>'name</code> has not been commanded. When applied
   * to more than one condition, as in <code>not(c1,c2,...,cn)</code>, it means a negation
   * of the conjunction of conditions.
   *
   * @param conditions the conditions, the conjunction of which will be negated.
   * @return the negated condition.
   */

  protected def not(conditions: Condition*): Condition = {
    val conditionList = conditions.toList
    assert(conditionList.length > 0)
    conditionList match {
      case PC(constraints, label) :: Nil =>
        assert(label == null)
        NC(constraints)
      case _ => NCC(conditionList)
    }
  }

  // ==========================================
  // Using Case Classes as Positive Conditions:
  // ==========================================

  /**
   * The extension of a type <code>T</code> with variable patterns of type <code>Symbol</code>,
   * such as <code>'s</code>, <code>'name</code>, etc. This type is used to define parameters
   * to case classes representing observations (extending class <code>Obs</code>), which then can
   * be used as events and facts in rules.
   */

  type +[T] = Either[T, Symbol]

  /**
   * Lifts a value of type <code>T</code> to <code>Either[T,Symbol]</code>, also containing variables of
   * type <code>Symbol</code>. Used to define arguments for case classes representing events and facts.
   */

  implicit def convValueToEither[T](x: T): Either[T, Symbol] = Left(x)

  /**
   * Lifts a value of type <code>Symbol</code> to <code>Either[T,Symbol]</code>, thereby representing a variable
   * of type <code>T</code>. Used in rules to define patterns for arguments of case class instantiations representing events and
   * facts.
   */

  implicit def convSymbolToEither[T](x: Symbol): Either[T, Symbol] = Right(x)

  /**
   * Type of ''observations'', representing events as well as facts defined as case classes.
   */

  trait Obs

  /**
   * Lifts an instance of a (case) class sub-classing <code>Obs</code> to positive condition of type <code>PC</code>.
   * The lifting  essentially converts the object to the internal format for a condition as used by LogFire.
   */

  implicit def convObsToPC2(obs: Obs): PC = {
    val className = obs.getClass().getSimpleName().stripFromDollar
    val obsName = Symbol(className)
    val classFields = obs.getClass().getDeclaredFields()
    var argList: List[(Symbol, Any)] = List()
    for (classField <- classFields) {
      classField.setAccessible(true)
      val fieldName = classField.getName()
      val value = classField.get(obs)
      if (!fieldName.contains("$") && value != null) {
        val valueEmbedded = value match {
          case Left(x)   => x
          case Right(x)  => x
          case _: Symbol => value
        }
        argList :+= (Symbol(fieldName) -> valueEmbedded)
      }
    }
    obsName(argList: _*)
  }

  // ======
  // Rules:
  // ======

  /**
   * Lifts a rule name to an object that allows to define a rule with that name.
   *
   * @param name the name of the rule to be defined.
   * @return object providing methods that permit to define the rule.
   */

  implicit def nameToRuleDefinition(name: String) = {
    if (!kindsInitialized) {
      initialize()
      kindsInitialized = true
    }
    new RuleDefinition(name)
  }

  /**
   * This class provides an method that when applied to a condition, the first condition
   * in a rule, returns an object allowing to define the complete rule with the remaining
   * conditions and action.
   *
   * @param name the name of the rule being defined.
   */

  class RuleDefinition(name: String) {
    private def when(c: Condition) = new RuleConditions(name, List(c))

    /**
     * When applied to a condition, this method returns a
     * <code>RuleDefinition</code> object, which allows to complete the named rule
     * with more conditions and a final action.
     *
     * @param c the first condition of the rule.
     * @return an object with methods for definning the subsequent conditions and the final
     *         action.
     */

    def --(c: Condition) = when(c)
  }

  /**
   * This class offers methods for defining a rule in terms of its conditions (left-hand side)
   * and a final action (right-hand side).
   *
   * @param name the name of the rule.
   * @param conditions the conditions collected sofar as the left-hand side of the rule
   *                   is processed.
   */

  class RuleConditions(name: String, conditions: List[Condition]) {
    /**
     * This method <i>ands</i> (Boolean 'and') another condition to the left-hand side
     * of a rule.
     *
     * @param c the condition to be <i>anded</i>.
     * @return object that allows to define the remaining part of the rule.
     */

    def &(c: Condition) = and(c)

    /**
     * This method creates a reference to the condition entered immediately to the left. The reference is
     * a symbol that can be referred to on the right-hand side of the rule to remove the fact
     * associated with the condition.
     *
     * @param label the label associated with the fact that the condition matches.
     * @return object that allows to define the remaining part of the rule.
     */

    def ==(label: Symbol) = as(label)

    /**
     * This method defines the transition from left-hand side to right-hand side of a rule.
     * It takes as argument a sequence of actions to be executed in case the left-hand side
     * can be completely matched. Note that it is also possible to write a Scala statement
     * on the right-hand side, see the other version of this method. However, using actions
     * may be faster when there are many symbols bound on the left-hand side, but only some
     * of these are used up on the right-hand side.
     *
     * @param actions the actions to be executed in case the left-hand side can be completely
     *                matched.
     */

    def |->(actions: Action*) {
      thenDoActions(actions: _*)
    }

    /**
     * This method defines the transition from left-hand side to right-hand side of a rule.
     * It takes as argument a positive condition, representing a fact, to be added to the fact memory
     * in case the left-hand side can be completely matched. Note that it is also possible to write a
     * Scala statement on the right-hand side, see the other version of this method. However, using positive
     * conditions may be faster when there are many symbols bound on the left-hand side, but only some
     * of these are used up on the right-hand side. The same holds for actions.
     *
     * @param pc the positive condition representing a fact to be added in case the left-hand side can be
     * completely matched.
     */

    def |->(pc: PC) {
      thenDoActions(add(pc))
    }

    /**
     * This method defines the transition from left-hand side to right-hand side of a rule.
     * It takes as argument a statement to be executed in case the left-hand side
     * can be completely matched.
     *
     * @param stmt the statement to be executed in case the left-hand side can be completely
     *             matched.
     */
    def |->(stmt: => Any) {
      thenDoActions(code(stmt))
    }

    private def and(c: Condition) = new RuleConditions(name, c :: conditions)

    private def as(label: Symbol): RuleConditions = {
      conditions match {
        case PC(constraints, oldLabel) :: rest =>
          assert(oldLabel == null)
          new RuleConditions(name, PC(constraints, label) :: rest)
        case _ =>
          throw new SyntaxException("attempt to label composite condition")
      }
    }

    private def thenDoActions(actions: Action*) {
      var actionSofar: Action = null
      for (action <- actions) {
        if (actionSofar == null)
          actionSofar = action
        else
          actionSofar = And(actionSofar, action)
      }
      addProduction(Production(name, conditions.reverse, actionSofar))
    }
  }

  // ========
  // Actions:
  // ========

  /**
   * Creates action that prints a message.
   *
   * @param string the message to be printed.
   * @return the print action.
   */

  protected def print(string: String): Action = Print(string)

  /**
   * Creates an error action that prints a standard error message.
   *
   * @return the error action.
   */

  protected def error: Action = code {
    fail()
  } // Print(red("*** error")) -- COST

  /**
   * Creates an error action that prints a user provided error message.
   *
   * @param msg the error message to be printed.
   * @return the error action.
   */

  protected def error(msg: String): Action = code {
    fail(msg)
  } // Print(red("*** error: " + msg)) -- COST

  /**
   * Creates a code action carrying a piece of code to be executed.
   *
   * @param stmt the code to be executed.
   * @return the code action.
   */

  protected def code(stmt: => Any): Action = Code((x: Unit) => stmt)

  /**
   * Creates an action that adds a fact to the fact memory.
   *
   * @param pc the fact to be added.
   * @return the add action.
   */

  protected def add(pc: PC): Action = Add(pc.constraints)

  /**
   * Creates an action that removes a fact from the fact memory. The fact is referenced by
   * a label that can have been introduced explicitly in a rule left-hand side as follows:
   *
   * <code>... 'SomeFact(...) == 'l ... |-> ...</code>
   *
   * In this case it can be removed by: <code>rem('l)</code>.
   * Alternatively, if a fact of a specific kind only occurs once on a rule left-hand side,
   * then it can be removed by just referring to the name of the fact. For example if the left-hand
   * side contains:
   *
   * <code>... 'SomeFact(...) ... |-> ...</code>
   *
   * then it can be removed by the action: <code>rem('SomeFact)</code>
   *
   * @param label the label referencing the fact to be removed.
   * @return the remove action.
   */

  protected def rem(label: Symbol): Action = Rem(label)

  //protected def del(constraints: (Symbol, Pattern)*): Action = Del(constraints.toMap)

  /**
   * Creates an action that adds a fact to the fact memory. The positive
   * condition <code>pc</code> is by this implicit conversion transformed
   * to <code>add(pc)</code>. This implicit conversion allows us for example
   * to write a rule of the form:
   *
   * <code>
   * "my rule" -- 'COMMAND('name) |-> 'Commanded('name)
   * </code>
   *
   * to mean that if a <code>'COMMAND('name)</code> event is observed, then the fact
   * <code>'Commanded('name)</code> is added to the fact memory. This rule is equivalent to:
   *
   * <code>
   * "my rule" -- 'COMMAND('name) |-> add('Commanded('name))
   * </code>
   *
   * @param pc The fact to be added.
   * @return the add action.
   */

  implicit def pcToAction(pc: PC): Action = add(pc)

  // ================================
  // Adding rules as data structures:
  // ================================

  /**
   * This method adds a rule to the monitor.
   *
   * Consider for example the following monitor expressing that a started command
   * must be stopped before it can be started again, using normal DSL syntax:
   *
   * {{{
   * class OneCommand extends Monitor {
   *   val EVR = event
   *   val Commanded = fact
   *
   *   "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
   *   "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
   *   "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)
   * }
   * }}}
   *
   * This monitor can instead be expressed as follows using the addRule method:
   *
   * {{{
   * class OneCommand extends Monitor {
   *   // "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
   *   addRule(
   *     "r1",
   *     List(
   *       (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
   *       (false, 'Commanded, Map('name -> 'n), null)),
   *     List(
   *       ('Commanded, Map('name -> 'n))),
   *     Nil)
   *
   *   // "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
   *   addRule(
   *     "r2",
   *     List(
   *       (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
   *       (true, 'Commanded, Map('name -> 'n), null)),
   *     Nil,
   *     Nil,
   *     "two commands")
   *
   *   // "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)
   *   addRule(
   *     "r3",
   *     List(
   *       (true, 'EVR, Map('name -> 'n, 'message -> "STOP"), null),
   *       (true, 'Commanded, Map('name -> 'n), 'l)),
   *     Nil,
   *     List('l))
   * }
   * }}}
   *
   * @param name the name of the rule.
   * @param conditions the left hand-side conditions. Each condition is of the form
   *        (positive,kind,constraints,label)
   *        where positive=true  if there should exist a fact matching the constraints,
   *        and   positive=false if there should not exist a fact matching the constraints;
   *        where kind is the name of the fact,
   *        where constraints map symbols (fields) to either symbols to be bound, or values to match exact;
   *        and where label is a reference to the fact, which can be referred to if removing the fact
   *        on the right hand side in an action (can be null if not needed).
   * @param add a list of facts to be added. Each fact has the form (kind, constraints),
   *        where kind is the name of the fact, and
   *        where constraints map symbols (fields) to either symbols that must have been bound, or values.
   * @param rem a list of facts to be removed, where a fact is identified by its associated label
   *        on the left-hand side.
   * @param errorText a text indicating an error, which will be reported. Default value is null inducating
   *        no error will be reported.
   */

  def addRule(
    name: String,
    conditions: List[(Boolean, Symbol, Map[Symbol, Any], Symbol)],
    add: List[(Symbol, Map[Symbol, Any])],
    rem: List[Symbol],
    errorText: String = null) {
    val convertedConditions: List[Condition] = conditions map {
      case (positive, kind, constraints, label) =>
        val wfConstraints = convertConstraints(constraints) + ('kind -> Constant(kind))
        if (positive) PC(wfConstraints, label) else NC(wfConstraints)
    }
    val convertedAddActions = add map {
      case (kind, constraints) => Add(convertConstraints(constraints) + ('kind -> Constant(kind)))
    }
    val convertedRemActions = rem map {
      case symb => Rem(symb)
    }
    var convertedActions: Action = null
    for (action <- (convertedAddActions ++ convertedRemActions)) {
      if (convertedActions == null)
        convertedActions = action
      else
        convertedActions = And(convertedActions, action)
    }
    if (errorText != null) {
      val errorAction: Action = error(errorText)
      if (convertedActions == null)
        convertedActions = errorAction
      else
        convertedActions = And(convertedActions, errorAction)
    }
    addProduction(Production(name, convertedConditions, convertedActions))
  }

  /**
   * This method adds a rule to the monitor. The action is a lambda-abstraction, allowing
   * any Scala code to be executed in case the rule fires. This allows to access the values
   * of matched identifiers.
   *
   * Consider for example the following monitor expressing that a started command
   * must be stopped before it can be started again, using normal DSL syntax:
   *
   * {{{
   * class OneCommand extends Monitor {
   *   val EVR = event
   *   val Commanded = fact
   *
   *   "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
   *   "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
   *   "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)
   * }
   * }}}
   *
   * This monitor can instead be expressed as follows using the addRule method:
   *
   * {{{
   * class OneCommand extends Monitor {
   *   // "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('name -> 'n)) |-> Commanded('name -> 'n)
   *   addRule(
   *     "r1",
   *     List(
   *       (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
   *       (false, 'Commanded, Map('name -> 'n), null)),
   *     {
   *       case _ =>
   *         insert('Commanded('name -> 'n))
   *     })
   *
   *   // "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('name -> 'n) |-> error("two commands")
   *   addRule(
   *     "r2",
   *     List(
   *       (true, 'EVR, Map('name -> 'n, 'message -> "START"), null),
   *       (true, 'Commanded, Map('name -> 'n), null)),
   *     {
   *       case _ =>
   *         fail(s"two ${'n.s} commands") // note access to value of 'n
   *     })
   *
   *   // "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('name -> 'n) |-> remove('Commanded)
   *   addRule(
   *     "r3",
   *     List(
   *       (true, 'EVR, Map('name -> 'n, 'message -> "STOP"), null),
   *       (true, 'Commanded, Map('name -> 'n), 'l)),
   *     {
   *       case _ =>
   *         remove('l)
   *     })
   * }
   * }}}
   *
   * @param name the name of the rule.
   * @param conditions the left hand-side conditions. Each condition is of the form
   *        (positive,kind,constraints,label)
   *        where positive=true  if there should exist a fact matching the constraints,
   *        and   positive=false if there should not exist a fact matching the constraints;
   *        where kind is the name of the fact,
   *        where constraints map symbols (fields) to either symbols to be bound, or values to match exact;
   *        and where label is a reference to the fact, which can be referred to if removing the fact
   *        on the right hand side in an action (can be null if not needed).
   * @param action the action (a lambda abstraction) to be executed in case the left-hand side
   *        matches.
   */

  def addRule(
    name: String,
    conditions: List[(Boolean, Symbol, Map[Symbol, Any], Symbol)],
    action: Unit => Any) {
    val convertedConditions: List[Condition] = conditions map {
      case (positive, kind, constraints, label) =>
        val wfConstraints = convertConstraints(constraints) + ('kind -> Constant(kind))
        if (positive) PC(wfConstraints, label) else NC(wfConstraints)
    }
    val convertedAction: Action = Code(action)
    addProduction(Production(name, convertedConditions, convertedAction))
  }

  private def convertConstraints(constraints: Map[Symbol, Any]): Map[Symbol, Pattern] = {
    constraints map {
      case (symb, any) =>
        val pattern: Pattern = any match {
          case symbol: Symbol => Variable(any.asInstanceOf[Symbol])
          case _              => Constant(any)
        }
        (symb -> pattern)
    }
  }

  // =========
  // Printing:
  // =========

  private[rete] def printEvent(wme: Wme) {
    println()
    println("--- " + wme.toStringSimple + " ---")
  }

  private[rete] def printFact(wme: Wme) {
    println()
    println("=== " + wme.toStringSimple + " ===")
  }

  private[rete] def printFacts() {
    println("")
    println("--- facts: ---")
    println(toStringWmes)
    println("--------------")
  }

  // ==========================
  // Commands from within code:
  // ==========================

  /**
   * Inserts fact into the fact memory.
   *
   * @param pc the fact to be inserted.
   */

  def insert(pc: PC) {
    val fields = for ((field, pattern) <- pc.constraints) yield {
      val value = pattern match {
        case Variable(x) => get[Any](x)
        case Constant(v) => v
      }
      (field -> value)
    }
    val wme = Wme(fields)
    if (HISTORY) wme.history =
      computeHistory(executingEventNumber, executingEvent, executingRule, executingToken, fields)
    addWme(wme)
  }

  /**
   * Removes a fact from the fact memory. The fact is referenced by
   * a label that can have been introduced explicitly in a rule left-hand side as follows:
   *
   * <code>... 'SomeFact(...) == 'l ... |-> ...</code>
   *
   * In this case it can be removed by: <code>remove('l)</code>.
   * Alternatively, if a fact of a specific kind only occurs once on a rule left-hand side,
   * then it can be removed by just referring to the name of the fact. For example if the left-hand
   * side contains:
   *
   * <code>... 'SomeFact(...) ... |-> ...</code>
   *
   * then it can be removed by the action: <code>remove('SomeFact)</code>
   *
   * @param label the label referencing the fact to be removed.
   */

  def remove(label: Symbol) {
    val index = executingTokenIndex(label)
    val Token(_, wme) = executingToken.getIthToken(index)
    wme.remove()
  }

  /**
   * Updates a fact with new parameters. For example:
   *
   * <code>update('Counter(...))</code>
   *
   * is equivalent to:
   *
   * <code>remove('Counter);insert(Counter(...))</code>
   *
   * @param pc the new fact to be added, updating the old fact.
   */

  def update(pc: PC) {
    val label: Symbol = (pc.constraints.get('kind): @unchecked) match {
      case Some(Constant(symbol)) => symbol.asInstanceOf[Symbol]
    }
    val index = executingTokenIndex(label)
    val Token(_, wme) = executingToken.getIthToken(index)
    wme.remove()
    insert(pc)
  }

  /**
   * Replaces a fact of one kind with a fact of a different kind. For example,
   *
   * <code>replace('Commanded)('Succeeded('cmdName))</code>
   *
   * is equivalent to:
   *
   * <code>remove('Commanded);insert('Succeeded('cmdName))</code>
   *
   * @param label reference to the fact to be removed.
   * @param pc the new fact to be added.
   */

  def replace(label: Symbol)(pc: PC) {
    val index = executingTokenIndex(label)
    val Token(_, wme) = executingToken.getIthToken(index)
    wme.remove()
    insert(pc)
  }

  /**
   * Checks the truth of a condition and reports an error if it is false.
   * The method should be called inside a code action.
   *
   * @param condition the condition to be checked.
   */

  def ensure(condition: Boolean) {
    if (!condition) {
      fail()
    }
  }

  /**
   * Checks the truth of a condition and reports an error if it is false.
   * The provided text is printed followed by the text <code>- failed</code>.
   * Hence the text is expected to describe the property that should hold (and not
   * an error message).
   *
   * @param text the text to be printed.
   * @param condition the condition to be checked.
   */

  def ensure(text: String)(condition: Boolean) {
    if (!condition) {
      fail("[" + text + "] : failed!")
    }
  }

  /**
   * Checks the truth of a condition and reports an error if it is true.
   * The method should be called inside a code action.
   *
   * @param condition the condition to be checked.
   */

  def avoid(condition: Boolean) {
    if (condition) {
      fail()
    }
  }

  /**
   * Checks the truth of a condition and reports an error if it is true.
   * The provided text is printed followed by the text <code>- became true</code>.
   * Hence the text is expected to describe the property that should not hold (and not
   * an error message).
   *
   * @param text the text to be printed.
   * @param condition the condition to be checked.
   */

  def avoid(text: String)(condition: Boolean) {
    if (condition) {
      fail("[" + text + "] : became true!")
    }
  }

  /**
   * Reports a result on standard out, and stores it in the `MonitorResult`
   * object associated with the monitor. This `MonitorResult` object can at any
   * time be accessed with the method `getMonitorResult`.
   *
   * @param msg
   */

  def report(msg: String) {
    val binding = Map('kind -> 'Fail, 'one -> msg)
    val history = computeHistory(executingEventNumber, executingEvent, executingRule, executingToken, binding, msg)
    monitorResult.putHistory(history)
    if (PRINT_PROGRESS) println(history)
  }

  /**
   * Reports an error, printing the provided error message. The method should be called inside a code action.
   *
   * @param msg the error message to be printed.
   */

  def fail(msg: String = "") {
    val message = "ERROR" + (if (msg == "") "" else " " + msg)
    report(message)
    if (STOP_AFTER_FAIL) {
      println()
      println(s"Processed $executingEventNumber events.")
      System.exit(0)
    }
  }

  /**
   * Returns the entire argument map associated with an event or fact.
   * The event/fact is referenced by a label that can have been introduced
   * explicitly in a rule left-hand side as follows:
   *
   * <code>... 'SomeFact(...) == 'l ... |-> ...</code>
   *
   * In this case the argument map is obtained by: <code>getArgs('l)</code>.
   * Alternatively, if an event/fact of a specific kind only occurs once on a rule left-hand side
   * (which is always the case for events), then the name of the event/fact can be used as label.
   * For example if the left-hand side contains:
   *
   * <code>... 'SomeFact(...) ... |-> ...</code>
   *
   * then the map can be obtained by: <code>getArgs('SomeFact)</code>
   *
   * @param label the label referencing the event/fact.
   * @return the argument map of that event/fact.
   */

  def getArgs(label: Symbol): Binding = {
    val index = executingTokenIndex(label)
    val Token(_, wme) = executingToken.getIthToken(index)
    wme.fields
  }

  /**
   * The type of the argument to an event or fact: a mapping from fields to their values.
   */

  type Binding = Map[Symbol, Any]
}

/**
 * This class offers all of the main features of LogFire. See the package tutorial for a description of its use.
 * It extends the classes <code>Options</code> - offering
 * the user modifiable options - and <code>Logic</code>  - offering the DSL for writing rules. In addition
 * this class offers methods for adding events and facts to the monitor state, as well as removing facts.
 * Furthermore if offers methods for writing (serialize) the fact memory to permanent store and for loading facts from
 * such permanent store.
 */

class Monitor extends Logic {

  import Serialization._

  private def convAnyToMap(obs: Any): Map[Symbol, Any] = {
    val className = obs.getClass().getSimpleName().stripFromDollar
    val classFields = obs.getClass().getDeclaredFields()
    var map: Map[Symbol, Any] = Map('kind -> Symbol(className))
    for (classField <- classFields) {
      classField.setAccessible(true)
      val value = classField.get(obs)
      val fieldName = classField.getName()
      if (!fieldName.contains("$")) {
        map += (Symbol(fieldName) -> value)
      }
    }
    map
  }

  // ==============
  // Adding events:
  // ==============

  /**
   * Submits an event to the monitor. The event is represented as a map.
   *
   * @param map the map representing the event.
   */

  def addMapEvent(map: Map[Symbol, Any]) {
    val wme = Wme(map)
    if (PRINT || PRINT_EVENTS) printEvent(wme)
    executingEventNumber += 1 // COST
    executingEvent = wme // COST
    addWme(wme)
    removeWme(wme)
    while (!isEmpty) {
      executeActions()
    }
    if (PRINT || PRINT_FACTS) printFacts()
  }

  /**
   * Submits an event to the monitor. The event is represented as a list of field-value pairs,
   * in essence representing a map from fields to values.
   *
   * @param fields the field-value pairs representing the map.
   */

  def addMapEvent(fields: (Symbol, Any)*) {
    addMapEvent(fields.toMap)
  }

  /**
   * Submits an event to the monitor. The event is represented by a kind and then a list
   * of field-value pairs, in essence representing a map from fields to values.
   *
   * @param kind the event kind.
   * @param fields the field-value pairs representing the contents of the event.
   */

  def addMapEvent(kind: Symbol)(fields: (Symbol, Any)*) {
    addMapEvent(fields.toMap + ('kind -> kind))
  }

  /**
   * Submits an event to the monitor. The event is represented by a kind and then a list
   * of values, representing an event with positional (un-named) parameters.
   *
   * @param kind the event kind.
   * @param values the arguments of the event.
   */

  def addEvent(kind: Symbol)(values: Any*) {
    var position: Int = 0
    var map: Map[Symbol, Any] = Map('kind -> kind)
    for (value <- values) {
      position += 1
      map += (nameOfPosition(position) -> value)
    }
    addMapEvent(map)
  }

  /**
   * Submits an event represented as an object to the monitor. The object is converted into a map of
   * type <code>Map[Symbol,Any]</code>, mapping object field names (converted to symbols) to the values
   * they denote in the object.
   *
   * @param obj the object representing an event.
   */

  def addObjEvent(obj: Any) {
    addMapEvent(convAnyToMap(obj))
  }

  /**
   * Submits a CSV record read by a CSV reader. Fields of the record are extracted
   * and an event is emitted to the monitor with a call of <code>addMapEvent</code>.
   * As soon as a field is empty, as in <code>open,1,,</code> the argument collection 
   * terminates on that single event line, and the reader moves on to the next line.  
   * This means that for example in <code>open,1,,2</code> the <code>2</code> will 
   * be ignored. The user has to make sure that this situation does not occur.
   * 
   * @param record the CSV record to be submitted.
   */
  
  def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map();
    map += 'kind -> Symbol(record.get(0))
    breakable {
      for (i <- 1 until record.size()) {
        val value = record.get(i)
        if (value == "") break
        map += nameOfPosition(i) -> value
      }
    }
    addMapEvent(map);
  }  
  
  // ==========================
  // Adding and removing facts:
  // ==========================

  /**
   * Adds a fact to the fact memory. The fact is represented by a map.
   *
   * @param map the map representing the fact.
   * @return a reference to the fact that has just been added. This reference can be used to
   *         later remove the fact.
   */

  def addMapFact(map: Map[Symbol, Any]): Fact = {
    val wme = Wme(map)
    if (PRINT || PRINT_EVENTS) printFact(wme)
    addWme(wme)
    while (!isEmpty) {
      executeActions()
    }
    if (PRINT || PRINT_FACTS) printFacts()
    wme
  }

  /**
   * Adds a fact to the fact memory. The fact is represented as a list of field-value pairs,
   * in essence representing a map from fields to values.
   *
   * @param fields the field-value pairs representing the map.
   * @return a reference to the fact that has just been added. This reference can be used to
   *         later remove the fact.
   */

  def addMapFact(fields: (Symbol, Any)*): Fact = addMapFact(fields.toMap)

  /**
   * Adds a fact to the fact memory. The fact is represented by a kind and then a list
   * of field-value pairs, in essence representing a map from fields to values.
   *
   * @param kind the fact kind.
   * @param fields the field-value pairs representing the contents of the fact.
   * @return a reference to the fact that has just been added. This reference can be used to
   *         later remove the fact.
   */

  def addMapFact(kind: Symbol)(fields: (Symbol, Any)*): Fact =
    addMapFact(fields.toMap + ('kind -> kind))

  /**
   * Adds a fact to the fact memory. The fact is represented by a kind and then a list
   * of values, representing a fact with positional (un-named) parameters.
   *
   * @param kind the fact kind.
   * @param values the arguments of the fact.
   * @return a reference to the fact that has just been added. This reference can be used to
   *         later remove the fact.
   */

  def addFact(kind: Symbol)(values: Any*): Fact = {
    var position: Int = 0
    var map: Map[Symbol, Any] = Map('kind -> kind)
    for (value <- values) {
      position += 1
      map += (nameOfPosition(position) -> value)
    }
    addMapFact(map)
  }

  /**
   * Submits a fact represented as an object to the monitor. The object is converted into a map of
   * type <code>Map[Symbol,Any]</code>, mapping object field names (converted to symbols) to the values
   * they denote in the object.
   *
   * @param obj the object representing a fact.
   */

  def addObjFact(obj: Any) {
    addMapFact(convAnyToMap(obj))
  }

  /**
   * Deletes all facts in the fact memory that satisfy a predicate (which has a default value
   * true on all facts).
   *
   * @param pred the predicate, which has a default value mapping all facts to true.
   *             Any fact <i>F</i> for which <code>pred(</code><i>F</i><code>)</code>
   *             is true will be deleted.
   */

  def clearFacts(pred: Binding => Boolean = (m => true)) {
    if (PRINT_PROGRESS) println("\nClearing facts")
    for (wme <- getAllWmes if pred(wme.fields)) {
      wme.remove()
    }
    if (PRINT || PRINT_FACTS) printFacts()
  }

  /**
   * Extracts all facts from the fact memory that satisfies a predicate and returns them
   * as a set of bindings. This method can be useful for examining the contents of the
   * fact memory.
   *
   * @param pred the filtering predicate, having the default value true on all facts.
   * @return the set of facts collected from the fact memory, presented as bindings.
   */

  def getMaps(pred: Binding => Boolean = (m => true)): Set[Binding] =
    for (wme <- getAllWmes if pred(wme.fields)) yield wme.fields

  /**
   * Prints a binding in a readable format reflecting that it is a fact.
   *
   * @param binding the binding to be printed.
   */

  def printMap(binding: Binding) {
    println(PP.bindingToString(binding))
  }

  // ============
  // Terminating:
  // ============

  /**
   * The end symbol, which is emitted on a call of <code>terminate()</code>.
   * The symbol can be used on the left-hand side of rules, which then may get
   * triggered on the termination of monitoring, depending on what other conditions
   * occur on the rule left-hand side.
   */

  val END = 'END

  private var hotFacts: Set[Symbol] = Set()

  /**
   * Defines a sequence of fact names as hot. When the monitor is terminated
   * with the <code>terminate</code> function, an error message is issued for
   * each hot fact remaining in fact memory. The function must not be be called
   * before the first rule declaration in the monitor. Good style is to call
   * it after all the rule declarations.
   *
   * @param symbols names of hot facts.
   */

  def hot(symbols: Symbol*) {
    assert(kindsInitialized, "hot function must be called after first rule declaration")
    for (symb <- symbols) {
      "end" -- symb() & END() |-> fail("hot fact remains")
    }
  }

  /**
   * Terminates monitoring. The function should only be called at the end of a monitoring
   * session if some facts have been declared hot with the <code>hot</code> function.
   * Any hot fact remaining in fact memory upon termination will be reported as an error.
   */

  def terminate() {
    println("\nterminating and computing result!\n")
    addEvent(END)()
    if (!monitorResult.errorsReported) {
      println()
      println("===================")
      println("Property Satisfied!")
      println("===================")
      println()
      println(s"Processed $executingEventNumber events.")
    }
  }

  // ==============
  // Serialization:
  // ==============

  /**
   * Writes facts from fact memory, filtered by a predicate, to a permanent file
   * for later use. This file can at a later stage be be re-loaded into a fact memory
   * with the <code>readFacts</code> function, or can simply be read by the <code>getFacts</code>
   * function.
   *
   * @param file the file to write the facts to.
   * @param pred the predicate filtering the facts to be written. Only facts <i>F</i>
   *             for which <code>pred(</code><i>F</i><code>)</code> is true are written to the file.
   *             The predicate is by default true for all facts.
   */

  def writeFacts(file: String, pred: Binding => Boolean = (m => true)) {
    val database = new Database
    var counter = 0
    for (wme <- getAllWmes if pred(wme.fields)) {
      database.addFact(wme.fields)
      counter += 1
    }
    serialize(file, database)
    if (PRINT_PROGRESS) println(s"\nWrote $counter facts to file: $file")
  }

  /**
   * Loads facts filtered by a predicate into the fact memory from a permanent
   * file to which they have previously been written with a call of
   * <code>writeFacts</code>.
   *
   * @param file the file from which to load the facts.
   * @param pred the filtering predicate, which by default is true for all facts. Only facts <i>F</i>
   *             for which <code>pred(</code><i>F</i><code>)</code> is true are loaded.
   */

  def readFacts(file: String, pred: Binding => Boolean = (m => true)) {
    if (PRINT_PROGRESS) println(s"\nLoading facts from file: $file")
    val database: Database = unserialize(file).asInstanceOf[Database]
    var counter = 0
    for (fact <- database.getFacts if pred(fact)) {
      addMapFact(fact)
      counter += 1
    }
    if (PRINT_PROGRESS) println(s"Read $counter facts")
  }

  /**
   * Reads facts from a permanent file to which they have previously been written with a call of
   * <code>writeFacts</code>. The facts are not loaded into the fact memory, but are instead returned
   * as a set of bindings.
   *
   * @param file the file from which to read the facts.
   * @return the set of bindings corresponding to the facts read from the file.
   */

  def getFacts(file: String): Set[Binding] = {
    val database: Database = unserialize(file).asInstanceOf[Database]
    database.getFacts
  }
}