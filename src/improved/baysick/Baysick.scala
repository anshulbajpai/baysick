package improved.baysick

import collection.mutable.HashMap
import scala.math._
import sun.org.mozilla.javascript.internal.EvaluatorException


class Baysick {

  val lines = new HashMap[Int,Command]
  val symbols = new HashMap[Symbol,Any]

  type Eval[T] = () => T

  trait Command

  trait ExecutableCommand extends Command {
    def execute : Unit
  }

  class PrintValue(value: Any) extends ExecutableCommand{
    def execute = println(value match {
      case fn:Eval[String] => fn()
      case sym:Symbol => symbols(sym)
      case _ => value.toString
    })
  }

  case class Goto(toLineNumber : Int) extends Command

  class Input(sym : Symbol) extends ExecutableCommand{
    def execute = symbols(sym) = readLine()
  }

  class End(lineNumber : Int) extends ExecutableCommand {
    def execute = {println( "\nBreak at line number " + lineNumber); sys.exit()}
  }

  class Assignment(fn : Eval[Unit]) extends ExecutableCommand {
    def execute = fn()
  }

  class If(fn : Eval[Boolean]) extends ExecutableCommand{
    var branch : () => Unit = null
    def execute = if (fn()) branch()
    def THEN(toLineNumber : Int) = { branch = () => executeLine(toLineNumber) }
  }

  class Appender(left : Any){

    val leftValue = () => left match {
      case s:Symbol => symbols(s)
      case f:Eval[String] => f()
      case _ => left.toString
    }

    def %(right : Any) = () => leftValue() + (right match {
      case s:Symbol =>  symbols(s)
      case f:Eval[Any] => f().toString
      case _ => right.toString
    }).toString
  }

  class MathBuilder(f : Eval[Int]){
    def <=(sym : Symbol) = () => f() <= symbols(sym).toString.toInt
    def >(i : Int) = () => f() > i
    def <(i : Int) = () => f() < i

    def *(i : Int) = () => f() * i
    def +(s : Symbol) = () => f() + symbols(s).toString.toInt
    def +(f2 : Eval[Int]) = () => f() + f2()
    def /(f2 : Eval[Int]) = () => f() / f2()
    def -(f2 : Eval[Int]) = () => f() - f2()
    def -(s : Symbol) = () => f() - symbols(s).toString.toInt
  }

  class SymbolAssigner(sym : Symbol){
    def :=(value : Any) : Eval[Unit] = () => symbols(sym) =  value match {
      case f:Eval[Int] => f()
      case _ => value
    }

    def === (rhs : Any) : Eval[Boolean] = () => {symbols(sym) == rhs}
  }

  class CommandBuilder(lineNumber: Int) {

    def PRINT(value: Any) = lines(lineNumber) =  new PrintValue(value)

    def INPUT(sym : Symbol) = lines(lineNumber) = new Input(sym)

    def LET(f : Eval[Unit]) = lines(lineNumber) = new Assignment(f)

    def GOTO(toLineNumber: Int) = lines(lineNumber) = new Goto(toLineNumber)

    def END = lines(lineNumber) = new End(lineNumber)

    def IF(f : Eval[Boolean]) = {
      val iff = new If(f)
      lines(lineNumber) = iff
      iff
    }

  }
  def RUN = executeLine(10)

  def SQRT(sym : Symbol) = () => sqrt(symbols(sym).toString.toInt)
  def ABS(sym : Symbol) = () => abs(symbols(sym).toString.toInt)

  private def executeLine(lineNumber : Int) : Unit = lines(lineNumber) match {
      case Goto(l) => executeLine(l)
      case c:ExecutableCommand => c.execute; executeLine(lineNumber + 10)
  }

  implicit def lineNumberToCommand(lineNumber : Int) = new CommandBuilder(lineNumber)
  implicit def stringToAppend(init : Any) = new Appender(init)
  implicit def symbolToSymbolAssigner(sym : Symbol) = new SymbolAssigner(sym)
  implicit def symbolToLogical(sym : Symbol) = new MathBuilder(() => symbols(sym).toString.toInt)
  implicit def functionToLogical(v : Eval[Int]) = new MathBuilder(v)
}
