import parser.*
import interpreterProp.*
object propLoc {
  sealed abstract class propLoc
  case class and(x: propLoc, y: propLoc) extends propLoc
  case class or(x: propLoc, y: propLoc) extends propLoc
  case class neg(x: propLoc) extends propLoc
  case class letter(x: String) extends propLoc
  case class imp(x1: propLoc, x2: propLoc) extends propLoc

  def toPropLoc(expr: Option[parser.PropExpr]): propLoc = {
    expr match {
      case Some(value) => value match
        case Var(name) => letter(name)
        case Not(expr) => neg(toPropLoc(Some(expr)))
        case And(left, right) => and(toPropLoc(Some(left)), toPropLoc(Some(right)))
        case Or(left, right) => or(toPropLoc(Some(left)), toPropLoc(Some(right)))
        case Implication(left, right) => imp(toPropLoc(Some(left)), toPropLoc(Some(right)))
    case None => throw Exception("error")
    }
  }


  def main(args: Array[String]): Unit = {
    val exp5 = "((p -> q) -> ((p -> r) -> (q -> r)))"
    makeTruthTable(toPropLoc(parse(exp5)))
    println("")
    val exp1 = "((a | b) & --a & --c & (b -> c))"
    runProgram(toPropLoc(parse(exp1)))
    val exp2 = "(((a | b) & --a & --c) -> b)"
    runProgram(toPropLoc(parse(exp2)))
    val exp3 = "((a | b) & --a & (--c -> b))"
    runProgram(toPropLoc(parse(exp3)))
    val exp4 = "(((a | b) & --a) -> (--c | b))"
    runProgram(toPropLoc(parse(exp4)))
    println("")
    val exp281 = "((a | --b) & (c | a))"
    val exp282 = "(a | (--b & c))"
    val exp283 = "((--a) -> (--b & c))"
    val exp284 = "((--b & c) -> a)"
    checkEquivalence(toPropLoc(parse(exp281)), toPropLoc(parse(exp282)))
    checkEquivalence(toPropLoc(parse(exp282)), toPropLoc(parse(exp283)))
    checkEquivalence(toPropLoc(parse(exp283)), toPropLoc(parse(exp284)))

  }
}