import parser._
import interpreterProp._
import propLoc._

object Main {

  def run(exp: String): Unit = {
    runProgram(toPropLoc(parse(exp)))
  }

  def equal(exp1: String, exp2: String): Unit = {
    checkEquivalence(toPropLoc(parse(exp1)), toPropLoc(parse(exp2)))
  }

  def truth(exp: String): Unit = {
    makeTruthTable(toPropLoc(parse(exp)))
  }

  def main(args: Array[String]): Unit = {
    val exp281 = "((a | --b) & (c | a))"
    val exp282 = "(a | (--b & c))"
    val exp283 = "((--a) -> (--b & c))"
    val exp284 = "((--b & c) -> a)"

    equal(exp281, exp282)
    equal(exp282, exp283)
    equal(exp283, exp284)
  }
}
