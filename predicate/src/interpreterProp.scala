import propLoc.*

import scala.util.control.Breaks.{break, breakable}

object interpreterProp {
  def getLetters(program: propLoc): Set[String] = {
    program match {
      case and(x, y) => getLetters(x) ++ getLetters(y)
      case or(x, y) => getLetters(x) ++ getLetters(y)
      case neg(x) => getLetters(x)
      case letter(x) => Set(x)
      case imp(x1, x2) => getLetters(x1) ++ getLetters(x2)
    }
  }
  def eval(program: propLoc, value: Map[String, Boolean]): Boolean = {
    program match {
      case and(x, y) => eval(x, value) && eval(y, value)
      case or(x, y) => eval(x, value) || eval(y, value)
      case neg(x) => !eval(x, value)
      case letter(x) => value.getOrElse(x, false)
      case imp(x1, x2) => !eval(x1, value) || eval(x2, value)
    }
  }
  def allCombs(program: propLoc): List[Map[String, Boolean]] = {
    val letters = getLetters(program).toList
    val n = letters.length
    val numCombinations = 1 << n // 2^n combinations

    (0 until numCombinations).map { i =>
      letters.zipWithIndex.map { case (letter, idx) =>
        letter -> ((i & (1 << idx)) != 0)
      }.toMap
    }.toList
  }

  def runProgram(program: propLoc): Unit = {
    val List1 = allCombs(program)
    var satis: Boolean = false
    var valid: Boolean = true
    breakable {
      for (ist1 <- List1) {
        val value1 = eval(program, ist1)
        if (!satis && value1) satis = true
        if (!value1) valid = false
        if (!valid && satis) {
          break() // Exit the loop
        }
      }
    }
    println(s"satisfiable: $satis")
    println(s"valid: $valid")
  }

  def checkEquivalence(program1: propLoc, program2: propLoc): Unit = {
    if(getLetters(program1) != getLetters(program2)) print("not equivalent") else {
      var equivalence = true
      val List1 = allCombs(program1)
      breakable {
        for (ist1 <- List1) {
          val prog1: Boolean = eval(program1, ist1)
          val prog2: Boolean = eval(program2, ist1)
          if(prog1 != prog2)
            equivalence = false
            break()
        }
      }
      if(equivalence) println("is equivalent") else println("not equivalent")
    }
  }

  def makeTruthTable(program: propLoc): Unit = {
    val List1 = allCombs(program)
    val names = getLetters(program)
    for(ist1 <- List1){
      var s: String = ""
      for(n <- names){
        s = s + s"$n: ${ist1.getOrElse(n, false)}, "
      }
      s = s + s"eval to: ${eval(program,ist1)}"
      println(s)
    }
  }
  
  def main(args: Array[String]): Unit = {
    val program = or(letter("A"), letter("B"))
    runProgram(program)
    val program1 = imp(and(neg(letter("b")), letter("c")), letter("a"))
    runProgram(program1)
    val program2 = or(letter("a"), neg(letter("a")))
    runProgram(program2)
    val program3 = or(imp(letter("q"), letter("p")), imp(letter("p"), letter("q")))
    runProgram(program3)
    val program4 = imp(imp(letter("p"), neg(letter("q"))), letter("p"))
    makeTruthTable(program4)
    //opgave 28 i 2021
    val prog1 = and(or(letter("a"), neg(letter("b"))), or(letter("c"), letter("a")))
    val prog2 = or(letter("a"), and(neg(letter("b")), letter("c")))
    val prog3 = imp(neg(letter("a")), and(neg(letter("b")), letter("c")))
    val prog4 = imp(and(neg(letter("b")), letter("c")), letter("a"))
    checkEquivalence(prog1, prog2)
    checkEquivalence(prog2, prog3)
    checkEquivalence(prog3, prog4)
    //opgave 20 i 2021
    val a = letter("a")
    val b = letter("b")
    val c = letter("c")
    val prog201 = and(and(and(or(a, b), neg(a)), neg(c)), imp(b, c))
    val prog202 = imp(and(and(or(a,b), neg(a)), neg(c)), b)
    val prog203 = and(and(or(a,b), neg(a)), imp(neg(c), b))
    val prog204 = imp(and(or(a,b), neg(a)), or(neg(c), b))
    runProgram(prog201)
    runProgram(prog202)
    runProgram(prog203)
    runProgram(prog204)
    val prog205 = imp(imp(letter("p"), letter("q")), imp(imp(letter("p"), letter("r")), imp(letter("q"), letter("r"))))
    runProgram(prog205)

  }
}