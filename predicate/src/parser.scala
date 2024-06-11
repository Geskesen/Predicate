object parser {
  sealed trait PropExpr

  case class Var(name: String) extends PropExpr

  case class Not(expr: PropExpr) extends PropExpr

  case class And(left: PropExpr, right: PropExpr) extends PropExpr

  case class Or(left: PropExpr, right: PropExpr) extends PropExpr

  case class Implication(left: PropExpr, right: PropExpr) extends PropExpr

  // Tokenize the input string into a list of tokens
  def tokenize(input: String): List[String] = {
    val pattern = "([()&|]|--|->|[^()&|\\s]+)".r
    pattern.findAllIn(input).toList
  }

  // Parse the input string into a PropExpr
  def parse(input: String): Option[PropExpr] = {
    val tokens = tokenize(input)
    val (expr, _) = parseImplication(tokens)
    expr
  }

  def parseImplication(tokens: List[String]): (Option[PropExpr], List[String]) = {
    val (leftExpr, remainingTokens) = parseOr(tokens)
    remainingTokens match {
      case "->" :: rest =>
        val (rightExpr, finalTokens) = parseImplication(rest)
        (for {
          left <- leftExpr
          right <- rightExpr
        } yield Implication(left, right), finalTokens)
      case _ => (leftExpr, remainingTokens)
    }
  }

  def parseOr(tokens: List[String]): (Option[PropExpr], List[String]) = {
    val (leftExpr, remainingTokens) = parseAnd(tokens)
    remainingTokens match {
      case "|" :: rest =>
        val (rightExpr, finalTokens) = parseOr(rest)
        (for {
          left <- leftExpr
          right <- rightExpr
        } yield Or(left, right), finalTokens)
      case _ => (leftExpr, remainingTokens)
    }
  }

  def parseAnd(tokens: List[String]): (Option[PropExpr], List[String]) = {
    val (leftExpr, remainingTokens) = parsePrimary(tokens)
    remainingTokens match {
      case "&" :: rest =>
        val (rightExpr, finalTokens) = parseAnd(rest)
        (for {
          left <- leftExpr
          right <- rightExpr
        } yield And(left, right), finalTokens)
      case _ => (leftExpr, remainingTokens)
    }
  }

  def parsePrimary(tokens: List[String]): (Option[PropExpr], List[String]) = tokens match {
    case "(" :: rest =>
      val (expr, remainingTokens) = parseImplication(rest)
      remainingTokens match {
        case ")" :: tail => (expr, tail)
        case _ => (None, remainingTokens)
      }
    case "--" :: rest =>
      val (expr, remainingTokens) = parsePrimary(rest)
      (expr.map(Not), remainingTokens)
    case token :: rest =>
      if (token.matches("[a-zA-Z]+"))
        (Some(Var(token)), rest)
      else
        (None, rest)
    case Nil => (None, Nil)
  }
}
