package project.regexparser

/*
* This class represents the different data types and their hierarchy.
 */
abstract class RegexExpr

// For representing ., a, b
case class Literal(c: Char) extends RegexExpr

//for representing a | b
case class Or( expr1 : RegexExpr, expr2: RegexExpr) extends RegexExpr

// for representing a +
case class Plus(expr: RegexExpr) extends RegexExpr

// for representing a*
case class Repeat(expr: RegexExpr) extends RegexExpr

// for representing combination of different RegexExpr
// ab is represented as Concat(a,b)
// a+b is Concat(Plus(a),b)
// abc is Concat(a, Concat(b,c))
case class Concat( expr1 : RegexExpr, expr2 : RegexExpr) extends RegexExpr


