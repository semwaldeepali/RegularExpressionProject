package project.regexparser

import scala.util.parsing.combinator._

/* Binding Strength is the operator's affinity of the characters around it.
  In BODMAS, * Binds more strongly than +.
  It is different that left/ right association

  In regular expressions the order of binding strength is:
  1. Character Literal & parentheses (Highest priority)
  2. + and *  (Middle Priority)
  3. “Concatenation” – a is after b
  4. | (Lowest Priority)

  We have 4 levels of hierarchy. The lowest one would be at THE TOP OF THE TREE. HIGHEST AT THE BOTTOM

  This object PARSES A REGULAR EXPRESSION AND CONVERTS IT INTO A TREE.
  We build the tree from leaves to root.
*/
object RegexParser extends RegexParsers {

  // parsing characters
  // This will match either a character \w or . in the input tsring and convert it into the type Literal
  def charLit: Parser[RegexExpr] = ("""\w""".r | ".") ^^ { ch => assert(ch.length==1); Literal(ch.head) }

  //parsing parenthesis
  // This will match opening and closing parenthesis with expression in between.
  // Since parentheses have strong binding they can enforce higher bindings at lower level also.
  def parenthesisExp: Parser[RegexExpr] = "(" ~> higherExp <~ ")"

  // combining literals at the bottom of the tree which can either be character or parenthesis
  def lit: Parser[RegexExpr] = charLit | parenthesisExp

  // parsing repeat
  // If * comes after lit repeat the lit.

  // TODO : understand case in these definitions. why using them?

  def repeat: Parser[RegexExpr] = lit <~ "*" ^^ { l => Repeat(l) }

  def plus: Parser[RegexExpr] = lit <~ "+" ^^ { p => Plus(p) }

  def lowExp: Parser[RegexExpr] = repeat | plus | lit

  //concatenation for the next level up
  // rep() Keeps matching lowExp and returns the list of them
  def concat: Parser[RegexExpr] = rep(lowExp) ^^ { list => listToConcat(list) }

  // level up to either concat list of lower expressions or single low expression
  def midExpr: Parser[RegexExpr] = concat | lowExp

  // binding multiple mid expressions using or
  def or: Parser[RegexExpr] = midExpr ~ "|" ~ midExpr ^^ { case l ~ "|" ~ r => Or(l, r) }

  // further binding mid level expressions
  // The higher level i.e. closer to root bindings are for or or simply midLevel expressions.
  def higherExp: Parser[RegexExpr] = or | midExpr

  // function to concatenate the list
  def listToConcat(list: List[RegexExpr]): RegexExpr = list match {
    case x :: Nil => x
    case x :: xs => Concat(x, listToConcat(xs))
  }

  // Applying the function to the input
  // by calling highExp method.
  def apply(input: String): Option[RegexExpr] =
    parseAll(higherExp, input) match {
      case Success(matched, _) => Some(matched)
      case _ => None
    }

}

object Main extends App{
  val input = "a*b+(c|d)*"
  println(RegexParser.apply(input))
}



