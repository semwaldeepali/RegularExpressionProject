package project.regexparser

/**
 * This object contains the logic of NFA of regular expression.
 */
object NFA {
  /**
   * Entry point for converting regex to NFA.
   * @param regex : Regular expression to be parsed.
   * @return
   */
  def regexToNFA(regex: RegexExpr) : State =
    regexToNFA(regex, Match())

  /**
   * This function goes in with given regex and the next state needed.
   * @param regex : Regular expression to be parsed.
   * @param andThen : next state context for the regex
   * @return
   */
  private def regexToNFA(regex: RegexExpr, andThen: State) : State =
    regex match {
    case Literal(c) => new Consume(c, andThen)
    case Concat(first, second) =>
      // convert first to NFA and the out state for that is the NFA of the second expression.
      regexToNFA(first, regexToNFA(second, andThen))

    case Or(left, right) => new Split( regexToNFA(left, andThen), regexToNFA(right, andThen))
    case Repeat(r) =>
      //Since repeat is not pre determined, we set the output state as a variable.
      val placeholder = new Placeholder(null)
      // we go into two states 1. repeat with unset output state. 2. andThen as the next output state.
      val split = new Split(regexToNFA(r,placeholder), andThen)
      //once we have consumed all the repeat characters, we set the placeholder to the expected out state of andThen.
      placeholder.pointingTo = andThen
      placeholder

    case Plus(p) => regexToNFA(Concat(p, Repeat(p)), andThen)
  }
}
