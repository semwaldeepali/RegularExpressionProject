package project.regexparser

/**
 * This class defines the component for implementing NFA for the regular expression defined in RegexExpr.
 * We are converting regex tree into NFA.
 */
abstract class State

/**
 * This class represents the arrow in the graph that consumes in an input character
 * and goes to the given output state.
 * @param c : Input character to be consumed
 * @param out : output state
 */
class Consume(val c: Char, val out: State ) extends State

/**
 * This class provides mechanism to split from one state into two given output states.
 * @param out1 : output state 1
 * @param out2 : output state 2
 */
class Split(val out1: State, val out2: State) extends State

/**
 * This class represents the final state of NFA when the input has been consumed and was in accordance with the
 * grammar of NFA.
 * We use case class as Match state is same even if reached from different states of input consumption.
 */
case class Match() extends State

/**
 * TODO : not so clear of the usage.
 * @param pointingTo : final output state variable.
 */
class Placeholder(var pointingTo: State) extends State