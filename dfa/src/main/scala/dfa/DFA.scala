package dfa // leave this line in the file

class DFA(val states: Set[State], val start: State, val accept: Set[State], val transitions: Set[Transition]):
    /** Returns true if DFA accepts input, false otherwise. 
      */
    def accepts(input: String): Boolean = input match {
        case "" => accept.contains(start) // if empty string, return whether or not start state is an accepting state
        case _ => acceptsHelper(input, start)
    }

    /** Recursive helper for accepts function which returns Boolean value
      * if DFA is accepted, false otherwise.
      * Has additional parameter currState that keeps track of the current state 
      */
    def acceptsHelper(input: String, currState: State): Boolean = {
        lazy val Trans = returnTransition(input(0), currState) // retrieve the transition that takes 
        input match {
            case "" => accept.contains(currState) // if empty string, return whether or not current state is an accepting state
            case input if !Trans.isEmpty => acceptsHelper(input.substring(1), Trans.head.to) // recurse if there exists valid transition
                                                                                             // currState is set to what the transition leads to
            case _ => false
        }
    }

    /** Helper function that fetches transition from the set transitions that
      * satisfies from = currState and symbol = char
      * if such transition does not exist, returns empty set
      */
    def returnTransition(char: Char, currState: State) = {
        val trans = transitions.filter(x => x.from == currState && x.symbol == char )
        trans 
    }
            
case class State(label: String)

case class Transition(from: State, to: State, symbol: Char)
