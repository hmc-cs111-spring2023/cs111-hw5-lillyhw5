package dfa // leave this line in the file

class DFA(val states: Set[State], val start: State, val accept: Set[State], val transitions: Set[Transition]):
    def accepts(input: String): Boolean = input match {
        case "" => accept.contains(start) // if empty string, return true if start state is also an accepting state; false otherwise
        case _ => acceptsHelper((input, start))
    }

    // recursive helper for accepts function
    def acceptsHelper(tup: (String, State)): Boolean = {
        lazy val Trans = returnTransition(tup(0)(0), tup(1)) // retrieve the transition that takes 
        tup match {
            case ("", currState) => accept.contains(currState) 
            case (input, currState) if !Trans.isEmpty => acceptsHelper((input.substring(1), Trans.head.to))
            case (input, currState) => false 
        }
    }

    def returnTransition(char: Char, currState: State) = {
        val trans = transitions.filter(x => x.from == currState && x.symbol == char )
        trans 
    }
            
case class State(label: String)

case class Transition(from: State, to: State, symbol: Char)
