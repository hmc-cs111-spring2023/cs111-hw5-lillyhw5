package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

trait RegularLanguage

case object Empty extends RegularLanguage
case object Epsilon extends RegularLanguage
case class Character(char: Char) extends RegularLanguage
case class Union(r1: RegularLanguage, r2: RegularLanguage) extends RegularLanguage
case class Concat(r1: RegularLanguage, r2: RegularLanguage) extends RegularLanguage
case class Star(r: RegularLanguage) extends RegularLanguage

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = lang match {
  case Concat(Epsilon, r: RegularLanguage) => simplify(r)
  case Concat(r: RegularLanguage, Epsilon) => simplify(r)
  case Concat(Empty, r: RegularLanguage) => simplify(Empty)
  case Concat(r: RegularLanguage, Empty) => simplify(Empty)
  case Concat(r1: RegularLanguage, r2: RegularLanguage) => Concat(simplify(r1), simplify(r2))

  case Union(Empty, r: RegularLanguage) => simplify(r)
  case Union(r: RegularLanguage, Empty) => simplify(r)
  case Union(r1: RegularLanguage, r2: RegularLanguage) => Union(simplify(r1), simplify(r2))
  
  case Star(Epsilon) => Epsilon
  case Star(Empty) => Empty
  case Star(r: RegularLanguage) => Star(simplify(r))

  case _ => lang
}

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = 
  val simpLang = simplify(lang)
  simpLang match {
    case Epsilon => true 
    case Star(r: RegularLanguage) => true 
    case _ => false 
  }

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = l match {
  case Empty => Empty 
  case Epsilon => Empty 
  case Character(d) if (c == d) => Epsilon
  case Character(d) => Empty 
  case Union(r1: RegularLanguage, r2: RegularLanguage) => Union(derivative(r1)(c), derivative(r2)(c))
  case Concat(r1: RegularLanguage, r2: RegularLanguage) if !nullable(r1) => Concat(derivative(r1)(c), r2)
  case Concat(r1: RegularLanguage, r2: RegularLanguage) => Union(Concat(derivative(r1)(c), r2), derivative(r2)(c))
  case Star(r: RegularLanguage) => Concat(derivative(r)(c), Star(r))
}

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
