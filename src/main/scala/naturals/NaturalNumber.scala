package naturals

import properties._

/** The Natural Numbers
  *
  * The natural numbers, commonly denoted ''N'', is the set { 0, 1, 2, ... }.
  * This set forms a [[Monoid commutative monoid]] under addition.
  *
  * The natural numbers can be constructed axiomatically using the
  * [[https://en.wikipedia.org/wiki/Peano_axioms Peano Axioms]].  These define a minimal set
  * of axioms from which the entire behavior of the natural numbers can be defined.
  */
abstract class NaturalNumber extends Monoid[NaturalNumber]
    with Ordered[NaturalNumber]
    with Countable[NaturalNumber] {
    
    override def toString: String = (getDigits reverse) mkString
    
    /** Get the readable digit expansion of a number. */
    def getDigits: List[Char] = this match {
        case NaturalZero => List(digits head)
        case Successor(n) => incrementDigits(n getDigits)
    }
    
    /** There's nothing special about base 10.  Choose whatever representation you want! */
    val digits = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e')
    val representationBase = digits length
    
    def incrementDigits(ds: List[Char]): List[Char] = ds match {
        case List() => List((digits tail) head)
        case d :: rest if (d == (digits last)) => (digits head) :: incrementDigits(rest)
        case d :: rest => digits((digits indexOf d) + 1) :: rest
    }
    
    override def enumerate: Stream[NaturalNumber] =
        this #:: (Successor(this) enumerate)
}

object NaturalNumber extends Countable[NaturalNumber] {
    def apply(x: Int): NaturalNumber = x match {
        case 0 => NaturalZero
        case _ if (x > 0) => Successor(NaturalNumber(x - 1))
    }
    
    override def enumerate: Stream[NaturalNumber] = NaturalZero enumerate
}

/**
  * Axiom 1: 0 is a natural number
  */
object NaturalZero extends NaturalNumber {
    
    override def compare(that: NaturalNumber): Comparison = that match {
        case NaturalZero => Equal
        case Successor(_) => LessThan
    }
    
    /** Axiom A1: ''0 + a = a'' */
    override def +(that: NaturalNumber): NaturalNumber = that
}

/** Axiom 6: For every natural number ''n'', its successor ''S(n)'' is a natural number. */
case class Successor(n: NaturalNumber) extends NaturalNumber {
    
    /**
      * Axiom 7: For all ''m, n,'' ''m = n'' iff ''S(m) = S(n)''
      * Axiom 8: For all ''n'', ''S(n) = 0'' is false.
      */
    override def compare(that: NaturalNumber): Comparison = that match {
        case NaturalZero => GreaterThan
        case Successor(o) => n.compare(o)
    }
    
    /** Axiom A2: ''S(n) + m = S(n + m)'' */
    override def +(that: NaturalNumber): NaturalNumber = Successor(n + that)
}
