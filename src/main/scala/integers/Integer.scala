package integers

import naturals._
import properties._

/** The Ring of Integers
  *
  * The ring of [[https://en.wikipedia.org/wiki/Integer integers]]
  * (commonly denoted ''Z'') is the set { ..., -2, -1, 0, 1, 2, ... },
  * along with the two binary operations addition and multiplication.
  *
  * This can be constructed from the natural numbers by taking the
  * closure of the natural numbers under the group action of addition.
  * This requires there to be additive inverses, which yields the "negative
  * number" construction.
  *
  * The group of integers emits a ring structure as well, which we simply include
  * together.  The axioms defining a [[https://en.wikipedia.org/wiki/Ring_(mathematics) ring]]
  * specify the relationship between addition and multiplication that must exist.
  */
abstract class Integer extends Ring[Integer]
    with Ordered[Integer]
    with Countable[Integer] {
    
    /** A convenience shorthand notation */
    def -(that: Integer) = this + (- that)
    
    /** The [[Integer]] of the natural number precessor. */
    def priorNatural: Integer
}

object Integer extends Countable[Integer] {
    def apply(x: Int): Integer = x match {
        case 0 => IntegerZero
        case _ if x > 0 => PositiveInteger(NaturalNumber(x))
        case _ if x < 0 => NegativeInteger(NaturalNumber(-x))
    }
    
    def apply(x: NaturalNumber): Integer = x match {
        case NaturalZero => IntegerZero
        case Successor(_) => PositiveInteger(x)
    }
    
    /** The canonical enumeration of integers goes {0, 1, -1, 2, -2, ...} */
    def enumerate: Stream[Integer] = IntegerZero enumerate
}

object IntegerZero extends Integer {
    
    override def toString: String = NaturalZero toString
    
    def priorNatural = throw new NoSuchElementException
    
    /** Additive properties follow from the group axioms. */
    def +(that: Integer) = that
    
    def unary_- = this
    
    /** Multiplicative properties follow from the ring axioms. */
    def *(that: Integer) = this
    
    override def compare(that: Integer): Comparison = that match {
        case NegativeInteger(_) => GreaterThan
        case IntegerZero => Equal
        case PositiveInteger(_) => LessThan
    }
    
    override def enumerate: Stream[Integer] =
        this #:: (PositiveInteger(Successor(NaturalZero)) enumerate)
}

/** Positive integers are largely wrappers for [[NaturalNumber natural numbers]]. */
case class PositiveInteger(n: NaturalNumber) extends Integer {
    assert(n != NaturalZero)
    
    override def toString: String = n toString
    
    def priorNatural = n match {
        case Successor(pre) => pre match {
            case NaturalZero => IntegerZero
            case Successor(_) => PositiveInteger(pre)
        }
    }
    
    def +(that: Integer): Integer = that match {
        case PositiveInteger(o) => PositiveInteger(n + o)
        case IntegerZero => this
        case NegativeInteger(_) => this.priorNatural + that.priorNatural
    }
    
    /** The additive inverse is defined by the group axioms. */
    def unary_- = NegativeInteger(n)
    
    /** Multiplicaiton is defined by the ring axioms. */
    def *(that: Integer): Integer = that match {
        case NegativeInteger(_) => -(this * (-that))
        case IntegerZero => IntegerZero
        case PositiveInteger(_) => this.priorNatural * that + that
    }
    
    override def compare(that: Integer): Comparison = that match {
        case NegativeInteger(_) | IntegerZero => GreaterThan
        case PositiveInteger(o) => n compare o
    }
    
    override def enumerate: Stream[Integer] = this #:: (-this enumerate)
}

/**
  * Negative integers are the additive inverses of natural numbers.
  *
  * Most of the definitions are reductions to positive base cases.
  */
case class NegativeInteger(n: NaturalNumber) extends Integer {
    assert(n != NaturalZero)
    
    override def toString: String = "-" + (n toString)
    
    def priorNatural = n match {
        case Successor(pre) => pre match {
            case NaturalZero => IntegerZero
            case Successor(_) => NegativeInteger(pre)
        }
    }
    
    def +(that: Integer) = -(-this - that)
    
    def unary_- = PositiveInteger(n)
    
    def *(that: Integer) = -(-this * that)
    
    override def compare(that: Integer): Comparison = that match {
        case PositiveInteger(_) | IntegerZero => LessThan
        case NegativeInteger(o) => o compare n
    }
    
    override def enumerate: Stream[Integer] = this #:: (PositiveInteger(Successor(n)) enumerate)
}