package algebraic.integers

import algebraic.naturals._
import algebraic.properties._

import scala.util.{Failure, Try}

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
sealed abstract class Integer extends Ring[Integer] with Ordered[Integer] with Countable[Integer] {
    
  /** A convenience shorthand notation */
  def -(that: Integer): Integer = this + (-that)
  
  /** The [[Integer]] of the natural number precessor. */
  private[integers] def priorNatural: Integer
  
  def divide(that: Integer): Try[Integer]
  
  def %(that: Integer): Integer = that.cosetOf(this)
  def /(that: Integer): Integer = that.divide(this).get
}

object Integer extends Countable[Integer] {
  
  final val IntegerZero: Integer = IntZero
  
  final val IntegerOne: Integer = PositiveInteger(NaturalNumber.NaturalOne)
  
  def apply(x: Int): Integer = x match {
    case 0 => IntegerZero
    case _ if x > 0 => PositiveInteger(NaturalNumber(x))
    case _ if x < 0 => NegativeInteger(NaturalNumber(-x))
  }
  
  def apply(x: NaturalNumber): Integer = x match {
    case NaturalZero => IntegerZero
    case _ => PositiveInteger(x)
  }
  
  /** The canonical enumeration of integers goes {0, 1, -1, 2, -2, ...} */
  def enumerate: Stream[Integer] = IntegerZero.enumerate
  
  def abs(x: Integer): Integer = x match {
    case NegativeInteger(n) => PositiveInteger(n)
    case _ => x
  }
  
  /**
    * The gcd is computed with
    * [[https://en.wikipedia.org/wiki/Euclidean_algorithm Euclid's algorithm]]
    * since ''Z'' is a [[https://en.wikipedia.org/wiki/Euclidean_domain Euclidian Domain]].
    * */
  def gcd(a: Integer, b: Integer): Integer = {
    if ((a < IntegerZero) || (b < IntegerZero)) gcd(abs(a), abs(b))
    else if (a < b) gcd(b, a)
    else if (b == IntegerZero) a
    else gcd(b, a % b)
  }
  
  def sign(i: Integer): Integer = i match {
    case PositiveInteger(_) => IntegerOne
    case NegativeInteger(_) => -IntegerOne
    case IntZero => IntZero
  }
  
  //TODO: Factorize
}

private[integers] object IntZero extends Integer {
    
  override def toString: String = NaturalZero toString
  
  def priorNatural: Integer = throw new NoSuchElementException
  
  /** Additive properties follow from the group axioms. */
  def +(that: Integer): Integer = that
  
  def unary_- : Integer = this
  
  /** Multiplicative properties follow from the ring axioms. */
  def *(that: Integer): Integer = this
  
  def compare(that: Integer): Comparison = that match {
    case NegativeInteger(_) => GreaterThan
    case IntZero => Equal
    case PositiveInteger(_) => LessThan
  }
    
  def enumerate: Stream[Integer] =
    this #:: PositiveInteger(Successor(NaturalZero)).enumerate
    
  def cosetOf(that: Integer): Integer = that
    
  def divide(that: Integer): Try[Integer] = Failure(RingDivisionError("Division by zero"))
}

/** Positive integers are largely wrappers for [[NaturalNumber natural numbers]]. */
private[integers] final case class PositiveInteger(n: NaturalNumber) extends Integer {
  override def toString: String = n.toString
    
  def priorNatural: Integer = n match {
    case Successor(pre) => pre match {
      case NaturalZero => IntZero
      case _ => PositiveInteger(pre)
    }
    case NaturalZero => throw new NoSuchElementException
  }
    
  def +(that: Integer): Integer = that match {
    case PositiveInteger(o) => PositiveInteger(n + o)
    case IntZero => this
    case NegativeInteger(_) => this.priorNatural + that.priorNatural
  }
    
  /** The additive inverse is defined by the group axioms. */
  def unary_- : Integer = NegativeInteger(n)
    
  /** Multiplicaiton is defined by the ring axioms. */
  def *(that: Integer): Integer = that match {
    case NegativeInteger(_) => -(this * (-that))
    case IntZero => IntZero
    case PositiveInteger(_) => this.priorNatural * that + that
  }
    
  def compare(that: Integer): Comparison = that match {
    case NegativeInteger(_) | IntZero => GreaterThan
    case PositiveInteger(o) => n.compare(o)
  }
    
  def enumerate: Stream[Integer] = this #:: (-this).enumerate
    
  def cosetOf(that: Integer): Integer = that match {
    case NegativeInteger(_) => this.cosetOf(-that) match {
      case IntZero => IntZero
      case m => this - m
    }
    case _ if (that < this) => that
    case _ if (that >= this) => this.cosetOf(that - this)
  }
    
  def divide(that: Integer): Try[Integer] = Try { that match {
    case NegativeInteger(_) => -(this.divide(-that).get)
    case IntZero => IntZero
    case _ if (that >= this) => this.divide(that - this).get + Integer.IntegerOne
    case _ => throw RingDivisionError("does not divide")
  }}
}

/**
  * Negative integers are the additive inverses of natural numbers.
  *
  * Most of the definitions are reductions to positive base cases.
  */
private[integers] final case class NegativeInteger(n: NaturalNumber) extends Integer {
  override def toString: String = "-" + n.toString
    
  def priorNatural: Integer = n match {
    case Successor(pre) => pre match {
      case NaturalZero => IntZero
      case _ => NegativeInteger(pre)
    }
    case NaturalZero => throw new NoSuchElementException
  }
    
  def +(that: Integer): Integer = -(-this - that)
    
  def unary_- : Integer = PositiveInteger(n)
    
  def *(that: Integer): Integer = -(-this * that)
    
  def compare(that: Integer): Comparison = that match {
    case PositiveInteger(_) | IntZero => LessThan
    case NegativeInteger(o) => o.compare(n)
  }
    
  def enumerate: Stream[Integer] = this #:: PositiveInteger(Successor(n)).enumerate
    
  def cosetOf(that: Integer): Integer = (-this).cosetOf(that)
    
  def divide(that: Integer): Try[Integer] = Try { -((-this).divide(that).get) }
}

