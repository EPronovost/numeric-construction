package algebraic.rationals

import algebraic.integers._
import algebraic.properties._

import scala.annotation.tailrec

/** The Rational Numbers
  *
  * The [[https://en.wikipedia.org/wiki/Rational_number Rational Numbers]] is the
  * [[https://en.wikipedia.org/wiki/Field_of_fractions field of fractions]] of the
  * [[algebraic.integers.Integer integers]].  That is, it is the set of elements ''a/b'', where ''a''
  * and ''b'' are integers, and ''b != 0''.
  *
  * This defines the field operations and ordering definitions.
  * As a field, every non-zero element in the field is a unit, and so has an inverse.
  */
final case class Rational(n: Integer, d: Integer)
  extends Field[Rational]
  with Ordered[Rational]
  with Countable[Rational] {
  if (d == Integer.IntegerZero) throw FieldZeroDivisionError
  
  override def toString: String = s"$n / $d"
  
  override def equals(that: Any): Boolean = that match {
    case r @ Rational(_, _) => this.compare(r) == Equal
    case _ => false
  }
  
  /** The arithmetic operations. */
  def +(that: Rational): Rational = Rational(this.n * that.d + that.n * this.d, this.d * that.d).simplify
  
  def unary_- : Rational = Rational(-n, d)
  
  def -(that: Rational): Rational = this + (-that)
  
  def *(that: Rational): Rational = Rational(this.n * that.n, this.d * that.d).simplify
  
  def /(that: Rational): Rational = this * that.inverse
  
  def inverse: Rational = Rational(d, n)
  
  /** Ordering of a fraction field. */
  def compare(that: Rational): Comparison = (this.n * that.d).compare(this.d * that.n)
  
  def enumerate: Stream[Rational] = ???
  
  /** Every non-zero element of a field is a unit, so cosets are trivial. */
  def cosetOf(that: Rational): Rational = Rational(Integer.IntegerZero)
  
  def %(that: Rational): Rational = Rational(Integer.IntegerZero)
  
  def simplify: Rational = {
    val commonFactor = Integer.gcd(n, d)
    Rational(Integer.sign(d) * n / commonFactor, Integer.abs(d) / commonFactor)
  }
}

object Rational {
  
  /** To prevent stack overflow for the underlying natural numbers, we limit the precision. */
  final val DoublePrecision = 2
  
  final def RationalZero = Rational(Integer.IntegerZero)
  final def RationalOne = Rational(Integer.IntegerOne)
  
  def apply(x: Int): Rational = Rational(Integer(x))
  
  def apply(n: Int, d: Int): Rational = Rational(Integer(n), Integer(d)).simplify
  
  def apply(f: Float): Rational = Rational(f.toDouble)
  
  /** To limit stack overflow errors, we pre-optimize the integers before creating the [[Rational]]. */
  def apply(d: Double): Rational = {
    @tailrec
    def integerGcd(a: Int, b: Int): Int = {
      if (a < 0 || b < 0) integerGcd(Math.abs(a), Math.abs(b))
      else if (a < b) integerGcd(b, a)
      else if (b == 0) a
      else integerGcd(b, a % b)
    }
    val numerator = (d * Math.pow(10, DoublePrecision)).toInt
    val denominator = Math.pow(10, DoublePrecision).toInt
    val commonFactor = integerGcd(numerator, denominator)
    Rational(numerator / commonFactor, denominator / commonFactor)
  }
  
  def apply(i: Integer): Rational = Rational(i, Integer.IntegerOne)
  
  def abs(r: Rational): Rational = Rational(Integer.abs(r.n), Integer.abs(r.d))
  
  def sign(r: Rational): Integer = Integer.sign(r.n) * Integer.sign(r.d)
}