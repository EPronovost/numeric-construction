package reals.truncations

import reals._

/** Truncations
  *
  * The set of all truncations (terminal decimals) forms a commutative
  * ring when the length of the expansion is unbounded.  To facilitate this,
  * we mimic the conventional
  * [[https://en.wikipedia.org/wiki/IEEE_754 floating point representaiton]]
  * but allow for arbitrary precision in the significant.
  */
sealed abstract class Truncation extends Comparable[Truncation] {
  
  def toDouble: Double
  def simplify: Truncation = this
  
  def +(that: Truncation): Truncation
  def unary_- : Truncation
  
  final def -(that: Truncation): Truncation = this + -that
  
  def *(that: Truncation): Truncation
  
  def **(power: Int): Truncation
  
  def <<(shift: Int): Truncation = this
  final def >>(shift: Int): Truncation = this.<<(-shift)
}

object Truncation {
  
  /** The number of bits used in the standard double representation. */
  final val DoubleBits = 53
  
  def apply(d: Double): Truncation = d match {
    case 0 => Zero
    case _ if (d < 0) => -Truncation(-d)
    case _ if (d > 0) => {
      val maxExponent = log2(d).floor
      val powersOfTwo = (0 to DoubleBits).map(i => Math.pow(2, maxExponent - i))
      val conversion = powersOfTwo.scanLeft((false, d))((prior, next) =>
        if (prior._2 >= next) (true, prior._2 - next) else (false, prior._2))
      val bits = conversion.map(_._1).tail.toVector
      Decimal(true, maxExponent.toInt, bits).simplify
    }
  }
  
  def apply(i: Int): Truncation = Truncation(i.toDouble)
  
  def apply(f: Float): Truncation = Truncation(f.toDouble)
  
  def apply(l: Long): Truncation = Truncation(l.toDouble)
}

/**
  * Represents a non-zero decimal number.
  * @param sign true for positive, false for negative
  * @param exp the exponent to multiply by
  * @param digits the digits, where the first digit is most
  *               significant and corresponds to 2^exp^
  */
private[truncations] final case class Decimal(sign: Bit, exp: Int, digits: Vector[Bit]) extends Truncation {
  
  override def toString: String =
    this.copy(exp = 0).toDouble.toString + " * 2^" + exp.toString
  
  def toDouble: Double = {
    val absoluteValue = digits.zip(exp to (exp - digits.length) by -1)
      .map(pair => if (pair._1) Math.pow(2, pair._2) else 0)
      .reduce(_ + _)
    if (sign) absoluteValue else -absoluteValue
  }
  
  override def simplify: Truncation = {
    val frontZeros = digits.prefixLength(!_)
    if (frontZeros == digits.length) Zero
    else {
      val tailZeros = digits.length - digits.lastIndexOf(true) - 1
      this.copy(exp = this.exp - frontZeros, digits = this.digits.drop(frontZeros).dropRight(tailZeros))
    }
  }
  
  override def <<(shift: Int): Truncation = copy(exp = this.exp + shift)
  
  override def compareTo(o: Truncation): Int = o match {
    case Zero => if (sign) 1 else -1
    case Decimal(nSign, _, _) if (sign != nSign) =>
      if (sign) 1 else -1
    case n @ Decimal(nSign, _, _) if (sign == nSign) => {
      val (d1, d2) = Decimal.alignBits(this, n)
      val comparison = (d1 zip d2)
        .find(pair => (pair._1).compareTo(pair._2) != 0)
      comparison match {
        case None => 0
        case Some((b1, b2)) => if (sign) b1.compareTo(b2) else -b1.compareTo(b2)
      }
    }
  }
  
  private def matchExponent(n: Int): Vector[Bit] = {
    assert(n >= exp)
    Vector.fill(n - exp)(false) ++ digits
  }
  
  def +(that: Truncation): Truncation = that match {
    case Zero => this
    case n @ Decimal(nSign, _, _) if (sign == nSign) => {
      val (d1, d2) = Decimal.alignBits(this, n)
      val addedResult = Decimal.addBits(d1, d2)
      addedResult.copy(sign = this.sign, exp = addedResult.exp + (exp max n.exp)).simplify
    }
    case n @ Decimal(nSign, nExp, _) if (sign != nSign) => {
      val (d1, d2) = Decimal.alignBits(this, n)
      val d2Compliment = Decimal.twosCompliment(false +: d2)
      val added = Decimal.addBits(false +: d1, d2Compliment)
      val resultingBits = if (added.exp == 1) added.digits.tail else added.digits
      
      if (resultingBits.head) Decimal(!sign, exp max nExp, Decimal.twosCompliment(resultingBits).tail).simplify
      else Decimal(sign, exp max nExp, resultingBits.tail).simplify
    }
  }
  
  def *(that: Truncation): Truncation = that match {
    case Zero => Zero
    case Decimal(nSign, _, _) if (nSign != sign) => -(this * (-that))
    case n @ Decimal(_, nExp, nDigits) => {
      //TODO: Karatsuba
      val innerTerm = Decimal(true, 0, nDigits)
      val outerPowers = digits.zipWithIndex.filter(_._1).map(- _._2)
      val sumTerms: Vector[Truncation] = outerPowers.map(Decimal(true, _, nDigits))
      val sum = sumTerms.reduce(_ + _)
      sum << (this.exp + nExp)
    }
  }
  
  def **(power: Int): Truncation = {
    //TODO: powers of two
    assert(power >= 0, "exponent must be non-negative")
    if (power == 0) Truncation(1)
    else {
      val copies: List[Truncation] = List.fill(power)(this)
      copies.reduce(_ * _)
    }
  }
  
  def unary_- : Truncation = copy(sign = !this.sign)
}

private[truncations] final object Decimal {
  
  def addBits(d1: Vector[Bit], d2: Vector[Bit]): Decimal = {
    assert(d1.length == d2.length)
    val addition = (d1 zip d2).scanRight((false, false))(threeWayAdd).init
    val addedDigits = addition.map(_._2)
    
    if (addition.head._1) Decimal(true, 1, true +: addedDigits)
    else Decimal(true, 0, addedDigits)
  }
  
  def threeWayAdd(next: (Bit, Bit), prior: (Bit, Bit)): (Bit, Bit) = {
    val term = (next._1 ^ next._2) ^ prior._1
    val carry = ((next._1 ^ next._2) & prior._1) || (next._1 && next._2)
    (carry, term)
  }
  
  def twosCompliment(digits: Vector[Bit]): Vector[Bit] = {
    val inverted = digits.map(!_)
    inverted.lastIndexOf(false) match {
      case -1 => Vector.fill(digits.length)(false)
      case i => inverted.take(i) ++ (true :: List.fill(digits.length - i - 1)(false))
    }
  }
  
  /** Return the two bit lists, exponent aligned and length matched */
  def alignBits(x: Decimal, y: Decimal): (Vector[Bit], Vector[Bit]) = {
    val maxE = x.exp max y.exp
    val (d1, d2) = (x.matchExponent(maxE), y.matchExponent(maxE))
    val maxLength = d1.length max d2.length
    (d1.padTo(maxLength, false), d2.padTo(maxLength, false))
  }
}

private[truncations] final object Zero extends Truncation {
  
  override def toString: String = "0"
  
  def toDouble: Double = 0
  
  def +(that: Truncation): Truncation = that
  
  def unary_- : Truncation = Zero
  
  def *(that: Truncation): Truncation = Zero
  
  def **(power: Int) = {
    assert(power >= 0, "exponent must be non-negative")
    Zero
  }
  
  def /(that: Truncation): Truncation = Zero
  
  override def compareTo(o: Truncation): Int = o match {
    case Zero => 0
    case Decimal(true, _, _) => -1
    case Decimal(false, _, _) => 1
  }
}