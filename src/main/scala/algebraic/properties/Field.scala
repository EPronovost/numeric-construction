package algebraic.properties

/** Field
  *
  * A [[https://en.wikipedia.org/wiki/Field_(mathematics) field]] is a ring in which
  * every non-zero element is a unit (i.e. has a multiplicative inverse).  This means
  * that division becomes well defined so long as the divisor is not zero, so the only
  * division error possible is [[FieldZeroDivisionError]].  A common way to construct a
  * field from a ring is to use the
  * [[https://en.wikipedia.org/wiki/Field_of_fractions field of fractions]].
  */
trait Field[T] extends Ring[T] {
  def inverse: T
}

case object FieldZeroDivisionError extends ArithmeticException("Division by zero element")