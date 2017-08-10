package properties

/** Commutative Monoid
  *
  * A [[https://en.wikipedia.org/wiki/Monoid commutative monoid]] is defined
  * as a binary operation ''T'' x ''T'' -> ''T'', commonly denoted "+", that is
  * closed and commutative.
  */
trait Monoid[T] {
    def +(that: T): T
}
