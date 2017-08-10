package main.scala.properties

/** Commutative Ring with Unit
  *
  * A [[https://en.wikipedia.org/wiki/Ring_(mathematics) ring]] is a set equipped with two
  * closed binary operations, denoted + and *, that observe associativity and commutativity.
  * A ring with unit includes a multiplicative identity (e.g. 1), and a commutative ring
  * means that multiplication is commutative.
  *
  * Clearly this extends a commutative monoid.  Between these two categories lies the
  * category of groups (and abelian groups).  However, the group closure of the natural
  * numbers provides a full ring structure, so we skip right to rings.
  */
trait Ring[T] extends Monoid[T] {
    def unary_- : T
    def -(that: T): T
    def *(that: T): T
}
