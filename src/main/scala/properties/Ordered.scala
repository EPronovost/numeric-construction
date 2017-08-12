package properties

/** The types of comparisons possible in a totally ordered set. */
sealed trait Comparison
final case object LessThan extends Comparison
final case object Equal extends Comparison
final case object GreaterThan extends Comparison

/** Total Ordering
  *
  * A [[https://en.wikipedia.org/wiki/Total_order total ordering]] is a binary relation
  * ''T'' x ''T'' -> Comparison that is antisymmetric, transitive, and total (i.e. well
  * defined for all pairs in (''T'', ''T'').
  */
trait Ordered[T] {
    def compare(that: T): Comparison
    
    def <(that: T): Boolean = this.compare(that) == LessThan
    def >(that: T): Boolean = this.compare(that) == GreaterThan
    
    def <=(that: T): Boolean = !(this > that)
    def >=(that: T): Boolean = !(this < that)
}
