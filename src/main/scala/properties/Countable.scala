package properties

/** Countable Set
  *
  * A set is [[https://en.wikipedia.org/wiki/Countable_set countable]] (also referred
  * to as ''at most countable'') if all elements can be enumerated in a sequence,
  * such that every element of that set will eventually appear in the sequence.  In
  * other words, we can form a bijection from this set to the set of natural numbers,
  * the "fundamental" countably infinite set.
  */
trait Countable[T] {
  def enumerate: Stream[T]
}
