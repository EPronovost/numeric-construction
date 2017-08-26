# numeric-construction

Constructing Number Systems from First Principles

> In the beginning, there was zero.

---
## Algebraic Constructions

### The Natural Numbers

The beginning of developing an axiomatic number system starts with the [natural numbers](https://en.wikipedia.org/wiki/Natural_number) { 0, 1, 2, 3, ... }, which are constructed using the [Peano Axioms](https://en.wikipedia.org/wiki/Peano_axioms).  In this approach, we define everything in terms of a "starting element" 0 and a "successor function" _S(a)_.  This set, combined with the addition operation, forms a [commutative monoid](https://en.wikipedia.org/wiki/Monoid), but fails to form an addative group, lacking inverses.

### The Ring of Integers

To extend the natural numbers into a full [ring](https://en.wikipedia.org/wiki/Ring_(mathematics)), we include additive inverses (a.k.a. "negatives") for all the natural numbers.  This yields the ring of [integers](https://en.wikipedia.org/wiki/Integer).

### The Field of Fractions of the Rationals

Similarly to how the natural numbers lacked additive inverses to make it a group, the integers lack multiplicative inverses to make it a [field](https://en.wikipedia.org/wiki/Field_(mathematics)).  As the integers form an [integral domain](https://en.wikipedia.org/wiki/Integral_domain), we can extend them to a [field of fractions](https://en.wikipedia.org/wiki/Field_of_fractions), yielding the [rational numbers](https://en.wikipedia.org/wiki/Rational_number).

---
## A Precise Number System

The limitations of the algebraic representation are obvious if you try to create a natural number for 10,000: Stack Overflow!  It turns out this is a very inefficient way to represent numbers.

The next question I had is how can we define an infinite precision number system.  Fixed-size approximations such as `Float` or `Double` are just that; addition is technically [not even associative](https://en.wikipedia.org/wiki/Associative_property#Nonassociativity_of_floating_point_calculation).
  
### Finite Length Truncations

To solve this problem, we first create an object Truncation that represents any finite length decimal.  This is modeled after [IEEE floating points](https://en.wikipedia.org/wiki/IEEE_754), with a sign bit, exponent, and significant.  However, for infinite precision, the length of the significant is unbounded.  However, as it is a vector, it must be finite.  Along with these truncations, we define a Zero object.  These are captured in Scala's `BigDecimal` type.