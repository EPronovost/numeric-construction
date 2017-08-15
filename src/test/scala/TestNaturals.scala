import algebraic.naturals._
import org.scalatest.FunSuite

class TestNaturals extends FunSuite {
    
  val MAX_TO_TEST = 64
    
  // Ordered Tests
    
  test("Ordering") {
    for (i <- 0 to MAX_TO_TEST; j <- 0 to MAX_TO_TEST) {
      assertResult(i == j) { NaturalNumber(i) == NaturalNumber(j) }
      assertResult(i  < j) { NaturalNumber(i)  < NaturalNumber(j) }
      assertResult(i <= j) { NaturalNumber(i) <= NaturalNumber(j) }
    }
  }
    
  // Commutative Monoid Tests for Addition
  
  test("Addition") {
    for (i <- 0 to MAX_TO_TEST; j <- 0 to MAX_TO_TEST) {
      assertResult(NaturalNumber(i + j)) { NaturalNumber(i) + NaturalNumber(j) }
    }
  }
    
  // Countable
  
  test("Countable") {
    for ((i, n) <- (0 to MAX_TO_TEST).zip(NaturalNumber.enumerate)) {
      assert(NaturalNumber(i) == n)
    }
  }
}
