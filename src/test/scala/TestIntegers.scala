import algebraic.integers._
import algebraic.properties.RingDivisionError
import org.scalatest.FunSuite

class TestIntegers extends FunSuite {
  
  val MAX_TO_TEST = 64
  val MAX_TO_TEST_MULTIPLICATION = Math.sqrt(MAX_TO_TEST).toInt
  
  // Ordered Tests
  
  test("Ordering") {
    for (i <- -MAX_TO_TEST to MAX_TO_TEST; j <- -MAX_TO_TEST to MAX_TO_TEST) {
      assertResult(i == j) { Integer(i) == Integer(j) }
      assertResult(i  < j) { Integer(i)  < Integer(j) }
      assertResult(i <= j) { Integer(i) <= Integer(j) }
    }
  }
  
  // Group Tests for Addition
  
  test("Addition") {
    for (i <- -MAX_TO_TEST to MAX_TO_TEST; j <- -MAX_TO_TEST to MAX_TO_TEST) {
      assertResult(Integer(i + j)) { Integer(i) + Integer(j) }
    }
  }
  
  test("Additive Inverse") {
    for (i <- -MAX_TO_TEST to MAX_TO_TEST) {
      assert(Integer(-i) == -Integer(i))
    }
  }
  
  // Ring Tests
  test("Multiplication") {
    for (i <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         j <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION) {
      assertResult(Integer(i * j)) { Integer(i) * Integer(j) }
    }
  }
  
  test("Associativity") {
    for (i <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         j <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         k <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         myI = Integer(i);
         myJ = Integer(j);
         myK = Integer(k)) {
      assert(myI * (myJ + myK) == myI * myJ + myI * myK)
    }
  }
  
  // Enumeration
  test("Countable") {
    val expected = 0 :: ((1 to MAX_TO_TEST) flatMap { case i => List(i, -i) } toList)
    for ( (x, i) <- expected.zip(Integer.enumerate)) {
      assert(Integer(x) == i)
    }
  }
  
  // Modulo operation
  test("Modulo") {
    for (i <- 1 to MAX_TO_TEST;
         j <- 1 to MAX_TO_TEST) {
      assertResult(Integer(i % j)) { Integer(i) % Integer(j) }
    }
      
      for (i <- -MAX_TO_TEST to MAX_TO_TEST;
           j <- -MAX_TO_TEST to MAX_TO_TEST;
           myI = Integer(i);
           myJ = Integer(j);
           modulo = myI % myJ) {
        assert(myJ.cosetOf(myI - modulo) == Integer.IntegerZero)
      }
  }
  
  // Division
  test("Division") {
    for (i <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         if (i != 0);
         j <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         product = Integer(i) * Integer(j)) {
      assertResult(Integer(j)) {product / Integer(i)}
    }
    
    for (i <- -MAX_TO_TEST to MAX_TO_TEST) {
      assertThrows[RingDivisionError] { Integer(i) / Integer.IntegerZero }
    }
    
    for (i <- -MAX_TO_TEST to MAX_TO_TEST;
         j <- -MAX_TO_TEST to MAX_TO_TEST;
         if j != 0 && i % j != 0) {
      assertThrows[RingDivisionError] { Integer(i) / Integer(j) }
    }
  }
  
  // Abs
  test("Abs") {
    for (i <- -MAX_TO_TEST to MAX_TO_TEST) {
      assert(Integer(Math.abs(i)) == Integer.abs(Integer(i)))
    }
  }
  
  // GCD
  
  def gcd(a: Int, b: Int): Int = {
    if ((a < 0) || (b < 0)) gcd(Math.abs(a), Math.abs(b))
    else if (a < b) gcd(b, a)
    else if (b == 0) a
    else gcd(b, a % b)
  }
  
  test("GCD") {
    for (i <- -MAX_TO_TEST to MAX_TO_TEST;
         j <- -MAX_TO_TEST to MAX_TO_TEST) {
      assertResult(Integer(gcd(i, j))) { Integer.gcd(Integer(i), Integer(j)) }
    }
  }

}