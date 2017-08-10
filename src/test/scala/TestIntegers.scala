import main.scala.integers.Integer
import org.scalatest.FunSuite

class TestIntegers extends FunSuite {
    
    val MAX_TO_TEST = 64
    
    val MAX_TO_TEST_MULTIPLICATION = Math.sqrt(MAX_TO_TEST) toInt
    
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
        for (i <- -MAX_TO_TEST to MAX_TO_TEST; j <- -MAX_TO_TEST to MAX_TO_TEST)
            assertResult(Integer(i + j)) { Integer(i) + Integer(j) }
    }
    
    test("Additive Inverse") {
        for (i <- -MAX_TO_TEST to MAX_TO_TEST)
            assert(Integer(-i) == -Integer(i))
    }
    
    // Ring Tests
    test("Multiplication") {
        for (i <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
             j <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION)
            assertResult(Integer(i * j)) { Integer(i) * Integer(j) }
    }
    
    test("Associativity") {
        for (i <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
             j <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
             k <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
             myI = Integer(i);
             myJ = Integer(j);
             myK = Integer(k)
        ) assert(myI * (myJ + myK) == myI * myJ + myI * myK)
    }
}