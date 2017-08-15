import algebraic.rationals._
import org.scalatest.FunSuite

class TestRationals extends FunSuite {
  
  val MAX_TO_TEST = 64
  val MAX_TO_TEST_MULTIPLICATION = Math.sqrt(MAX_TO_TEST).toInt
  
  test("Addition") {
    for (i <- -MAX_TO_TEST to MAX_TO_TEST;
         j <- -MAX_TO_TEST to MAX_TO_TEST) {
      assertResult(Rational(i + j))(Rational(i) + Rational(j))
    }
  }
  
  test("Multiplication") {
    for (i <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         j <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         if j != 0) {
      assertResult(Rational(i))(Rational(i, j) * Rational(j))
    }
  }
  
  test("Inverse") {
    for (i <- -MAX_TO_TEST to MAX_TO_TEST by 4;
         j <- -MAX_TO_TEST to MAX_TO_TEST by 4;
         if (i != 0) && (j != 0)) {
      assertResult(Rational(i, j))(Rational(i, j).inverse.inverse)
    }
  }
  
  test("Ordering") {
    for (i <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         j <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         k <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         l <- -MAX_TO_TEST_MULTIPLICATION to MAX_TO_TEST_MULTIPLICATION;
         if (j != 0) && (l != 0)) {
      val a = i.toDouble / j.toDouble
      val b = k.toDouble / l.toDouble
      assertResult(a < b)(Rational(i, j) < Rational(k, l))
    }
  }
}
