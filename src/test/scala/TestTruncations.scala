import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import truncation.truncations.Truncation


object TestTruncations extends Properties("Truncation") {
  
  def withinEpsilon(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs / (d1 + d2).abs < Math.pow(2, -15)
  }
  
  property("initFromDouble") = forAll { (d: Double) =>
    Truncation(d).toDouble == d
  }
  
  property("addition") = forAll { (x: Double, y: Double) =>
    withinEpsilon((Truncation(x) + Truncation(y)).toDouble, x + y)
  }
  
  property("multiplication") = forAll { (x: Double, y: Double) =>
    (x * y).abs == Double.PositiveInfinity ||
      (x * y).abs == 0.0 ||
      withinEpsilon((Truncation(x) * Truncation(y)).toDouble, x * y)
  }
  
  property("ordering") = forAll { (x: Double, y: Double) =>
    Truncation(x).compareTo(Truncation(y)) == x.compareTo(y)
  }
}
