
package object truncation {
  type Bit = Boolean
  
  final val LogOf2: Double = Math.log(2)
  def log2(x: Double): Double = Math.log(x) / LogOf2
}
