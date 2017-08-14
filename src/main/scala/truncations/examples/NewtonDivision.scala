package truncations.examples

import truncations.Truncation
import truncations.truncationHelpers._

import scala.io.StdIn

/**
  * We can use the
  * [[https://en.wikipedia.org/wiki/Newton%27s_method Newton Raphson method]]
  * to find the inverse of a number by noting that the inverse of ''t'' is
  * the root of ''t - 1/x''.  The iterative update step of the Newton Raphson
  * method for this equation reduces to ''2x - tx^2^'', which we can compute
  * with the ring operations defined on truncations.
  */
object NewtonDivision {
  
  def getInitial(t: Double): Truncation = Truncation(t.signum) >> (log2(t.abs).ceil.toInt)
  
  def getNext(t: Truncation, x: Truncation): Truncation = (x << 1) - t * (x**2)
  
  def main(args: Array[String]): Unit = {
    printf("Input number to inverse: ")
    val t = StdIn.readDouble()
    printf("Input number of iterations to run: ")
    val iters = StdIn.readInt()
    
    val displayInput = t.toString.dropRight(t.toString.lastIndexWhere(_ != '0'))
    
    var inverse = getInitial(t)
    var i = 0
    do {
      printf("%3d iterations: 1/%s = %s\n",
        i, displayInput, inverse)
      inverse = getNext(Truncation(t), inverse)
      i = i + 1
    } while (i < iters)
    printf("%3d iterations:\t1/%s = %s\t~ %.10f",
      iters, displayInput, inverse, inverse.toDouble)
  }
}
