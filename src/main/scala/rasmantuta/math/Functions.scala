package rasmantuta.math

import collection.Seq

//   Copyright 2011 Kristian Berg (RasmanTuta)
//
//   The License is...
//   ...there is no License

object Functions {
  def factors(value: Int): Set[Int] = {
    factors(value, 1, Set())
  }

  def factors(value: Int, possibleFactor: Int, theFactors: Set[Int]): Set[Int] = {
    (value, possibleFactor, theFactors) match{
      case(v, c, _) if(v < (c*c)) => theFactors
      case (_, c, f) if(f.contains(c)) => theFactors
      case (v, c, _) if(v % c == 0) => factors (value, possibleFactor+1, theFactors + possibleFactor + value/possibleFactor)
      case (_, _, _) => factors(value, possibleFactor+1, theFactors)
    }
  }

  def binomialCoefficient(n: Int, k: Int): BigInt = {
    val nom = ((n - k + 1) to n).map(BigInt(_)).reduceLeft(_ * _)
    val denom = (1 to k).map(BigInt(_)).reduceLeft(_ * _)
    nom/denom
  }

  def findTrianglePathValue(triangle: Seq[Int])(operation: (Int, Int) => Int)(selection: (Int, Int) => Int): Int = {

    val tNumbers = Series.triangles.takeWhile(f => f <= triangle.size)
    if(tNumbers.last != triangle.size) throw new IllegalArgumentException("triangle: Seq[Int] must have the size of a triangle number.")

    val lines = (0 +: tNumbers).zip(tNumbers).map(f => triangle.slice(f._1, f._2))
    
    def compute(done:Seq[Int], todo:Seq[Seq[Int]]): Seq[Int] = {
      val v: Seq[Int] = todo.head
      val result: Seq[Int] = ((done :+ done.last).zip(v).map(f => operation(f._1, f._2))).zip((done.head +: done).zip(v).map(f => operation(f._1, f._2))).map(f => selection(f._1, f._2))
      todo.size match{
        case 1 => result
        case _ => compute(result, todo.tail)
      }
    }
    compute(lines.head, lines.tail).reduce(selection(_, _))
  }

  def properDivisors(value: Int)  = {factors(value) - value}
}
