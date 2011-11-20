package solutions

import collection.mutable.BitSet
import rasmantuta.math.{Primes, Functions, Series}
import scala.Int

object Solution_357 extends Solution{
//  1739023853137
//  Solved in: 13.089 seconds
  override def solveIt:Any = {
    val max = 100000001
    val primes = new Primes(max)
    val nums = primes.primesSet.par.filter(p => {
      val x=p-1
      forAllFactors(x, 1, primes)
    })
    nums.map(_.toLong).sum - nums.size
  }
  def forAllFactors(value:Int, possibleFactor:Int, primes:Primes):Boolean = {
     (value, possibleFactor) match{
       case(v, pf) if(v < (pf * pf)) => true
       case(v, pf) if(v % pf  == 0)=> if(test(v, pf, primes) && test(v, v/pf, primes)) forAllFactors(v, pf + 1, primes) else false
       case _ => forAllFactors(value, possibleFactor + 1, primes)
     }
  }
  def test(value:Int, factor:Int, primes: Primes):Boolean ={
    primes.isPrime(factor + value / factor)
  }
}
