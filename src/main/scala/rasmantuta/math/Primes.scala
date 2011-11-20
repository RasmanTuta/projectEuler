package rasmantuta.math

import collection.mutable.BitSet

class Primes(max:Int) {
  val primesSet:BitSet = new BitSet(max)

  for (i <- 2 to max ){primesSet.add(i)}

  val end = (scala.math.sqrt(max) + 1).toInt
  for (j <- 2 until end ){
      if (isPrime(j)){
          for (k <- j * 2 to max by j){
              primesSet.remove(k)
          }
      }
  }
  def isPrime(number:Int):Boolean = {
    primesSet.contains(number)
  }
}
