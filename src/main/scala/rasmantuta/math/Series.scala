package rasmantuta.math


object Series {
  lazy val fibonacci : Stream[Int] = 0 #:: 1 #:: fibonacci.zip(fibonacci.tail).map(p => p._1 + p._2)
  lazy val bigFibonacci : Stream[BigInt] = 0 #:: 1 #:: bigFibonacci.zip(bigFibonacci.tail).map(p => p._1 + p._2)
  lazy val primes : Stream[Int] = 2 #:: primeSieve(3)
  lazy val triangles: Stream[Int] = 1 #:: triangles.zipWithIndex.map(i => i._1 + i._2 + 2)

  
  private def primeSieve(p: Int): Stream[Int] = {
       p #:: primeSieve(
            Stream.from(p + 2, 2).
             find(i=> primes.takeWhile(j => j * j <= i).
                     forall(i % _ > 0)).get)
  }

  def collatzConjecture(n: Long): List[Long] = {
    Some(n) match {
      case Some(1) => List(1)
      case Some(x) if(x%2==0) => List(n) ++ collatzConjecture(n/2)
      case Some(_) => List(n) ++ collatzConjecture(3*n+1)
    }
  }

}
