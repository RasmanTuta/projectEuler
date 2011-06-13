package rasmantuta.math




object Series {
  lazy val fibonacci : Stream[Int] = 0 #:: 1 #:: fibonacci.zip(fibonacci.tail).map(p => p._1 + p._2)
  lazy val primes : Stream[Int] = 2 #:: primeSieve(3)

  
  private def primeSieve(p: Int): Stream[Int] = {
       p #:: primeSieve(
            Stream.from(p + 2, 2).
             find(i=> primes.takeWhile(j => j * j <= i).
                     forall(i % _ > 0)).get)
  }

//    3 #:: primes.zip(Stream.from(primes.last + 2, 2).find( i=> primes.takeWhile(j => j * j <= i).
//                     forall(i % _ > 0)).get)
}
