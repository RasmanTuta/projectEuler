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

  def factors(va: Int, cu: Int, fo: Set[Int]): Set[Int] = {
    (va, cu, fo) match{
      case(v, c, _) if(v < c) => fo
      case (_, c, f) if(f.contains(c)) => fo
      case (v, c, _) if(v % c == 0) => factors (va, cu+1, fo + cu + va/cu)
      case (_, _, _) => factors(va, cu+1, fo)
    }
  }


}
