package rasmantuta.math


//   Copyright 2011 Kristian Berg (RasmanTuta)
//
//   The License is...
//   ...there is no License

import org.specs2.mutable._


class FunctionsSpec extends Specification {
   
  "The findTrianglePathValue function" should {
    "throw IllegalArgumentException when receiving not triangle sized Seq" in {
      Functions.findTrianglePathValue(1 :: 2 :: 3 :: 4 :: Nil)(_ + _)(_ max _) must throwAn[IllegalArgumentException]
    }
  }
  "The findTrianglePathValue function" should {
    "return value for the Seq(3, 7, 4, 2, 4, 6, 8, 5, 9, 3)" in {
      Functions.findTrianglePathValue(3 :: 7 :: 4 :: 2 :: 4 :: 6 :: 8 :: 5 :: 9 :: 3 :: Nil)(_ + _)(_ max _) must be_== (23)
    }
  }
  "The findTrianglePathValue function" should {
    "return value for the Seq(3, 7, 4, 2, 4, 6, 8, 5, 9, 3)" in {
      Functions.findTrianglePathValue(3 :: 7 :: 4 :: 2 :: 4 :: 6 :: 8 :: 5 :: 9 :: 3 :: Nil)(_ * _)(_ min _) must be_== (210)
    }
  }

  "The properDivisors function" should {
    "return a Set for the value 220" in {
      Functions.properDivisors(220) must haveTheSameElementsAs( Set(1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110))
    }
  }

  "The properDivisors function" should {
    "return a Set for the value 284" in {
      Functions.properDivisors(284) must haveTheSameElementsAs( Set(1, 2, 4, 71, 142))
    }
  }

}
