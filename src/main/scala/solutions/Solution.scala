package solutions

trait Solution {
  def main(args: Array[String]) {
    val start = System.currentTimeMillis
    println (solveIt:Any)
    println("Solved in: " + (System.currentTimeMillis() - start) / 1000.0 + " seconds")
  }
  def solveIt:Any = {}
}
