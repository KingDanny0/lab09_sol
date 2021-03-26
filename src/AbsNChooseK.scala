package src

import java.util.OptionalInt

/**
  * An abstract class that computes n choose k using dynamic programming.
  */
abstract class AbsNChooseK (protected var n: Int) extends INChooseK {

  /**
    * The table containing Pascal's triangle, where c[i][j] equals i choose j.
    */
  protected var c: Array[Array[OptionalInt]] =
    Array.ofDim[OptionalInt](this.n + 1, this.n + 1)

  if (this.n < 0) {
    println(
      "You input " + this.n +
        ", but Pascal's triangle cannot have a negative dimension.")
    System.exit(-1)
  }

  initTable()

  fillTable()

  /**
   * Initializes the optimal values table with base cases, and 0 otherwise
   */
  private def initTable(): Unit = {
    for (i <- 0 to this.n) {
      for (j <- 0 to i) {
        if (j == 0 || j == i) {
          // Initialize the base cases to 1
          this.c(i)(j) = OptionalInt.of(1)
        } else {
          // General case: initialize the rest of the table to default value
          // which means the corresponding entry in Pascal's triangle has not
          // yet been computed
          this.c(i)(j) = OptionalInt.empty()
        }
      }
    }
  }

  /**
    * Fills in the table in either BU or TD manner.
    */
  protected def fillTable(): Unit

  override def choose(n: Int, k: Int): Int = {
    if (k > n || n > this.n) {
      println(
        "You cannot choose " + k + " elements from a set of size " +
          n)
    }
    this.c(n)(k).getAsInt
  }

}