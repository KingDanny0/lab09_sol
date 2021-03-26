package src

import java.util.OptionalInt

/**
 * A class that computes n choose k using top-down dynamic programming.
 *
 * @param n - the dimension of Pascal's triangle; must be non-negative
 */
class TDNChooseK(n: Int) extends AbsNChooseK(n) {

  /**
   * Fills in the table of numbers in a top-down fashion.
   */
  override protected def fillTable(): Unit = {
    for (j <- 1 until n) {
      fillTableRec(n, j)
    }
  }

  /**
   * Recursively fills in the table of numbers
   *
   * @param i - the row of the number in the table to compute
   * @param j - the column of the number in the table to compute
   * @return the number in Pascal's triangle in cell (i, j)
   */
  def fillTableRec(i: Int, j: Int): Int = {
    if (!this.c(i)(j).isPresent) {
      this.c(i)(j) =
        OptionalInt.of(fillTableRec(i - 1, j - 1) + fillTableRec(i - 1, j))
    }
    this.c(i)(j).getAsInt
  }

}