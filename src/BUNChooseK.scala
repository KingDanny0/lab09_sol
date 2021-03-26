package src

import java.util.OptionalInt

/**
 * A class that computes n choose k using bottom-up dynamic programming
 *
 * @param n - the dimension of Pascal's triangle; must be non-negative
 */
class BUNChooseK(n: Int) extends AbsNChooseK(n) {

  /**
   * Fills in the table of numbers in a bottom-up fashion.
   */
  protected override def fillTable(): Unit = {
    for (i <- 2 to n) {
      for (j <- 1 until i) {
        this.c(i)(j) = OptionalInt.of(
          this.c(i - 1)(j - 1).getAsInt + this.c(i - 1)(j).getAsInt)
      }
    }
  }
}