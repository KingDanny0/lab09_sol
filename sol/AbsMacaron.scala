package sol

import java.util.OptionalInt
import src.IMacaron

abstract class AbsMacaron(values: Array[Int]) extends IMacaron {

  protected var itemValues: Array[Int] = values

  protected var optimalValues: Array[OptionalInt] =
    new Array[OptionalInt](values.length)

  this.initTable()
  this.fillTable()

  def fillTable(): Unit

  def initTable(): Unit = {
    if (this.optimalValues.length > 0) {
      this.optimalValues(0) = OptionalInt.of(this.itemValues(0))
    }
    if (this.optimalValues.length > 1) {
      this.optimalValues(1) = OptionalInt.of((this.itemValues(1)).max(this.optimalValues(0).getAsInt))
    }
    for (i <- 2 until optimalValues.length) {
      optimalValues(i) = OptionalInt.empty()
    }
  }

  /**
   * Given a filled in table, returns the optimal value
   *
   * @return the optimal value given an array of optimal values
   */

  override def optimizeValue(): Int =
    if (this.optimalValues.length == 0) {
      0
    } else {
      this.optimalValues(this.optimalValues.length - 1).getAsInt
    }

  /**
   * Given a filled in table, reconstructs an optimal path
   *
   * @return an optimal set of items to take, represented as a boolean
   */
  override def optimizeSubSeq(): Array[Boolean] = {
    val taken: Array[Boolean] = Array.ofDim[Boolean](this.itemValues.length)
    if (taken.length <= 2) {
      if (taken.length == 1) {
        taken(0) = true
      } else if (taken.length == 2) {
        if (this.itemValues(0) >= this.itemValues(1)) {
          taken(0) = true
          taken(1) = false
        } else {
          taken(0) = false
          taken(1) = true
        }
      }
      return taken
    }

    var target: Int = this.optimalValues(this.itemValues.length - 1).getAsInt

    var i: Int = this.itemValues.length - 1

    for (i <- this.itemValues.length - 1 until 1 by -1) {
      if (target == this.optimalValues(i - 1).getAsInt) {
        taken(i) = false
      } else if (target ==
        this.optimalValues(i - 2).getAsInt + this.itemValues(i)) {
        taken(i) = true
        target = target - this.itemValues(i)
      }
    }

    if (target == this.itemValues(1) && !taken(2)) {
      taken(0) = false
      taken(1) = true
    } else if (target == this.itemValues(0)) {
      taken(0) = true
      taken(1) = false
    } else {
      throw new IllegalStateException("No valid optimal subsequence.")
    }
    taken
  }

}