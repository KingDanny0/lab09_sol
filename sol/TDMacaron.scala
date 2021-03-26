package sol
import java.util.OptionalInt

class TDMacaron(values: Array[Int]) extends AbsMacaron(values) {
  /**
   * Fills in the table of optimal values in a top-down fashion.
   *
   * @param index         - the array index of the value to fill in
   * @param itemValues    - item values
   * @param optimalValues - array of optimal values
   * @return the optimal value at the given index
   */
  def tdTableMakerRec(index: Int,
                      itemValues: Array[Int],
                      optimalValues: Array[OptionalInt]): Int = {

    if (!optimalValues(index).isPresent) {
      optimalValues(index) = OptionalInt.of(
        Math.max(tdTableMakerRec(index - 2, itemValues, optimalValues) +
          itemValues(index),
          tdTableMakerRec(index - 1, itemValues, optimalValues)))
    }
    optimalValues(index).getAsInt
  }


  override def fillTable(): Unit = {
    for (i <- 2 until this.itemValues.length) {
      this.optimalValues(i) = OptionalInt.empty()
    }

    if (itemValues.length > 0) {
      tdTableMakerRec(this.itemValues.length - 1,
        this.itemValues,
        this.optimalValues)
    }
  }

}