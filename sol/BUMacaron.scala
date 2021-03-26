package sol
import java.util.OptionalInt

class BUMacaron(values: Array[Int]) extends AbsMacaron(values) {

  override def fillTable(): Unit = {
    for (i <- 2 until this.itemValues.length) {
      this.optimalValues(i) = OptionalInt.of(
        Math.max(this.optimalValues(i - 2).getAsInt + this.itemValues(i),
          this.optimalValues(i - 1).getAsInt))
    }
  }

}