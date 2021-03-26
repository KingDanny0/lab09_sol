package src

/**
 * Trait for classes solving the Macaron problem. Has two methods,
 * optimizeValue and optimizeSubSeq, which return the optimal value and optimal
 * subsequence from the presenting sequence of tasty values, respectively.
 */
trait IMacaron {

  /**
   * @return - An int representing the optimal value achievable with the given
   *         data.
   */
  def optimizeValue(): Int

  /**
   * @return - A boolean array, representing the indices used in reaching the
   *         optimal value.
   */
  def optimizeSubSeq(): Array[Boolean]
}