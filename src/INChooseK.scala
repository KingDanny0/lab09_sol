package src

trait INChooseK {
  /**
   * Computes n choose k
   *
   * @param n - the number of elements in the set
   * @param k - the number of elements to choose from the set. k should be less
   *          than or equal to n
   * @return n choose k
   */
  def choose(n: Int, k: Int): Int

}