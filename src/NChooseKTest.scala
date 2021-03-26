package src

import tester.Tester

object NChooseKTest {

  /**
    * Tests both the bottom-up and top-down implementation on a particular
    * n-choose-k.
    *
    * @param n - the n of n choose k
    * @param k - the k of n choose k
    * @param expected - the expected value of n choose k
    * @param t - the tester
    */
  private def doTestNChooseK(n: Int, k: Int, expected: Int, t: Tester): Unit = {
    t.checkExpect(new BUNChooseK(n).choose(n, k), expected)
    t.checkExpect(new TDNChooseK(n).choose(n, k), expected)
  }

  /**
    * Runs tests on our dp implementations by calling the above helper method.
    *
    * @param t - the tester
    */
  def testNChooseK(t: Tester): Unit = {
    for (n <- 0.until(5)) {
      doTestNChooseK(n, 0, 1, t)
    }
    for (n <- 0.until(5)) {
      doTestNChooseK(n, n, 1, t)
    }
// General cases
    doTestNChooseK(4, 2, 6, t)
    for (n <- 1.until(5)) {
      doTestNChooseK(n, 1, n, t)
    }
  }

  def main(args: Array[String]): Unit = {
    Tester.run(NChooseKTest)
  }

}

