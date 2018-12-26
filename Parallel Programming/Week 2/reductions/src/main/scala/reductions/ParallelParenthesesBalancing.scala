package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceReq(currentIndex: Int, openParamsFound: Int) : Boolean = 
      if(openParamsFound < 0) false
      else if(currentIndex == chars.size) openParamsFound == 0
      else chars(currentIndex) match {
        case '(' => balanceReq(currentIndex + 1, openParamsFound + 1)
        case ')' => balanceReq(currentIndex + 1, openParamsFound - 1)
        case _ => balanceReq(currentIndex + 1, openParamsFound)
      }
    balanceReq(0, 0)  
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, count: Int, min: Int) : (Int, Int) = 
      if(idx == until) (count, min)
      else chars(idx) match {
        case '(' => traverse(idx + 1, until, count + 1, min)
        case ')' => traverse(idx + 1, until, count - 1, (count - 1).min(min))
        case _ => traverse(idx + 1, until, count, min)
      }
    

    def reduce(from: Int, until: Int) : (Int, Int) = 
      if(until - from <  threshold) traverse(from, until, 0, 0)
      else {
        val ((count1, min1), (count2, min2)) = parallel(
          traverse(from, until - (until - from)/2, 0, 0),
          traverse(until - (until - from)/2, until, 0, 0)
        )
        (count1 + count2, min1 min (count1 - min2))
      }
    

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
