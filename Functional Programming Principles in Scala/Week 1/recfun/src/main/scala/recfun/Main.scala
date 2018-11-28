package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = 
      if(c == 0 || r <= c) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balRec(openParams: Int, chars: List[Char]) : Boolean = 
        chars match {
          case Nil => openParams == 0
          case '(' :: xs => balRec(openParams + 1, xs)
          case ')' :: xs if openParams > 0 =>  balRec(openParams - 1, xs)
          case ')' :: xs => false
          case _ :: xs => balRec(openParams, xs)
        }
        balRec(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = 
      if(money == 0) 1
      else if(coins.isEmpty) 0
      else if(money < coins.head) countChange(money, coins.tail)
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
