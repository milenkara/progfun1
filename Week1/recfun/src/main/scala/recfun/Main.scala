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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balance(chars: List[Char], open: Int): Boolean = {
      chars match {
        case Nil => open == 0
        case x :: xs => {
          if (x == '(') balance(xs, open + 1)
          else if (x == ')') open > 0 && balance(xs, open - 1)
          else {
            balance(xs, open)
          }
        }
      }
    }

    balance(chars, 0)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else {
      coins match {
        case Nil => 0
        case x::xs => {
          if(money >= x) countChange(money - x, x::xs) + countChange(money, xs)
          else countChange(money, xs)
        }
      }
    }
  }
}
