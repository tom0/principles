package recfun

import scala.annotation.tailrec

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
      @tailrec
      def makeRow(ns: List[Int], row: List[Int] = List(1)): List[Int] = ns match {
        case List(1) => 1::row
        case x::y::xs => makeRow(y::xs, (x+y)::row)
      }

      def innerPascal(s: Int): List[Int] = s match {
        case 0 => List(1)
        case 1 => List(1, 1)
        case x => makeRow(innerPascal(s - 1))
      }

      innerPascal(r)(c)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def innerBalance(cs: List[Char], level: Int = 0): Int = cs match {
            case _ if level < 0 => level
            case Nil => level
            case '('::xs => innerBalance(xs, level+1)
            case ')'::xs => innerBalance(xs, level-1)
            case _::xs => innerBalance(xs, level)
          }
      innerBalance(chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def innerCountChange(m: Int = money, cs: List[Int] = coins): Int = cs match {
        case _ if m < 0 => 0
        case _ if m == 0 => 1
        case Nil => 0
        case x::xs => innerCountChange(m, xs) + innerCountChange(m - x, x::xs)
      }
      innerCountChange()
    }
  }
