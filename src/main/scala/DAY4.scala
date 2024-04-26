import scala.annotation.tailrec

object DAY4 {

  def genParentheses(n: Int): List[String] = {
    def genHelper(open: Int, close: Int, current: String): List[String] = {
      if (open == n && close == n) {
        List(current)
      } else {
        var res = List[String]()
        if (open < n)
          res = res ++ genHelper(open + 1, close, current + "(")
        if (close < open) {
          res = res ++ genHelper(open, close + 1, current + ")")
        }
        res
      }
    }
    genHelper(0, 0, "")
  }

  def main(args: Array[String]): Unit = {
    val n = 3
    val comb = genParentheses(n)
    val validcomb = comb.filter(DAY4.isValid)
    validcomb.foreach(println)
  }

  def isValid(str: String): Boolean = {
    @tailrec
    def isValidHelper(str: String, balanced: Int): Boolean = {
      if (balanced < 0) false
      else if (str.isEmpty) balanced == 0
      else if (str.head == '(') isValidHelper(str.tail, balanced + 1)
      else isValidHelper(str.tail, balanced - 1)
    }
    isValidHelper(str, 0)
  }
}
