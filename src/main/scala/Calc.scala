class Calc {
  /* 整数の配列を取得し、それらを足し合わせた整数を返す
   *
   * Intの最大を上回った際にはオーバーフローする
   */
  def sum(seq: Seq[Int]): Int = seq.foldLeft(0)(_ + _)

  def sub(seq: Seq[Int]): Int = seq.length match{
    case 0 => 0
    case 1 => seq(0)
    case 2 => seq(0) - seq(1)
  }

  def mul(seq: Seq[Int]): Int = seq.length match {
    case 0 => 0
    case _ => seq.foldLeft(1)((x, y) => {
      val ans = x.toLong * y.toLong
      if (ans < Int.MinValue || ans > Int.MaxValue){
        throw new ArithmeticException("Int out of bounds")
      }
      ans.toInt
    } )
  }

  def div(numerator: Int, denominator: Int): Double = {
    if (denominator == 0) throw new ArithmeticException("/ by zero")
    numerator.toDouble / denominator.toDouble
  }

  def power(base: Int, exponent: Int): Double = {
    if (exponent > 0) mul(Seq.fill(exponent)(base))
    else if (exponent == 0) 1
    else {
      if (base == 0) throw new ArithmeticException("minus power by zero")
      1.0 / mul(Seq.fill(exponent * -1)(base))
    }
  }

  def factorial(num: Int): Int = {
    if (num > 0) num * factorial(num-1)
    else if (num == 0) 1
    else throw new ArithmeticException("Num must be >=0")
  }

  def abs(num: Double): Double = {
    if (num < 0) num * -1
    else num
  }

  def isPrime(num: Int): Boolean = num match {
    case 0 => false
    case 1 => false
    case x if x < 0 => throw new ArithmeticException("Cannot Insert minus num")
    case _ =>
      def divideCheck(n: Int, d: Int): Boolean = {
        if (d == 1) true
        else if (n%d == 0) false
        else divideCheck(n, d-1)
      }
      divideCheck(num,num-1)
  }

}