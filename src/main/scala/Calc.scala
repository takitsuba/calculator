class Calc {
  /* 整数の配列を取得し、それらを足し合わせた整数を返す
   *
   * Intの最大を上回った際にはオーバーフローする
   */
  def sum(seq: Seq[Int]): Int = seq.foldLeft(0)((x, y) => x + y )

  def sub(seq: Seq[Int]): Int = seq.length match{
    case 0 => 0
    case 1 => seq(0)
    case 2 => seq(0) - seq(1)
  }

  def mul(seq: Seq[Int]): Any = seq.length match {
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
}