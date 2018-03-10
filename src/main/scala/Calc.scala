class Calc {
  /* 整数の配列を取得し、それらを足し合わせた整数を返す
   *
   * Intの最大を上回った際にはオーバーフローする
   */
  def sum(seq: Seq[Int]): Int = seq.foldLeft(0)((x, y) => x + y )

  def mul(seq: Seq[Int]): Any = seq.length match {
    case 0 => 0
    case _ => seq.foldLeft(1)((x, y) => {
      val ans = x.toLong * y.toLong
      if (ans > Int.MaxValue){
        throw new ArithmeticException("Overflow, Error")
      }
      ans.toInt
    } )
  }
}