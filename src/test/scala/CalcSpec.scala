import org.scalatest._

class CalcSpec extends FlatSpec with DiagrammedAssertions {

  val calc = new Calc

  "sum関数" should "整数の配列を取得し、それらを足し合わせた整数を返すことができる" in {
    assert(calc.sum(Seq(1,2,3)) === 6)
    assert(calc.sum(Seq(0)) === 0)
    assert(calc.sum(Seq(-1,1)) === 0)
    assert(calc.sum(Seq()) === 0)
  }

  it should "Intの最大を上まった際にはオーバーフローする" in {
    assert(calc.sum(Seq(Integer.MAX_VALUE, 1)) === Integer.MIN_VALUE)
  }

  "mul関数" should "整数の配列を取得し、それらを掛け合わせた整数を返すことができる" in {
    assert(calc.mul(Seq(2,3,4)) === 24)
    assert(calc.mul(Seq(2,3,0)) === 0)
    assert(calc.mul(Seq(0)) === 0)
    assert(calc.mul(Seq(-1,1)) === -1)
    assert(calc.sum(Seq(-2,-3)) === 6)
    assert(calc.sum(Seq()) === 0)
  }

  it should "Intの最大を上まった際にはオーバーフローする" in {
    assert(calc.mul(Seq(Integer.MAX_VALUE, 2)) === Integer.MIN_VALUE)
  }
}