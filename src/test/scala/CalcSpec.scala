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

  "sub関数" should "整数を2つ受け取り、前の数からあとの数を引いた値を返すことができる" in {
    assert(calc.sub(Seq(2,3)) === -1)
    assert(calc.sub(Seq(5,4)) === 1)
    assert(calc.sub(Seq(5,-4)) === 9)
    assert(calc.sub(Seq(-5,-4)) === -1)
    assert(calc.sub(Seq(0)) === 0)
    assert(calc.sub(Seq(4)) === 4)
    assert(calc.sub(Seq()) === 0)
  }

  it should "Intの最大を上まった際にはオーバーフローする" in {
    assert(calc.sub(Seq(Integer.MAX_VALUE, -1)) === Integer.MIN_VALUE)
  }

  it should "Intの最小を下まった際にはアンダーフローする" in {
    assert(calc.sub(Seq(Integer.MIN_VALUE, 1)) === Integer.MAX_VALUE)
  }

  "mul関数" should "整数の配列を取得し、それらを掛け合わせた整数を返すことができる" in {
    assert(calc.mul(Seq(2,3,4)) === 24)
    assert(calc.mul(Seq(2,3,0)) === 0)
    assert(calc.mul(Seq(0)) === 0)
    assert(calc.mul(Seq(-1,1)) === -1)
    assert(calc.mul(Seq(-2,-3)) === 6)
    assert(calc.mul(Seq()) === 0)
  }

  it should "Intの最大を上まった際にはエラーメッセージを出す" in {
    intercept[ArithmeticException]{
      calc.mul(Seq(Integer.MAX_VALUE, 2))
    }
  }

  it should "Intの最小を上まった際にはエラーメッセージを出す" in {
    intercept[ArithmeticException]{
      calc.mul(Seq(Integer.MIN_VALUE, 2))
    }
  }
}