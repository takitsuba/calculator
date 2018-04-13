import org.scalatest._

class CalcSpec extends FlatSpec with DiagrammedAssertions {

  // val calc = new Calc

  "sum関数" should "整数の配列を取得し、それらを足し合わせた整数を返すことができる" in {
    assert(Calc.sum(Seq(1,2,3)) === 6)
    assert(Calc.sum(Seq(0)) === 0)
    assert(Calc.sum(Seq(-1,1)) === 0)
    assert(Calc.sum(Seq()) === 0)
  }

  it should "Intの最大を上まった際にはオーバーフローする" in {
    assert(Calc.sum(Seq(Integer.MAX_VALUE, 1)) === Integer.MIN_VALUE)
  }

  "sub関数" should "整数を2つ受け取り、前の数からあとの数を引いた値を返すことができる" in {
    assert(Calc.sub(Seq(2,3)) === -1)
    assert(Calc.sub(Seq(5,4)) === 1)
    assert(Calc.sub(Seq(5,-4)) === 9)
    assert(Calc.sub(Seq(-5,-4)) === -1)
    assert(Calc.sub(Seq(0)) === 0)
    assert(Calc.sub(Seq(4)) === 4)
    assert(Calc.sub(Seq()) === 0)
  }

  it should "Intの最大を上まった際にはオーバーフローする" in {
    assert(Calc.sub(Seq(Integer.MAX_VALUE, -1)) === Integer.MIN_VALUE)
  }

  it should "Intの最小を下まった際にはアンダーフローする" in {
    assert(Calc.sub(Seq(Integer.MIN_VALUE, 1)) === Integer.MAX_VALUE)
  }

  "mul関数" should "整数の配列を取得し、それらを掛け合わせた整数を返すことができる" in {
    assert(Calc.mul(Seq(2,3,4)) === 24)
    assert(Calc.mul(Seq(2,3,0)) === 0)
    assert(Calc.mul(Seq(0)) === 0)
    assert(Calc.mul(Seq(-1,1)) === -1)
    assert(Calc.mul(Seq(-2,-3)) === 6)
    assert(Calc.mul(Seq()) === 0)
  }

  it should "Intの最大を上まった際にはエラーメッセージを出す" in {
    intercept[ArithmeticException]{
      Calc.mul(Seq(Integer.MAX_VALUE, 2))
    }
  }

  it should "Intの最小を上まった際にはエラーメッセージを出す" in {
    intercept[ArithmeticException]{
      Calc.mul(Seq(Integer.MIN_VALUE, 2))
    }
  }

  "div関数" should "整数を2つ受け取り、分子を分母で割った浮動小数てんの値を返す" in {
    assert(Calc.div(6,3) === 2.0)
    assert(Calc.div(1,3) === 0.3333333333333333)
  }

  it should "0で割ろうとした際には実行時例外が投げられる" in {
    intercept[ArithmeticException]{
      Calc.div(1,0)
    }
  }

  "power関数" should "整数を2つ受け取り、1要素めを2要素乗した値を返す" in {
    assert(Calc.power(2,3) === 8)
    assert(Calc.power(3,0) === 1)
    assert(Calc.power(4,-1) === 0.25)
  }

  it should "0をマイナス累乗した際には実行時例外が投げられる" in {
    intercept[ArithmeticException]{
      Calc.power(0,-1)
    }
  }

  "factorial関数" should "整数を1つ受け取り、階乗した値を返す" in {
    assert(Calc.factorial(4) === 24)
    assert(Calc.factorial(1) === 1)
    assert(Calc.factorial(0) === 1)
  }

  it should "負の数を与えた際には例外を返す" in {
    intercept[ArithmeticException]{
      Calc.factorial(-2)
    }
  }

  "abs関数" should "Doubleを受け取り、絶対値を返す" in {
    assert(Calc.abs(2) === 2)
    assert(Calc.abs(-3) === 3)
    assert(Calc.abs(4.1) === 4.1)
    assert(Calc.abs(-5.1) === 5.1)
    assert(Calc.abs(0) === 0)
  }

  "isPrime関数" should "Intを受け取り、素数であればTrue,素数でなければFalseを返す" in {
    assert(Calc.isPrime(0) === false)
    assert(Calc.isPrime(1) === false)
    assert(Calc.isPrime(2) === true)
    assert(Calc.isPrime(3) === true)
    assert(Calc.isPrime(4) === false)
    assert(Calc.isPrime(4998) === false)
    assert(Calc.isPrime(4999) === true)
  }

  it should "負の数を与えた際には例外を返す" in {
    intercept[ArithmeticException]{
      Calc.isPrime(-2)
    }
  }
}