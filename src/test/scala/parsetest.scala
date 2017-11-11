/*
import minitest._

object parsetest extends SimpleTestSuite {

  test("should parse"){

    // SKK === I
    assertResult[Expression](CombI().expand){
      parser.parse("(λxyz.xz(xz))(λxy.x)λxy.x").normalForm
    }

    // 2^3 === 8
    assertResult[Expression](arith.church(8)){
      parser.parse("(λfz.f(f(fz)))λfz.f(fz)").normalForm
    }

    // macros
    assertResult[Expression](Var("x")){
      parser.parse("Z=SKI;Zx").normalForm
    }
  }

  test("ignore comments"){
    assertResult[Expression](CombI().expand){
      parser.parse("# ignore this\n λ f . f").normalForm
    }
  }
}
*/