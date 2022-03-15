package PrintScript.parsing

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TokenSuite extends AnyFunSuite  {
  test("sameTypeTokensShouldBeEqual") {
    val t1 = new AssignationType
    val t2 = new AssignationType
    assert(t1.equals(t2))
  }
}
