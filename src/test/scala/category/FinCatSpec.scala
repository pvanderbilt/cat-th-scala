package category

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite

trait FinCatTests extends AnyFunSuite with GivenWhenThen { this: AnyFunSuite =>

  def verify_finite_category(c: FiniteCat, name: String) = {

    test(name ++ ": Test id objects") {
      c.objects.foreach(obj => {
        val idObj : c.TArr = c.id(obj);
        Given(s"c.id($obj) = $idObj, check containment and ends")
        assert(c.arrows.contains(idObj));
        assert(c.dom(idObj) === obj);
        assert(c.cod(idObj) === obj);
      })
    }

    test(name ++ ": Test identity laws") {
      c.arrows.foreach(arr => {
        Given(s"an arrow $arr : ${c.dom(arr)}->${c.cod(arr)}, check composition with ids")
        assert(c.comp(arr, c.id(c.dom(arr))) === arr);
        assert(c.comp(c.id(c.cod(arr)), arr) === arr);
      })
    }

    test(name ++ ": Test composition") {
      c.arrows.foreach(f =>
        c.arrows.foreach(g =>
          if (c.cod(f) == c.dom(g)) {
            val res = c.comp(g, f);
            Given(s"comp($g, $f) = $res, check containment and ends");
            assert(c.arrows.contains(res));
            assert(c.dom(res) === c.dom(f));
            assert(c.cod(res) === c.cod(g));
          }
          //else
          //  Given(s"$g and $f are incompatible")
        )
      )
    }

  }

}

class FinCatSpec extends AnyFunSuite with FinCatTests {
  testsFor(verify_finite_category(cat323, "cat323"));
  testsFor(verify_finite_category(cat323b, "cat323b"));
}
