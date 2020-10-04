package category

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite

trait FinCatTests extends AnyFunSuite with GivenWhenThen { this: AnyFunSuite =>

  def verify_finite_category(c: FiniteCat, name: String) = {

    test(name ++ ": Check iterators") {
      c.objIter.foreach(obj => assert(c.objsContain(obj)));
      c.arrIter.foreach(arr => assert(c.arrsContain(arr)));
    }

    test(name ++ ": Test id objects") {
      c.objIter.foreach(obj => {
        val idObj : c.TArr = c.id(obj);
        // info(s"  Check: c.id($obj) \t= $idObj")
        assert(c.arrsContain(idObj));
        assert(c.dom(idObj) === obj);
        assert(c.cod(idObj) === obj);
      })
    }

    test(name ++ ": Test identity laws") {
      c.arrIter.foreach(arr => {
        // info(s"  Check: ${arr} : ${c.dom(arr)}->${c.cod(arr)}")
        assert(c.comp(arr, c.id(c.dom(arr))) === arr);
        assert(c.comp(c.id(c.cod(arr)), arr) === arr);
      })
    }

    test(name ++ ": Test composition") {
      c.arrIter.foreach(f =>
        c.arrIter.foreach(g =>
          if (c.cod(f) == c.dom(g)) {
            val res = c.comp(g, f);
            // info(s"  Check: comp($g, $f) \t= $res");
            assert(c.arrsContain(res));
            assert(c.dom(res) === c.dom(f));
            assert(c.cod(res) === c.cod(g));
          }
        )
      )
    }

    test(name ++ ": Test associativity") {
      c.arrIter.foreach(f =>
        c.arrIter.foreach(g =>
          c.arrIter.foreach(h =>
            if (c.cod(f) == c.dom(g) && c.cod(g) == c.dom(h)) {
              assert(c.comp(h, c.comp(g, f)) == c.comp(c.comp(h, g), f))
            }
          )
        )
      )
    }
  }

  def verify_CatWithInitial(c: CatWithInitial with FiniteCat, name: String) = {

    test(name ++ ": Check out arrows of initial object") {
      c.objIter.foreach(obj => {
        val outArr = c.io_univ(obj);
        assert(c.dom(outArr) == c.initialObj);
        assert(c.cod(outArr) == obj);
        // check uniqueness
        c.arrIter.filter(arr => (c.dom(arr) == c.initialObj && c.cod(arr) == obj))
          .foreach(arr => assert(arr == outArr, "not unique"));
      })
    }
  }

}


/*
trait SCatTests extends AnyFunSuite with GivenWhenThen { this: AnyFunSuite =>

  def verify_finite_category(c: SCategory, name: String) = {

    test(name ++ ": Test id objects (check containment and ends)") {
      c.objects.foreach(obj => {
        val idObj : c.TArr = c.id(obj);
        // info(s"Check: c.id($obj) \t= $idObj")
        assert(c.arrows.contains(idObj));
        assert(c.dom(idObj) === obj);
        assert(c.cod(idObj) === obj);
      })
    }

    test(name ++ ": Test identity laws (check composition of each arrow with ids)") {
      c.arrows.foreach(arr => {
        // info(s"Check: ${arr} : ${c.dom(arr)}->${c.cod(arr)}")
        assert(c.comp(arr, c.id(c.dom(arr))) === arr);
        assert(c.comp(c.id(c.cod(arr)), arr) === arr);
      })
    }

    test(name ++ ": Test composition (check containment and ends)") {
      c.arrows.foreach(f =>
        c.arrows.foreach(g =>
          if (c.cod(f) == c.dom(g)) {
            val res = c.comp(g, f);
            // info(s"Check: comp($g, $f) \t= $res");
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
 */


class FinCatSpec extends AnyFunSuite with FinCatTests {
  testsFor(verify_finite_category(cat323, "cat323"));
  testsFor(verify_finite_category(cat323b, "cat323b"));
  testsFor(verify_finite_category(cat323c, "cat323c"));
  testsFor(verify_finite_category(categories.boolCat, "boolCat"));
  testsFor(verify_finite_category(dualFns.dualFinCat(categories.boolCat), "boolCatDual"));
  testsFor(verify_finite_category(emptyCat, "emptyCat"));
  testsFor(verify_finite_category(unitCat, "unitCat"));
  testsFor(verify_CatWithInitial(categories.boolCat, "boolCat"));


}
