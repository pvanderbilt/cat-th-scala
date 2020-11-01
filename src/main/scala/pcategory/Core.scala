package pcategory.core

/*
 * This file has:
 * Definitions of categorical things, parameterized by the types of objects and arrows
 */

// -------------------------------- CATEGORY --------------------------------

/*
 * Basic Category definition
 *    `TObj` and `TArr` are the Scala base types for object and arrows.
 */

trait Category [TObj, TArr] {
  def dom: TArr => TObj;
  def cod: TArr => TObj;
  def id:  TObj => TArr;
  def comp: (TArr, TArr) => TArr;

  def checkId(x: TObj): Boolean =
    dom(id(x)) == x && cod(id(x)) == x

  def checkUnits(f: TArr): Boolean =
    comp(f, id(dom(f))) == f && comp(id(cod(f)), f) == f
}

// -------------------------------- Category operations --------------------------------

object catOps {

  def dualCat[TObj, TArr] (C: Category[TObj, TArr]): Category[TObj, TArr] =
    new Category[TObj, TArr] {
      def dom = C.cod
      def cod = C.dom
      def id  = C.id
      def comp = { case (g, f) => C.comp(f, g) }
    }

}

// -------------------------------- CATEGORIES --------------------------------
/*
 * FinSetCat: Category of finite sets
 */

class FinSetCat [A] extends Category[Set[A], (Set[A], A => A, Set[A])]  {
  type TObj = Set[A];
  type TArr = (TObj, A => A, TObj);
  def dom = { case (a, _, _) => a };
  def cod = { case (_, _, b) => b };
  def id = (s: TObj) => (s, (x: A) => x, s);
  def comp = { case ((gd, gf, gc), (fd, ff, fc)) => (fd, ff.andThen(gf), gc) }
  // helper functions
  def arrFunc : TArr => A => A = { case (_, f, _) => f };
} 

