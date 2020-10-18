package category

// The category of finite sets and related

/*
 * FinSetCat[A]
 *   This corresponds to CCT's code of section 3.4.1
 *   `TObj` denotes sets of values of type A
 *   `TArr` denotes a set of (dom, A => A, cod) triples
 *    Note that `comp` does not check that the arrows are compatible.
 */

class FinSetCat [A] extends Category {
  type TObj = Set[A];
  type TArr = (TObj, A => A, TObj);
  def dom = { case (a, _, _) => a };
  def cod = { case (_, _, b) => b };
  def id = (s: TObj) => (s, (x: A) => x, s);
  def comp = { case ((gd, gf, gc), (fd, ff, fc)) => (fd, ff.andThen(gf), gc) }
  // helper functions
  def arrFunc : TArr => A => A = { case (_, f, _) => f };
} 

/*
 *  FSVal: A set of class providing values for sums, prods and the like
 */

trait FSVal;
// case object Empty extends FinSet;
case object Unit extends FSVal;
case class IntLit (n: Int) extends FSVal;
case class Left  (s: FSVal) extends FSVal;
case class Right (s: FSVal) extends FSVal;
case class Pair (s: FSVal, t: FSVal) extends FSVal;
case object Error extends FSVal;

/*
 * FSCat: A specialization of FinSetCat to FSVal
 *   It comes with initial anf final objects
 */

object FSCat extends FinSetCat[FSVal]
    with CatWithInitial with CatWithFinal with CatWithProds with CatWithCoprods
{
  // Initial object
  val initialObj = Set[FSVal]();
  def io_univ = (s: TObj) => (initialObj, (x: FSVal) => x, s);

  
  // Final object
  val finalObj = Set(Unit);
  def fo_univ = (s: TObj) => (s, (_: FSVal) => Unit, finalObj);

  // products
  def prod  = (a: TObj, b: TObj) => for (x <- a; y <- b) yield Pair(x, y)
  def projL = (a: TObj, _: TObj, axb: TObj) =>
    (axb, { case Pair(x, _) => x; case _ => Error }, a)
  def projR = (_: TObj, b: TObj, axb: TObj) =>
    (axb, { case Pair(_, y) => y; case _ => Error }, b)
  def prod_univ = (axb: TObj, c: TObj, f: TArr, g: TArr) =>
    (c, (v: FSVal) => Pair(arrFunc(f)(v), arrFunc(g)(v)), axb);

  // coproducts
  def coprod = (a: TObj, b: TObj) => a.map(Left) ++ b.map(Right)
  def injL   = (a: TObj, b: TObj, apb: TObj) => (a, Left, apb)
  def injR   = (a: TObj, b: TObj, apb: TObj) => (a, Right, apb)
  def coprod_univ = (apb: TObj, c: TObj, f: TArr, g: TArr) =>
    (apb, { case Left(v) => arrFunc(f)(v); case Right(v) => arrFunc(g)(v) }, c)
 
}



/*
 * FinSetCatPlus[A]
 *   FinSetCat [A] with the initial object specified
 */

class FinSetCatPlus [A] extends FinSetCat[A] with CatWithInitial  {
  // Initial object
  val initialObj = Set[A]();
  def io_univ = (s: TObj) => (initialObj, (x: A) => x, s);

  // with CatWithFinal
  // // Final object
  // val finalObj = Set[A](());
  // def fo_univ = (s: TObj) => (s, (_: A) => (), finalObj);
}


class FinSetCatPlus2 [A] extends FinSetCat[A] with InitialObj {
  override val io_cat = this
  // Initial object
  val initialObj = Set[A]();
  def io_univ = (s: TObj) => (initialObj, (x: A) => x, s);
}

// class FinSetCatPlus3 [A] extends FinSetCat[A] with CatWithInitial2 {
//   // Initial object
//   val initialObj = Set[A]();
//   def io_univ = (s: TObj) => (initialObj, (x: A) => x, s);
// }


object finSetOps {
  def cartProd [A,B] (s1: Set[A], s2: Set[B]): Set[(A,B)] =
    for { a <- s1; b <- s2 } yield (a, b)

}

import finSetOps._


/*
 * The χ functor on FinSetCat
 */

// class ChiFun [A] extends Functor {
//   val DomC = new FinSetCat[A];
//   val CodC = new FinSetCat[(A,A)];
//   def objMap = (a: Set[A]) => cartProd(a, a);
//   def arrMap = { case (a, f, b) =>
//     (cartProd(a, a), { case (x,y) => (f(x), f(y)) }, cartProd(b, b))
//   }
// }

object chiFun extends Functor {
  val DomC = FSCat;
  val CodC = FSCat;
  def objMap = (a: Set[FSVal]) => FSCat.prod(a, a);
  def arrMap = { case (a, f, b) =>
    (FSCat.prod(a, a), { case Pair(x, y) => Pair(f(x), f(y)) }, FSCat.prod(b, b))
  }
}

// class GraphCat [A] extends CommaCat(IdFunctor(FinSetCat[A]), ChiFun[A]);




/* TBD : Trying to et it more generic.
class FinSetCat2 extends Category with CatWithInitial {
  class TObj {
    type TObjBase;
    val obj: Set[TObjBase];
  }
  class TArr (
    val dom: TObj,
    val cod: TObj,
    val func: dom.TObjBase => cod.TObjBase
  ) {}
  // override type TObj = Set[TObjBase];
  // override type TArr = (TObj, TObjBase => TObjBase, TObj);
  override def dom = (arr: TArr) => arr.dom;
  override def cod = (arr: TArr) => arr.cod;
  override def id = (s: TObj) => new TArr (s, s, (x: s.TObjBase) => x)
  override def comp = (g: TArr, f: TArr) => (f._1, f._2.andThen(g._2), g._3);
  override val initialObj = Set[TObjBase]();
  override def io_outArr = (s: this.TObj) => (initialObj, (x: TObjBase) => x, s);

  // def id (s: TObj) = (s, (_: TObjBase), s);
  // def comp2 : (TArr, TArr) => TArr = {
  //   case((gd, gf, gc), (fd, ff, fc)) => (fd, ff.andThen(gf), gc)
  // }
} 
 */
