package category

/*
 * Functor
 *    `TObj` and `TArr` are the Scala base types for object and arrows.
 *    A given category's objects may be all of TObj or some subset drawn from TObj;
 *    similarly for arrows.
 *    The remaining operations are those for the category.
 */


trait Functor {
  val DomC: Category;
  val CodC: Category;
  def objMap: DomC.TObj => CodC.TObj;
  def arrMap: DomC.TArr => CodC.TArr;
}

class IdFunctor (val C: Category) extends Functor {
  val DomC : Category { type TObj = C.TObj; type TArr = C.TArr } = C;
  val CodC : Category { type TObj = C.TObj; type TArr = C.TArr } = C ;
  val objMap = (obj: C.TObj) => obj;
  val arrMap = (arr: C.TArr) => arr;
}

/*
class DualFunctor (val F: Functor) extends Functor {
 // val C = new Dual(F.C);
 // val D = new Dual(F.D);
  val C = new Dual(F.C);
  val D = new Dual(F.D);
  def objMap = F.objMap;
  def arrMap = F.arrMap;
}
 */
/*
class DualFunctor2 (val F: Functor) extends Functor {
  val C = new Dual2  { val SrcCat = F.C };
  val D = new Dual2  { val SrcCat = F.D };
  def objMap = F.objMap;
  def arrMap = F.arrMap;
}

 */

trait Functor2 {
  type CObj;
  type CArr;
  type DObj;
  type DArr;
  val C: Category { type TObj = CObj; type TArr = CArr };
  val D: Category { type TObj = DObj; type TArr = DArr };
  def objMap: CObj => DObj;
  def arrMap: CArr => DArr;
}

/*
class DualFunctor2 (val F: Functor2) extends Functor2 {
  type CObj = F.CObj;
  type CArr = F.CArr;
  type DObj = F.DObj;
  type DArr = F.DArr;
  val C = new Dual(F.C);
  val D = new Dual(F.D);
  def objMap = F.objMap;
  def arrMap = F.arrMap;
}
 */
