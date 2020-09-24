package category

/*
 * FinSetCat
 *   `objects` is a set of strings
 *   `arrows` is a set of (dom, name, cod) triples, including
 *     the identity arrows of the form (o, o, o) for each object o
 *     any arrows needed for closure
 */

class FinSetCat[TObjBase] extends Category with CatWithInitial {
  override type TObj = Set[TObjBase];
  override type TArr = (TObj, TObjBase => TObjBase, TObj);
  override def dom = (arr: TArr) => arr._1;
  override def cod = (arr: TArr) => arr._3;
  override def id = (s: TObj) => (s, (x: TObjBase) => x, s);
  override def comp = { case((gd, gf, gc), (fd, ff, fc)) => (fd, ff.andThen(gf), gc) }
  // Initial object
  override val initialObj = Set[TObjBase]();
  override def io_outArr = (s: this.TObj) => (initialObj, (x: TObjBase) => x, s);
} 

/* TBD !!
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
