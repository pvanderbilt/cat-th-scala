package category


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
