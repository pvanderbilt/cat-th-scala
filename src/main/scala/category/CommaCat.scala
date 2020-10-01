package category

/*
 * Comma Category: See CCT, Section 5.1
 */

class CommaCat (val L: Functor, val R: Functor) extends Category {
  // Note it should be that L.CodC = R.CodC
  val A = L.DomC
  val B = R.DomC
  val C = L.CodC

  case class TObj (a: L.DomC.TObj, f: L.CodC.TArr, b: R.DomC.TObj);
  case class TArr (dom: TObj, s: L.DomC.TArr, t: R.DomC.TArr, cod: TObj) {

    def checkCommutes: Boolean = {
      val ft : L.CodC.TArr = this.cod.f;
      val ls : L.CodC.TArr = L.arrMap(this.s);
      val fb : L.CodC.TArr = this.dom.f;
      val rt : L.CodC.TArr = R.arrMap(this.t).asInstanceOf[L.CodC.TArr];
      val fbls = L.CodC.comp(this.dom.f, L.arrMap(this.s));
      var rtft = L.CodC.comp(rt, this.cod.f);
      (fbls == rtft)
    }

  }
 
  def dom  = _.dom;
  def cod  = _.cod;
  def id   = { case obj =>
    TArr(obj, L.DomC.id(obj.a), R.DomC.id(obj.b), obj)
  };
  def comp = { case (g, f) =>
    TArr(f.dom, L.DomC.comp(g.s, f.s), R.DomC.comp(g.t, f.t), g.cod)
  };
}

trait CategoryC [TObj, TArr] {
  def dom: TArr => TObj;
  def cod: TArr => TObj;
  def id:  TObj => TArr;
  def comp: (TArr, TArr) => TArr;
}

trait FunctorC [DomObj, DomArr, CodObj, CodArr] {
  val DomC: CategoryC [DomObj, DomArr];
  val CodC: CategoryC [CodObj, CodArr];
  def objMap: DomObj => CodObj;
  def arrMap: DomArr => CodArr;
}


class CommaCat2 [AObj, AArr, BObj, BArr, CObj, CArr]
  (val L: FunctorC[AObj, AArr, CObj, CArr],
   val R: FunctorC[BObj, BArr, CObj, CArr]
  ) extends Category {

  // val A : Category { type TObj = AObj; type TArr = AArr } = L.DomC
  // val B : Category { type TObj = BObj; type TArr = BArr } = R.DomC
  // val C : Category { type TObj = CObj; type TArr = CArr } = L.CodC
  val A : CategoryC [AObj, AArr] = L.DomC
  val B : CategoryC [BObj, BArr] = R.DomC
  val C : CategoryC [CObj, CArr] = L.CodC
  // which should equal R.DomC

  case class TObj (a: AObj, f: CArr, b: BObj);
  case class TArr (dom: TObj, s: AArr, t: BArr, cod: TObj) {
    def checkCommutes: Boolean = {
      val ft : CArr = this.cod.f;
      val ls : CArr = L.arrMap(this.s);
      val fb : CArr = this.dom.f;
      val rt : CArr = R.arrMap(this.t);
      C.comp(fb, ls) == C.comp(rt, ft)
    }
  }

  def dom  = ???;
  def cod  = ???;
  def id   = ???;
  def comp = ???;


}

