package pcategory.cctth4

import pcategory.core._
import pcategory.functor._
import pcategory.limits._

/*
 * CCT Theorem 4, section 5.5, p 129: 
 *   For C and cocomplete D, their functor cat is cocomplete
 *     So, forall cats C & D, where D is cocomplete,
 *     FC_Cocomplete(C, D) is a subclass of FunctorCat(C, D) 
 *     that implements Cocomplete
 */

class FC_Cocomplete [C_TObj, C_TArr, D_TObj, D_TArr] (
  C: Category[C_TObj, C_TArr],
  D: Category[D_TObj, D_TArr] with CocompleteCat[D_TObj, D_TArr]
)
    extends FunctorCat(C, D) with CocompleteCat[Functor[C_TObj, C_TArr, D_TObj, D_TArr],
      NatTrans[C_TObj, C_TArr, D_TObj, D_TArr]]
{  FC =>

  // aliases for the functor cat's objects and arrows
  type FC_TObj = Functor[C_TObj, C_TArr, D_TObj, D_TArr]
  type FC_TArr = NatTrans[C_TObj, C_TArr, D_TObj, D_TArr]

  def diagAt (a: C_TObj, d: Diagram[FC_TObj, FC_TArr]) : Diagram[D_TObj, D_TArr] =
    new Diagram[D_TObj, D_TArr] {
      def nodes = d.nodes.map(F => F.objMap(a))
      def edges = d.edges.map(NT => NT.component(a))
    }

  def diagMorph (f: C_TArr, d: Diagram[FC_TObj, FC_TArr]) : Set[D_TArr] =
    d.nodes.map(F => F.arrMap(f))

  def colimitAt (a: C_TObj, d: Diagram[FC_TObj, FC_TArr]) : Colimit[D_TObj, D_TArr] =
    D.colimit(diagAt(a, d))

  def colimit (d: Diagram[FC_TObj, FC_TArr]) = new Colimit[FC_TObj, FC_TArr] {
    val hostCat = FC
    val base = d
    // coapex: FC obj, so functor C --> D
    def coapex = new Functor[C_TObj, C_TArr, D_TObj, D_TArr] {
      val DomC = C
      val CodC = D
      def objMap = a => colimitAt(a, d).coapex
      def arrMap = f => {
        val a = C.dom(f)
        val b = C.cod(f)
        // need: D arrow, colimitAt(a, d).coapex --> colimitAt(b, d).coapex
        // create images of d
        val da = diagAt(a, d)
        val db = diagAt(b, d)
        // create colimits in D
        val cla = D.colimit(da)
        val clb = D.colimit(db)
        // rebase clb to da
        val ccbRebased = new Cocone[D_TObj, D_TArr] {
          val hostCat = D
          val base = da
          def coapex = clb.coapex
          // injMap : Map((n: D), n --> clb.coapex)
          val injMap: Map[D_TObj, D_TArr] =
            Map.from(diagMorph(f, d).map((darr: D_TArr) =>
              (D.dom(darr), D.comp(clb.inj(D.cod(darr)), darr))))
          def inj = injMap
        }
        // use univ
        cla.univ(ccbRebased)
      } // end arrMap
    }
    // inj: (n: diag.Nodes) => n --> coapex
    def inj = (n: FC_TObj) => new NatTrans[C_TObj, C_TArr, D_TObj, D_TArr] {
      val DomC = C
      val CodC = D
      val DomF = n
      val CodF = coapex
      def component = (a: C_TObj) => colimitAt(a, d).inj(n.objMap(a))
    }
    // univ: (cc: Cocone) => coapex --> cc.coapex (an NT)
    def univ = (cc: Cocone[FC_TObj, FC_TArr]) =>
      new NatTrans[C_TObj, C_TArr, D_TObj, D_TArr] {
        val DomC = C
        val CodC = D
        val DomF = coapex
        val CodF = cc.coapex
        def component = (a: C_TObj) => {
          // need D Arr: coapex(a) --> cc.coapex(a)
          val ccInD = new Cocone[D_TObj, D_TArr] {
            val hostCat = D
            val base = diagAt(a, d)
            val coapex = cc.coapex.objMap(a)
            val inj = Map.from(d.edges.map(e => (FC.dom(e).objMap(a), e.component(a))))
          }
          // cheat: recreate colimit in D (instead of mapping this one)
          val colimInD = colimitAt(a, d)
          // get univ: colimInD.coapex --> ccInD.coapex
          colimInD.univ(ccInD)
        }
      }
  }
}
