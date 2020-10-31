package pcategory.cctth4

import pcategory.core._
import pcategory.functor._
import pcategory.limits._

/*
 * CCT Theorem 4, section 5.5, p 129: 
 *   For C and cocomplete D, their functor cat is cocomplete
 *     So, forall cats C1 & D1, where D1 is cocomplete,
 *     FC_Cocomplete(C1, D1) is a subclass of FunctorCat(C1, D1) 
 *     that implements Cocomplete
 */

class FC_Cocomplete [C_TObj, C_TArr, D_TObj, D_TArr] (
  C1: Category[C_TObj, C_TArr],
  D1: Category[D_TObj, D_TArr] with Cocomplete[D_TObj, D_TArr]
)
    extends FunctorCat(C1, D1) with Cocomplete[Functor[C_TObj, C_TArr, D_TObj, D_TArr],
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
    D1.colimit(diagAt(a, d))

  def colimit (d: Diagram[FC_TObj, FC_TArr]) = new Colimit[FC_TObj, FC_TArr] {
    val C = FC
    val base = d
    // coapex: FC obj, so functor C --> D
    def coapex = new Functor[C_TObj, C_TArr, D_TObj, D_TArr] {
      val DomC = C1
      val CodC = D1
      def objMap = a => colimitAt(a, d).coapex
      def arrMap = f => {
        val a = C1.dom(f)
        val b = C1.cod(f)
        // need: D arrow, colimitAt(a, d).coapex --> colimitAt(b, d).coapex
        // create images of d
        val da = diagAt(a, d)
        val db = diagAt(b, d)
        // create colimits in D
        val cla = D1.colimit(da)
        val clb = D1.colimit(db)
        // rebase clb to da
        val ccbRebased = new Cocone[D_TObj, D_TArr] {
          val C = D1
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
      val DomC = C1
      val CodC = D1
      val DomF = n
      val CodF = coapex
      def component = (a: C_TObj) => colimitAt(a, d).inj(n.objMap(a))
    }
    // univ: (cc: Cocone) => coapex --> cc.coapex (an NT)
    def univ = (cc: Cocone[FC_TObj, FC_TArr]) =>
      new NatTrans[C_TObj, C_TArr, D_TObj, D_TArr] {
        val DomC = C1
        val CodC = D1
        val DomF = coapex
        val CodF = cc.coapex
        def component = (a: C_TObj) => {
          // need D Arr: coapex(a) --> cc.coapex(a)
          val ccInD = new Cocone[D_TObj, D_TArr] {
            val C = D1
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
