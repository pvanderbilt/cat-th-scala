package pcategory.adjunct

import pcategory.core._
import pcategory.functor._
import pcategory.functor.functorOps._

/*
 * UniversalArrow: See CCTh def 21 (p141)
 *   For functor R: C->D, a universal arrow from D obj b to G
 *    is a pair (c: C, u: b->R(c))) such that 
 *    for any other pair (c': C, f: b->R(c')))
 *      there exists unique C arrow f#: c -> c' such that R(f#) ○ u = f
 */

trait UnivArrow [C_TObj, C_TArr, D_TObj, D_TArr] {
  val C: Category[C_TObj, C_TArr]
  val D: Category[D_TObj, D_TArr]
  val R: Functor[C_TObj, C_TArr, D_TObj, D_TArr]
  val d: D_TObj
  val c: C_TObj
  val u: D_TArr
  def hashf (c2: C_TObj, f: D_TArr): C_TArr

  def precondition: Boolean = D.dom(u) == c && D.cod(u) == R.objMap(c)
  def hashf_prec (c2: C_TObj, f: D_TArr): Boolean =
    D.dom(f) == c2 && D.cod(f) == R.objMap(c2)
  def hashf_postc (c2: C_TObj, f: D_TArr): Boolean = {
    val fhash: C_TArr = hashf(c2, f)
    C.dom(fhash) == c && C.cod(fhash) == c2 && f == D.comp(R.arrMap(fhash), u)
  }
}

/*
 * AdjunctionUniv: Adjunction defined in terms of universal arrows
 */

trait AdjunctionUniv [C_TObj, C_TArr, D_TObj, D_TArr] {
  val C: Category[C_TObj, C_TArr]
  val D: Category[D_TObj, D_TArr]
  val R: Functor[C_TObj, C_TArr, D_TObj, D_TArr]
  def uArr (d: D_TObj): UnivArrow [C_TObj, C_TArr, D_TObj, D_TArr]

  def uArr_postc (d: D_TObj): Boolean = {
    val ua = uArr(d)
    ua.C == C && ua.D == D && ua.R == R && ua.d == d
  }
}

/*
 * AdjunctionNTs: Adjunction defined in terms of R & L and NTs eta and epsilon
 */

trait AdjunctionNTs [C_TObj, C_TArr, D_TObj, D_TArr] {
  val C: Category[C_TObj, C_TArr]
  val D: Category[D_TObj, D_TArr]
  val R: Functor[C_TObj, C_TArr, D_TObj, D_TArr]
  val L: Functor[D_TObj, D_TArr, C_TObj, C_TArr]
  val eta: NatTrans[D_TObj, D_TArr, D_TObj, D_TArr]
  val eps: NatTrans[C_TObj, C_TArr, C_TObj, C_TArr]

  def precondition: Boolean =
    R.DomC == C && R.CodC == D && L.DomC == D && L.CodC == C && 
      eta.DomC == C && eta.CodC == D && eps.DomC == D && eps.CodC == C &&
      eta.DomF == functorComp(R,L) && eta.CodF == functorId(C) &&
      eps.DomF == functorId(D) && eps.CodF == functorComp(L,R)
}

/*
 * AdjunctionIsos: Adjunction defined in terms of isomorphisms between Hom sets
 */

trait AdjunctionIsos [C_TObj, C_TArr, D_TObj, D_TArr] {
  val C: Category[C_TObj, C_TArr]
  val D: Category[D_TObj, D_TArr]
  val R: Functor[C_TObj, C_TArr, D_TObj, D_TArr]
  val L: Functor[D_TObj, D_TArr, C_TObj, C_TArr]
  def IsoR (c: C_TObj, d: D_TObj, f: C_TArr): D_TArr // f: L(d)->c => d->R(c)
  def IsoL (c: C_TObj, d: D_TObj, f: D_TArr): C_TArr // f: d->R(c) => L(d)->c
}

object adjOps {

  def adjUnivToNTs [C_TObj, C_TArr, D_TObj, D_TArr] (
    adju: AdjunctionUniv[C_TObj, C_TArr, D_TObj, D_TArr]
  ) = new AdjunctionNTs[C_TObj, C_TArr, D_TObj, D_TArr] { adj =>
    val C = adju.C
    val D = adju.D
    val R = adju.R
    val L = new Functor[D_TObj, D_TArr, C_TObj, C_TArr] {
      val DomC = D
      val CodC = C
      // for object d, let (c,u) be the corresponding UA; L(d) := c
      def objMap = (d: D_TObj) => adju.uArr(d).c
      // for f: d -> d2 with uar2 = (c2, u2) the UA for d2, L(f) := (u2.f)#
      def arrMap = (f: D_TArr) => {
        val uar  = adju.uArr(D.dom(f))
        val uar2 = adju.uArr(D.cod(f))
        uar.hashf(objMap(D.cod(f)), D.comp(uar2.u, f))
      }
    }
    val eta = new NatTrans [D_TObj, D_TArr, D_TObj, D_TArr] {
      val DomC = D
      val CodC = D
      val DomF = functorId (D)
      val CodF = functorComp(adj.R, adj.L)
      def component = (d: D_TObj) => adju.uArr(d).u
    }
    val eps = new NatTrans[C_TObj, C_TArr, C_TObj, C_TArr] {
      val DomC = C
      val CodC = C
      val DomF = functorComp(adj.L, adj.R)
      val CodF = functorId(C)
      def component = (c: C_TObj) => {
        val cR = adj.R.objMap(c)
        adju.uArr(cR).hashf(c, D.id(cR))
      }
    }
  }
}
