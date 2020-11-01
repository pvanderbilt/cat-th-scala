package pcategory.functor

import pcategory.core._

/*
 * This file has: functors, natural transformations and the category of functors
 */

// -------------------------------- FUNCTORS --------------------------------

/*
 * PFunctor: A functor has two mappings, one for objects and one for arrows
 */

trait Functor [C_TObj, C_TArr, D_TObj, D_TArr]{
  val DomC: Category[C_TObj, C_TArr];
  val CodC: Category[D_TObj, D_TArr];
  def objMap: C_TObj => D_TObj;
  def arrMap: C_TArr => D_TArr;

  def checkId (obj: C_TObj): Boolean = arrMap(DomC.id(obj)) == CodC.id(objMap(obj))
  def checkComp (g: C_TArr, f: C_TArr): Boolean =
    arrMap(DomC.comp(g, f)) == CodC.comp(arrMap(g), arrMap(f))
}

object functorOps {
  import pcategory.core.catOps._

  /*
   * Identity Functor 
   */

  def functorId [C_TObj, C_TArr] (C: Category[C_TObj, C_TArr]):
      Functor[C_TObj, C_TArr, C_TObj, C_TArr] =
    new Functor[C_TObj, C_TArr, C_TObj, C_TArr] {
      val DomC = C;
      val CodC = C;
      val objMap = (obj: C_TObj) => obj;
      val arrMap = (arr: C_TArr) => arr;
    }

  /*
   * Functor composition
   *   F: A --> B, G: B --> C
   */

  def functorComp [A_TObj, A_TArr, B_TObj, B_TArr, C_TObj, C_TArr] (
    G: Functor[B_TObj, B_TArr, C_TObj, C_TArr],
    F: Functor[A_TObj, A_TArr, B_TObj, B_TArr]
  ) : Functor[A_TObj, A_TArr, C_TObj, C_TArr] =
    new Functor[A_TObj, A_TArr, C_TObj, C_TArr] {
      val DomC: Category[A_TObj, A_TArr] = F.DomC
      val CodC: Category[C_TObj, C_TArr] = G.CodC;
      def objMap = (obj: A_TObj) => G.objMap(F.objMap(obj));
      def arrMap = (arr: A_TArr) => G.arrMap(F.arrMap(arr));
    }

  /*
   * Dual of a functor: Dual(F: A --> B): dual(A) --> dual(B)
   */

  def dualFun [A_TObj, A_TArr, B_TObj, B_TArr] (
    F: Functor[A_TObj, A_TArr, B_TObj, B_TArr]
  ): Functor[A_TObj, A_TArr, B_TObj, B_TArr] =
    new Functor[A_TObj, A_TArr, B_TObj, B_TArr] {
      val DomC = dualCat(F.DomC)
      val CodC = dualCat(F.CodC)
      def objMap = F.objMap
      def arrMap = F.arrMap
    }
}

// -------------------------------- Natural Transformations --------------------------------

/*
 * PNatTrans: A natural transformation is a set of arrows between functor endpoints
 *   NT : F --> G = (x: C) => F(x) --> G(x) 
 */

trait NatTrans [C_TObj, C_TArr, D_TObj, D_TArr] { nt =>
  val DomC: Category[C_TObj, C_TArr];
  val CodC: Category[D_TObj, D_TArr];
  val DomF: Functor[C_TObj, C_TArr, D_TObj, D_TArr];
  val CodF: Functor[C_TObj, C_TArr, D_TObj, D_TArr];
  def component: C_TObj => D_TArr;

  // naturality condition
  def checkCommutes (arr: C_TArr): Boolean =
    ( CodC.comp( CodF.arrMap(arr), component(DomC.dom(arr)) )
      == CodC.comp( component(DomC.cod(arr)), DomF.arrMap(arr) ) )
}

object natTransOps {

  /*
   *  Identity NT (for a given functor, F: C --> D)
   */

  def natTransId[C_TObj, C_TArr, D_TObj, D_TArr] (
    F: Functor[C_TObj, C_TArr, D_TObj, D_TArr]
  ): NatTrans[C_TObj, C_TArr, D_TObj, D_TArr] =
    new NatTrans[C_TObj, C_TArr, D_TObj, D_TArr] {
      val DomC = F.DomC; // C
      val CodC = F.CodC; // D
      val DomF = F;
      val CodF = F;
      def component = (x: C_TObj) => CodC.id(F.objMap(x));
    }

  /*
   * NT (Vertical) Composition
   *   alpha: F --> G, beta: G --> H; F,G,H: C --> D
   */

  def natTransVComp[C_TObj, C_TArr, D_TObj, D_TArr] (
    beta: NatTrans[C_TObj, C_TArr, D_TObj, D_TArr],
    alpha: NatTrans[C_TObj, C_TArr, D_TObj, D_TArr]
  ): NatTrans[C_TObj, C_TArr, D_TObj, D_TArr] =
    new NatTrans[C_TObj, C_TArr, D_TObj, D_TArr] {
      //  alpha and beta should have the same domain and codomain cats
      val DomC = alpha.DomC; // C
      val CodC = alpha.CodC; // D
      val DomF = alpha.DomF; // F
      val CodF = beta.CodF;  // H
      def component =
        (x: C_TObj) => CodC.comp(beta.component(x), alpha.component(x));
    }

  /*
   * NT Horizontal Composition
   *   F1, F2: C --> D, G1, G2: D --> E, alpha: F1 --> F2, beta: G1 --> G2
   *   (beta . alpha)(x: C) = beta(F2(x)) o G1(alpha(x))
   */

  def natTransHComp[C_TObj, C_TArr, D_TObj, D_TArr, E_TObj, E_TArr] (
    beta: NatTrans[D_TObj, D_TArr, E_TObj, E_TArr],
    alpha: NatTrans[C_TObj, C_TArr, D_TObj, D_TArr]
  ): NatTrans[C_TObj, C_TArr, E_TObj, E_TArr] =
    new NatTrans[C_TObj, C_TArr, E_TObj, E_TArr] {
      val DomC = alpha.DomC;
      val CodC = beta.CodC;
      val DomF = functorOps.functorComp[C_TObj, C_TArr, D_TObj, D_TArr, E_TObj, E_TArr] (
        beta.DomF, alpha.DomF)
      val CodF = functorOps.functorComp[C_TObj, C_TArr, D_TObj, D_TArr, E_TObj, E_TArr] (
        beta.CodF, alpha.CodF)
      def component = (x: C_TObj) => CodC.comp(
        beta.component(alpha.CodF.objMap(x)),
        beta.DomF.arrMap(alpha.component(x))
      )
    }

}

// -------------------------------- Category of Functors --------------------------------

  /*
   * The functor category (between two categories C and D) has 
   *   functors (C-->D) as objects and NTs as arrows
   */

class FunctorCat [C_TObj, C_TArr, D_TObj, D_TArr] (
  val C: Category[C_TObj, C_TArr],
  val D: Category[D_TObj, D_TArr]
)
    extends Category[Functor[C_TObj, C_TArr, D_TObj, D_TArr],
      NatTrans[C_TObj, C_TArr, D_TObj, D_TArr]]
{
  import natTransOps._
  def dom  = _.DomF
  def cod  = _.CodF
  def id   = natTransId[C_TObj, C_TArr, D_TObj, D_TArr]
  def comp = natTransVComp[C_TObj, C_TArr, D_TObj, D_TArr]
}

