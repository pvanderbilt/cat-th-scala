package category

/*
 * Functor: A mapping between categories
 *   DomC and CodC are the domain and codomain categories
 *   `objMap` maps DomC objects to CodC objects
 *   `arrMap` maps DomC arrows  to CodC arrows
 */

trait Functor {
  val DomC: Category;
  val CodC: Category;
  def objMap: DomC.TObj => CodC.TObj;
  def arrMap: DomC.TArr => CodC.TArr;
}

object functorOps {

  /*
   * Identity Functor 
   */

  def functorId (C: Category): Functor = new Functor {
    val DomC: Category { type TObj = C.TObj; type TArr = C.TArr } = C;
    val CodC: Category { type TObj = C.TObj; type TArr = C.TArr } = C ;
    val objMap = (obj: C.TObj) => obj;
    val arrMap = (arr: C.TArr) => arr;
  }

  /*
   * Functor composition
   *   F: A --> B, G: B --> C
   */

  def functorComp [B_TObj, B_TArr] (
    G: Functor { val DomC : Category { type TObj = B_TObj; type TArr = B_TArr }},
    F: Functor { val CodC : Category { type TObj = B_TObj; type TArr = B_TArr }}
  ) : Functor {
    val DomC: Category { type TObj = F.DomC.TObj; type TArr = F.DomC.TArr };
    val CodC: Category { type TObj = G.CodC.TObj; type TArr = G.CodC.TArr };
  } = new Functor {
    val DomC: Category { type TObj = F.DomC.TObj; type TArr = F.DomC.TArr } = F.DomC;
    val CodC: Category { type TObj = G.CodC.TObj; type TArr = G.CodC.TArr } = G.CodC;
    def objMap = (obj: F.DomC.TObj) => G.objMap(F.objMap(obj));
    def arrMap = (arr: F.DomC.TArr) => G.arrMap(F.arrMap(arr));
  }

  /*
   * Functor composition, experimental alternative
   *   F: A --> B, G: B --> C
   */

  def functorComp2 (
    A: Category,
    B: Category,
    C: Category
  ) (
    G: Functor {
      val DomC : Category { type TObj = B.TObj; type TArr = B.TArr }
      val CodC : Category { type TObj = C.TObj; type TArr = C.TArr }
    },
    F: Functor {
      val DomC : Category { type TObj = A.TObj; type TArr = A.TArr }
      val CodC : Category { type TObj = B.TObj; type TArr = B.TArr }
    }
  ) : Functor {
    val DomC: Category { type TObj = A.TObj; type TArr = A.TArr };
    val CodC: Category { type TObj = C.TObj; type TArr = C.TArr };
  } = new Functor {
    val DomC: Category { type TObj = A.TObj; type TArr = A.TArr } = A;
    val CodC: Category { type TObj = C.TObj; type TArr = C.TArr } = C;
    def objMap = (obj: A.TObj) => G.objMap(F.objMap(obj));
    def arrMap = (arr: A.TArr) => G.arrMap(F.arrMap(arr));
  }
}

object exampleFunctors {
}

/*
 * Other attempts at making functors and their duals work.  Not used.
 */

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
