package category

/*
 * Basic Category definition
 */

trait Category {
  type TObj;
  type TArr;
  def dom: TArr => TObj;
  def cod: TArr => TObj;
  def id:  TObj => TArr;
  def comp: (TArr, TArr) => TArr;
}

/*
 * FiniteCat: A category that has finite sets for both objects and arrows
 *   Think of these sets as subtypes of TObj and TArr
 *   The fact that there are sets permits exhautive testing
 * An implementation should define `TObj` and `TArr` 
 *   and `objects: Set[TObj]` and `arrows: Set[TArr]`
 */
trait FiniteCat extends Category {
  val objects: Set[TObj];
  val arrows:  Set[TArr];
}
 
/*
 * StringFCat
 *   `objects` is a set of strings
 *   `arrows` is a set of (dom, name, cod) triples, including
 *     the identity arrows of the form (o, o, o) for each object o
 *     any arrows needed for closure
 */

trait StringFinCat extends FiniteCat {
  //type ObjBase = String;
  //type ArrBase = String;
 // val objects: Set[String];
  //val arrows: Set[(String, String, String)];
  def arrName(arr: TArr) = arr._2;
  def getArr(name: String) = arrows.find(arr => arr._2 == name);
  override type TObj = String;
  override type TArr = (String, String, String);
  override def dom = (arr: TArr) => arr._1;
  override def cod = (arr: TArr) => arr._3;
  override def id = (s: TObj) => (s, s, s);
  override def comp = (g: TArr, f: TArr) => {
    // composition of g after f
    //   if the dom of g == f (as strings), f must be an identity and similarly for g
    //   else if the arrows are compatible, compose their names using semicolon
    //   else the arrows are incompatible
    if (dom(g) == arrName(f)) g
    else if (cod(f) == arrName(g)) f
    else if (cod(f) == dom(g)) (dom(f), arrName(f) ++ ";" ++ arrName(g), cod(g))
    else ("", "*ERROR*", "")
  }
 }




// ********* Stuff not using any more *********

//  def ccomp(g: TArr, f: TArr): Option[TArr] =
//    if (cod(f) != dom(g)) None else Some(comp(g,f)) ;

trait FinSet1 [TObjBase] extends Category {
  type TObj <: TObjBase;
  type TArr <: TObjBase => TObjBase;
  def iterTObj: Iterator[TObj];
  def iterTArr: Iterator[TArr];
//  def comp: PartialFunction[(TArr, TArr), TArr];
} 


trait InitialObj {
  val C: Category
  val io: C.TObj;
  def outArr: this.C.TObj => this.C.TArr;
}

trait Junk extends Category {
  // checked versions of some operations
  def id_chkd(x: TObj): TArr = {
    val res = id(x);
    require (dom(res) == x, "id domain check failed");
    require (cod(res) == x, "id codomain check failed");
    return res
  }

  def comp_chkd(g: TArr, f: TArr): TArr = {
    require (cod(f) == dom(g));
    val res = comp(g, f)
    require (dom(res) == dom(f), "comp domain check failed");
    require (cod(res) == cod(g), "comp codomain check failed");
    return res;
  }
}
