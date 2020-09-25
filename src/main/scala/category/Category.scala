package category

/*
 * Basic Category definition
 *    `TObj` and `TArr` are the Scala base types for object and arrows.
 *    A given category's objects may be all of TObj or some subset drawn from TObj;
 *    similarly for arrows.
 *    The remaining operations are those for the category.
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
 * FiniteCat: A category with finite sets of objects and arrows
 *     In particular, these categories have terminating iterators for both objects and arrows.
 *     They also have methods for checking containment. (The object one isn't used currently)   
 *   The fact that the objects and arrows are finite permits exhautive testing.
 */

trait FiniteCat extends Category {
  def objIter: Iterator[TObj];
  def arrIter: Iterator[TArr];
  def objContains: TObj => Boolean;
  def arrContains: TArr => Boolean;
} 


/*
 * SCategory: A finite category that has Scala sets for both objects and arrows
 *   Think of these sets as subtypes of TObj and TArr
 * An implementation should define `TObj` and `TArr` 
 *   and `objects: Set[TObj]` and `arrows: Set[TArr]`
 */

trait SCategory extends FiniteCat {
  val objects: Set[TObj];
  val arrows:  Set[TArr];
  // Implement FiniteCat methods
  def objIter = objects.iterator;
  def arrIter = arrows.iterator;
  def objContains = objects.contains;
  def arrContains = arrows.contains;
}


/*
 * SimpleFinCat
 *   `objects` is a set of identifiers (of abstract type ObjId)
 *   `arrows` is a set of (dom, List[ArrId], cod) triples (for abstract type ArrId)
 *      The `arrows` set should include the identity arrows of the form (o, o, o) for each object o
 *      any arrows needed for closure.
 */
 
trait SimpleFinCat extends SCategory {
  type ObjId;
  type ArrId;
  // Define Category's TObj and TArr as discussed
  override type TObj = ObjId;
  override type TArr = (ObjId, List[ArrId], ObjId);
  // Inherit abstract `objects: Set[ObjId]` and `arrows: Set[(ObjId, Seq[ArrId], ObjId)]`
  // Implement Category's abstract methods
  override def dom = (arr: TArr) => arr._1;
  override def cod = (arr: TArr) => arr._3;
  override def id = (s: TObj) => (s, List(), s);
  override def comp = (g: TArr, f: TArr) => (dom(f), f._2 ++ g._2, cod(g))
 }


/*
 * StringFinCat
 *   `objects` is a set of strings
 *   `arrows` is a set of (dom, name, cod) triples, including
 *     the identity arrows of the form (o, o, o) for each object o
 *     any arrows needed for closure.
 *   (This is an older trait superceeded by the above)
 */

trait StringFinCat extends SCategory {
  // Define Category's TObj and TArr as discussed
  override type TObj = String;
  override type TArr = (String, String, String);
  // Inherits `objects: Set[T]` and `arrows: Set[(String, String, String)]`
  // Define a couple of helper functions
  def arrName(arr: TArr) = arr._2;
  def getArr(name: String) = arrows.find(arr => arr._2 == name);
  // Implement Category's abstract methods
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
