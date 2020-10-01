package category

//  CATEGORIES CREATED FROM OTHER ONES

// ------ PRODUCT CATEGORIES ------

/*
 *  ProdCat: Product category of categories c1 and c2.
 */

class ProdCat (val c1: Category, val c2: Category) extends Category {
  type TObj = (c1.TObj, c2.TObj)
  type TArr = (c1.TArr, c2.TArr)
  def dom  = { case (a1, a2) => (c1.dom(a1), c2.dom(a2)) };
  def cod  = { case (a1, a2) => (c1.cod(a1), c2.cod(a2)) };
  def id   = { case (o1, o2) => (c1.id(o1), c2.id(o2)) };
  def comp = { case ((g1, g2), (f1, f2)) => (c1.comp(g1, f1), c2.comp(g2, f2)) };
}

/*
 *  ProdFinCat: Product of Finite categories.
 *   Defines the iterators and containment in terms of the component categories
 *   (See the notes in Scala-issues.md)
 */

class ProdFinCat (cp1: FiniteCat, cp2: FiniteCat) extends ProdCat(cp1, cp2) with FiniteCat {
  override val c1 = cp1;
  override val c2 = cp2;
  def objIter = for {o1 <- c1.objIter; o2 <- c2.objIter} yield (o1, o2);
  def arrIter = for {a1 <- c1.arrIter; a2 <- c2.arrIter} yield (a1, a2);
  def objsContain = {case (o1, o2) => c1.objsContain(o1) && c2.objsContain(o2)};
  def arrsContain = {case (a1, a2) => c1.arrsContain(a1) && c2.arrsContain(a2)};
}

/*
 *  ProdSCat: Product of SCategories
 *   Defines the object and arrow sets in term of the ones of the component cats
 *   (This implements FiniteCat with a different, but equivalent, implementation from the one above.)
 */

class ProdSCat (cp1: SCategory,  cp2: SCategory) extends ProdCat(cp1, cp2) with SCategory {
  override val c1 = cp1
  override val c2 = cp2
  val objects : Set[TObj] = for {o1 <- c1.objects; o2 <- c2.objects} yield (o1, o2)
  val arrows  : Set[TArr] = for {a1 <- c1.arrows; a2 <- c2.arrows} yield (a1, a2)
}


/*
 *  ProdSCat2: Product of SCategories not extending ProdCat (but copying in the code).
 *   (This was done in the course of defining the one above.)
 */

class ProdSCat2 (val c1: SCategory, val c2: SCategory) extends SCategory  {
  type TObj = (c1.TObj, c2.TObj)
  type TArr = (c1.TArr, c2.TArr)
  def dom  = { case (a1, a2) => (c1.dom(a1), c2.dom(a2)) };
  def cod  = { case (a1, a2) => (c1.cod(a1), c2.cod(a2)) };
  def id   = { case (o1, o2) => (c1.id(o1), c2.id(o2)) };
  def comp = { case ((g1, g2), (f1, f2)) => (c1.comp(g1, f1), c2.comp(g2, f2)) };
  val objects : Set[TObj] = for {o1 <- c1.objects; o2 <- c2.objects} yield (o1, o2)
  val arrows  : Set[TArr] = for {a1 <- c1.arrows; a2 <- c2.arrows} yield (a1, a2)
}



// ------ DUAL CATEGORIES (and dual functors) ------

/*
 *  Dual category (see CCT section 3.6)
 *    aka Op (Opposite)
 */

class Dual (val c: Category) extends Category {
  type TObj = c.TObj;
  type TArr = c.TArr;
  def dom = c.cod;
  def cod = c.dom;
  def id  = c.id;
  def comp = { case (g, f) => c.comp(f, g) };
}

abstract class Dual2 extends Category {
  val SrcCat : Category;
  type TObj = SrcCat.TObj;
  type TArr = SrcCat.TArr;
  def dom = SrcCat.cod;
  def cod = SrcCat.dom;
  def id  = SrcCat.id;
  def comp = { case (g, f) => SrcCat.comp(f, g) };
}

object DualFns {

  def dualCat (c: Category): Category {
    type TObj = c.TObj;
    type TArr = c.TArr;
  } = new Category {
    type TObj = c.TObj;
    type TArr = c.TArr;
    def dom = c.cod;
    def cod = c.dom;
    def id  = c.id;
    def comp = { case (g, f) => c.comp(f, g) };
  }

  def dualFnctr (F: Functor): Functor = new Functor {
    val DomC = dualCat(F.DomC);
    val CodC = dualCat(F.CodC);
    def objMap = F.objMap;
    def arrMap = F.arrMap;
  }
}

/*
class Dual3 (val c: Category) extends Category {
  type TObj = c.TObj;
  type TArr = c.TArr;
  def dom = c.cod;
  def cod = c.dom;
  def id  = c.id;
  def comp = { case (g, f) => c.comp(f, g) };
}
 */

/*
 *  Diagonal categories
 */

// ISSUE: Objects should only be (a, a) pairs but how do we express this?
class DiagCat (val c: Category) extends ProdCat(c, c) {}

// ERROR with DiagFCat: I can't get the overrides to type
class DiagSCat (val d: SCategory) extends ProdSCat(d, d) {
  override val c1 = d
  override val c2 = d
  //override val objects : Set[(d.TObj, d.TObj)] = for {o <- d.objects} yield (o, o)
  //override val objects : Set[TObj] = for {o <- d.objects} yield ((o : c1.TObj), o)
 // override val arrows  : Set[TArr] = for {a <- d.arrows}  yield (a, a)
}


/*
 * Diagonal category for finite categories
 */

class DiagFCat2 (val c: SCategory) extends SCategory {
  type TObj = (c.TObj, c.TObj)
  type TArr = (c.TArr, c.TArr)
  def dom  = { case (a1, a2) => (c.dom(a1), c.dom(a2)) };
  def cod  = { case (a1, a2) => (c.cod(a1), c.cod(a2)) };
  def id   = { case (o1, o2) => (c.id(o1), c.id(o2)) };
  def comp = { case ((g, _), (f, _)) => { val res = c.comp(g, f) ; (res, res) }};
  val objects = c.objects.map(o => (o, o));
  val arrows  = c.arrows.map(f => (f, f));
}
