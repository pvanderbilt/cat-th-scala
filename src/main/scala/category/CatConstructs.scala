package category

//  CATEGORY CREATED FROM OTHER ONES

/*
 *  Product category
 */

class ProdCat (val c1: Category, val c2: Category) extends Category {
  type TObj = (c1.TObj, c2.TObj)
  type TArr = (c1.TArr, c2.TArr)
  def dom  = { case (a1, a2) => (c1.dom(a1), c2.dom(a2)) };
  def cod  = { case (a1, a2) => (c1.cod(a1), c2.cod(a2)) };
  def id   = { case (x1, x2) => (c1.id(x1), c2.id(x2)) };
  def comp = { case ((g1, g2), (f1, f2)) => (c1.comp(g1, f1), c2.comp(g2, f2)) };
}

/*
 *  ISSUES with ProdFinCat
 *     If parameters to ProdCat aren't val's, there's an error about private values escaping
 *     If ProdFinCat doesn't override c1 and c2, the typer doesn't see the formal parameter c1 
 *       as the same as the actual parameter, cp1.  Error:
 *          found   : o1.type (with underlying type ProdFinCat.this.cp1.TObj)
 *          required: ProdFinCat.this.c1.TObj
 *            val objects : Set[this.TObj] = cp1.objects.flatMap(o1 => cp2.objects.map(o2 => (o1, o2)))
 *       (Renaming cp1 parameters to c1 just makes the error message more mysterious).
 */

class ProdFinCat ( cp1: SCategory,  cp2: SCategory) extends ProdCat(cp1, cp2) with SCategory {
  override val c1 = cp1
  override val c2 = cp2
  val objects : Set[TObj] = for {o1 <- c1.objects; o2 <- c2.objects} yield (o1, o2)
  val arrows  : Set[TArr] = for {a1 <- c1.arrows; a2 <- c2.arrows} yield (a1, a2)

}

class ProdFinCat2 (val c1: SCategory, val c2: SCategory) extends SCategory  {
  type TObj = (c1.TObj, c2.TObj)
  type TArr = (c1.TArr, c2.TArr)
  def dom  = { case (a1, a2) => (c1.dom(a1), c2.dom(a2)) };
  def cod  = { case (a1, a2) => (c1.cod(a1), c2.cod(a2)) };
  def id   = { case (x1, x2) => (c1.id(x1), c2.id(x2)) };
  def comp = { case ((g1, g2), (f1, f2)) => (c1.comp(g1, f1), c2.comp(g2, f2)) };
  val objects : Set[TObj] = for {o1 <- c1.objects; o2 <- c2.objects} yield (o1, o2)
  val arrows  : Set[TArr] = for {a1 <- c1.arrows; a2 <- c2.arrows} yield (a1, a2)
}

 



/*
 *  Diagonal categories
 */

// ERROR:
//  Objects should only be (a, a) pairs but how do we express this?
class DiagCat (val c: Category) extends ProdCat(c, c) {}

// ERROR with next: I can't get the overrides to type

class DiagFCat (val d: SCategory) extends ProdFinCat(d, d) {
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
  def id   = { case (x1, x2) => (c.id(x1), c.id(x2)) };
  def comp = { case ((g, _), (f, _)) => { val res = c.comp(g, f) ; (res, res) }};
  val objects = c.objects.map(x => (x, x));
  val arrows  = c.arrows.map(f => (f, f));
}

