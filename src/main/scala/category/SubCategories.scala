package category

/*
 * CatWithInitial: trait for a category with an initial object:
 *   initialObj: the initial object
 *   io_univ: (c) => io -> c: the universal property:
 *     there is a unique arrow from the initial object to every other object
 */

trait CatWithInitial extends Category {
  val initialObj: TObj;
  def io_univ: TObj => TArr;
}


/* 
 * Initial Object: Alternate definition of an initial object mixin trait.
 *   This has the category as a field, `io_cat`, which needs to be overridden
 *   `io_cat = this`. 
 */

trait InitialObj {
  val io_cat: Category
  val initialObj: io_cat.TObj;
  def io_univ: io_cat.TObj => io_cat.TArr;
}


/*
 *  Experimental alternative: This doesn't work because
 *   the connection is lost.
 */

trait CatWithInitial2 extends Category with InitialObj {
  override val io_cat = this
}


/*
 * CatWithFinalObj: trait for a category with a final object:
 *   finalObj: the final object
 *   fo_univ: the universal property:
 *     there is a unique arrow from every other object to the final object
 */

trait CatWithFinal extends Category {
  val finalObj: TObj;
  def fo_univ: TObj => TArr;
}


/*
 * CatWithProds: trait for a category with products:
 *   prod(a, b): a function returning a*b
 *   projL: {a, b} a*b -> a
 *   projR: {a, b} a*b -> b
 *   prod_univ: (a*b, c, f, g) => c -> a*b
 *     the universal property: for any obj c with f: c->a and g: c->b
 *     there is a unique arrow from c to a*b
 */

trait CatWithProds extends Category {
  def prod : (TObj, TObj) => TObj;
  def projL: (TObj, TObj, TObj) => TArr
  def projR: (TObj, TObj, TObj) => TArr
  def prod_univ: (TObj, TObj, TArr, TArr) => TArr;
}

/*
 * CatWithProds: trait for a category with products:
 *   coprod(a, b): a function returning a+b
 *   injL: {a, b} a -> a+b
 *   injR: {a, b} b -> a+b
 *   coprod_univ: (a+b, c, f, g) => a+b -> c
 *     the universal property: for any obj c with f: a->c and g: b->c
 *     there is a unique arrow from a+b to c
 */

trait CatWithCoprods extends Category {
  def coprod: (TObj, TObj) => TObj;
  def injL  : (TObj, TObj, TObj) => TArr
  def injR  : (TObj, TObj, TObj) => TArr
  def coprod_univ: (TObj, TObj, TArr, TArr) => TArr;
}
