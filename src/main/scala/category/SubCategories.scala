package category

/*
 * CatWithInitial: trait for a category with an initial object:
 *   initialObj: the initial object
 *   io_univ: the universal property:
 *     there is a unique arrow from the initial object to every other object
 */

trait CatWithInitial extends Category {
  val initialObj: TObj;
  def io_univ: TObj => TArr;
}


/* 
 * Initial Object
 */

trait InitialObj {
  val C: Category
  val initialObj: C.TObj;
  def io_univ: C.TObj => C.TArr;
}


/*
 *  Experimental alternative
 */

trait CatWithInitial2 extends Category with InitialObj {
  override val C = this
}

