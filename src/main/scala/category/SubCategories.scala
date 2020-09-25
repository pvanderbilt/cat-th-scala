package category

/*
 * CatWithInitial: trait for a category with an initial object:
 *   initialObj: the initial object
 *   io_univ: the universal property:
 *     there is a unique arrow from the initial object to every other object
 */

trait CatWithInitial extends Category {
  val initialObj: this.TObj;
  def io_univ: this.TObj => this.TArr;
}


/* 
 * Initial Object
 */

trait InitialObj {
  val C: Category
  val io: C.TObj;
  def univ: this.C.TObj => this.C.TArr;
}


/*
 *  Experimental alternative
 */

trait CatWithInitial2 extends Category with InitialObj {
  override val C = this
}

