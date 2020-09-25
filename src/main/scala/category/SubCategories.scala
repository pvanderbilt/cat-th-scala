package category

/*
 * CatWithInitial: trait for a category with an initial object:
 *   initialObj: the initial object
 *   outArr: the universal property:
 *     there is a unique arrow from the initial object to every other object
 */
trait CatWithInitial extends Category {
  val initialObj: this.TObj;
  def io_outArr: this.TObj => this.TArr;

  def outArr_chkd (x: this.TObj): this.TArr = {
    val res = io_outArr(x);
    require (dom(res) == initialObj, "InitObj outArr domain check failed");
    require (cod(res) == x,  "InitObj outArr codomain check failed");
    return res
  }
}

/* The following definition isn't very useful */
trait InitialObj {
  val C: Category
  val io: C.TObj;
  def outArr: this.C.TObj => this.C.TArr;
}

