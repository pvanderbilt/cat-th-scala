package category.examples
import category._

object emptyCat extends FiniteCat {
  type TObj = Nothing;
  type TArr = Nothing;
  def dom = (_) => throw new NotImplementedError; // x match {};
  def cod = (_) => throw new NotImplementedError;
  def id  = (_) => throw new NotImplementedError;
  def comp = (_, _) => throw new NotImplementedError;
  def objIter = Iterator();
  def arrIter = Iterator();
}

object unitCat extends FiniteCat {
  type TObj = Unit;
  type TArr = Unit;
  def dom = (_) => ();
  def cod = (_) => ();
  def id = (_) => ();
  def comp = (_, _) => ();
  def objIter = Iterator(());
  def arrIter = Iterator(());
}

class MonoidCat [A] (e: A, op: (A, A) => A) extends Category {
  type TObj = Unit;
  type TArr = A;
  def dom = (_) => ();
  def cod = (_) => ();
  def id = (_) => e;
  def comp = op;
}

object boolCat extends FiniteCat with CatWithInitial {
  type TObj = Boolean;
  type TArr = (Boolean, Boolean);
  val arrows : Set[TArr] = Set((false, false), (false, true), (true, true));
  def dom = { case (d, _) => d };
  def cod = { case (_, c) => c }
  def id = (b) => (b, b);
  def comp = { case ((gd, gc), (fd, fc)) => (fd, gc) };
  def objIter = Iterator(false, true);
  def arrIter = arrows.iterator;
  val initialObj = false;
  def io_univ    = (b: TObj) => (false, b);
}

object categories {

  // Incomplete and not currently used
  def boolCat2 (f: Boolean => Boolean) : Category = new Category {
    type TObj = Boolean;
    type TArr = (Boolean, Boolean);
    def dom = { case (d, _) => d };
    def cod = { case (_, c) => c }
    def id = (b) => (b, b);
    def comp = { case ((gd, gc), (fd, fc)) => (fd, gc) };
  }

  val boolEqMonCat = new MonoidCat(true, (x: Boolean, y: Boolean) => x&&y);
  val u: boolEqMonCat.TObj = ();
  val b: boolEqMonCat.TArr = false;

}
