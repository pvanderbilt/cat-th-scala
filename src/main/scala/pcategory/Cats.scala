package pcategory.cats

import pcategory.core._
import pcategory.functor._
import pcategory.functor.functorOps._

object unitCat extends Category[Unit, Unit] {
  def dom  = (_) => ()
  def cod  = (_) => ()
  def id   = (_) => ()
  def comp = (_, _) => ()
}

class MonoidCat [A] (e: A, op: (A, A) => A) extends Category[Unit, A] {
  def dom = (_) => ();
  def cod = (_) => ();
  def id = (_) => e;
  def comp = op;
}

object catOps {

  /*
   * oneFunctor(C): Functor from C to unitCat
   */
  def oneFunctor[C_TObj, C_TArr] (C: Category[C_TObj, C_TArr]): Functor[C_TObj, C_TArr, Unit, Unit] =
    new Functor[C_TObj, C_TArr, Unit, Unit] {
      val DomC = C;
      val CodC = unitCat;
      def objMap = (_) => ()
      def arrMap = (_) => ()
    }

}
