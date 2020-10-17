import category._

object Hello extends App {
  println("** Scala category tests **")

  def showIter [A] (xs: Iterator[A]): String = xs.mkString("{", ", ", "}");

  def showCat(C: FiniteCat, name: String): Unit = {
    println(s"Category: $name");
    println(s"  objects: ${showIter(C.objIter)}");
    println(s"  arrows:  ${showIter(C.arrIter)}");
    println(s"  identities:");
    for (obj <- C.objIter) {
      println(s"    id($obj)  = ${C.id(obj)}");
    }
    println();
  }

  import examples._
  showCat(emptyCat,  "emptyCat");
  showCat(unitCat,   "unitCat");
  showCat(boolCat,   "boolCat");
  showCat(cat323,    "cat323");
  showCat(cat323c,   "cat323c");
  

  println();
}
