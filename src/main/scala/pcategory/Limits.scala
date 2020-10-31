package pcategory.limits

import pcategory.core._

/*
 * This file has: limits and colimits (actually just the latter)
 *  Also cocomplete
 */


// -------------------------------- DIAGRAMS --------------------------------

/*
 * DIAGRAMS
 */

trait Diagram [TObj, TArr] {
  def nodes: Set[TObj];
  def edges: Set[TArr];
}

// -------------------------------- COLIMITS --------------------------------

/*
 * CoCone: for a given base (a diagram), a cocone has a co-apex and 
 *   injectors from the base to the co-apex
 */

trait Cocone [TObj, TArr] {
  val C: Category[TObj, TArr];
  val base: Diagram[TObj, TArr];
  def coapex: TObj;
  def inj : TObj => TArr; // n: nodes => n -> coapex

  def checkCoconeFacesComm (edge: TArr): Boolean =
    (inj(C.dom(edge)) == C.comp(inj(C.cod(edge)), edge))
}


/*
 * Colimit: a cocone is a colimit if it has the appropriate universal property
 */

trait Colimit [TObj, TArr] extends Cocone[TObj, TArr] {
  def univ : Cocone[TObj, TArr] => TArr; // cc: Cocone => this.coapex -> cc.coapex

  def checkUnivTrianglesComm (cc: Cocone[TObj, TArr], node: TObj): Boolean =
    (cc.inj(node) == C.comp(univ(cc), inj(coapex)))
}

trait Cocomplete [TObj, TArr] {
  def colimit (d: Diagram[TObj, TArr]): Colimit[TObj, TArr]
}



// -------------------------------- Experiments --------------------------------

/*
 * An attempt to put things with the scope of parameters TObja dn TArr,
 *   so that they don't have to be repeated over and over.
 */

trait PCatWithDiagrams [TObj, TArr] extends Category[TObj, TArr] {
  trait Diagram {
    def nodes: Set[TObj];
    def edges: Set[TArr];
  }
}

trait Cocone2 [TObj, TArr] {
  val C: PCatWithDiagrams[TObj, TArr];
  val base: C.Diagram;
  def coapex: TObj;
  def inj : TObj => TArr; // n: nodes => n -> coapex

  def checkCoconeFacesComm (edge: TArr): Boolean =
    (inj(C.dom(edge)) == C.comp(inj(C.cod(edge)), edge))
}

trait Cocomplete2 [TObj, TArr] extends PCatWithDiagrams[TObj, TArr] {
  def colimit (d: this.Diagram): Colimit[TObj, TArr]
}


// trait ColimitsDefs [TObj, TArr] extends PCatWithDiagrams[TObj, TArr]  { C =>
//   abstract class Cocone (val base: Diagram) {
//     val coapex: TObj;
//     //val base: Diagram;
//     val inj : TObj => TArr; // n: nodes => n -> coapex
//     def coconeFaceComms (edge: TArr): Boolean =
//       (inj(dom(edge)) == C.comp(inj(cod(edge)), edge))
//   }

//   abstract class Colimit (base: Diagram) extends Cocone(base) {
//     val univ : Cocone => TArr; // cc: Cocone => this -> cc
//     def univTriComms (cc: Cocone, node: TObj): Boolean = 
//       (cc.inj(node) == C.comp(univ(cc), inj(coapex)))
//   }

//   trait HasColimits {
//     def colimit (d: Diagram): Colimit
//   // { final val base = d }
//   }
// }
