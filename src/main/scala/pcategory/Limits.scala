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
  val hostCat: Category[TObj, TArr];
  val base:    Diagram[TObj, TArr];
  def coapex:  TObj;
  def inj :    TObj => TArr; // n: nodes => n -> coapex

  def checkCoconeFacesComm (edge: TArr): Boolean =
    (inj(hostCat.dom(edge)) == hostCat.comp(inj(hostCat.cod(edge)), edge))
}


/*
 * Colimit: a cocone is a colimit if it has the appropriate universal property
 */

trait Colimit [TObj, TArr] extends Cocone[TObj, TArr] {
  def univ : Cocone[TObj, TArr] => TArr; // cc: Cocone => this.coapex -> cc.coapex

  def checkUnivTrianglesComm (cc: Cocone[TObj, TArr], node: TObj): Boolean =
    (cc.inj(node) == hostCat.comp(univ(cc), inj(coapex)))
}


/*
 * CocompleteCat: a category with colimits for all diagrams
 */

trait CocompleteCat [TObj, TArr] extends Category[TObj,TArr] {
  def colimit (d: Diagram[TObj, TArr]): Colimit[TObj, TArr]
}

// -------------------------------- LIMITS --------------------------------

/*
 * Cone: for a given base (a diagram), a cocone has an apex and 
 *   projections from the apex to the base
 */

trait Cone [TObj, TArr] {
  val hostCat: Category[TObj, TArr];
  val base:    Diagram[TObj, TArr];
  def apex:    TObj;
  def proj :   TObj => TArr; // n: nodes => apex -> n
}


/*
 * Limit: a cone is a limit if it has the appropriate universal property
 */

trait Limit [TObj, TArr] extends Cone[TObj, TArr] {
  def univ : Cone[TObj, TArr] => TArr; // c: Cone => c.apex -> this.apex
}


/*
 * CompleteCat: a category with limits for all diagrams
 */

trait CompleteCat [TObj, TArr] extends Category[TObj,TArr] {
  def limit (d: Diagram[TObj, TArr]): Limit[TObj, TArr]
}



// -------------------------------- Experiments --------------------------------

/*
 * An attempt to put things within the scope of parameters TObj and TArr,
 *   so that they don't have to be repeated over and over.
 */

trait PCatWithDiagrams [TObj, TArr] extends Category[TObj, TArr] {
  trait Diagram {
    def nodes: Set[TObj];
    def edges: Set[TArr];
  }
}

// trait ColimitsDefs [TObj, TArr] extends PCatWithDiagrams[TObj, TArr]  { C =>
//   abstract class Cocone (val base: Diagram) {
//     val coapex: TObj;
//     //val base: Diagram;
//     val inj : TObj => TArr; // n: nodes => n -> coapex
//     def coconeFaceComms (edge: TArr): Boolean =
//       (inj(dom(edge)) == hostCat.comp(inj(cod(edge)), edge))
//   }

//   abstract class Colimit (base: Diagram) extends Cocone(base) {
//     val univ : Cocone => TArr; // cc: Cocone => this -> cc
//     def univTriComms (cc: Cocone, node: TObj): Boolean = 
//       (cc.inj(node) == hostCat.comp(univ(cc), inj(coapex)))
//   }

//   trait HasColimits {
//     def colimit (d: Diagram): Colimit
//   // { final val base = d }
//   }
// }
