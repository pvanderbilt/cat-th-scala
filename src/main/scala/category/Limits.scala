package category

trait CatWithDiagrams extends Category {
  trait Diagram {
    val nodes: Set[TObj];
    val edges: Set[TArr];
  }
}

/*
 * COLIMITS
 */

trait CatWithCocones extends CatWithDiagrams { C =>
  trait Cocone {
    val coapex: TObj;
    val base: Diagram;
    val inj : TObj => TArr; // n: nodes => n -> coapex
    def coconeFaceComms (edge: TArr): Boolean =
      (inj(dom(edge)) == C.comp(inj(cod(edge)), edge))
  }

  trait Colimit extends Cocone {
    val univ : Cocone => TArr; // cc: Cocone => this -> cc
    def univTriComms (cc: Cocone, node: TObj): Boolean = 
      (cc.inj(node) == C.comp(univ(cc), inj(coapex)))
  }
}

trait CatWithColimits extends CatWithCocones {
  def colimit (d: Diagram): Colimit
  // { final val base = d }
}

/*
 * LIMITS
 */
trait CatWithCones extends CatWithDiagrams { C =>
  trait Cone {
    val apex: TObj;
    val base: Diagram;
    val proj : TObj => TArr; // n: nodes => apex -> n
    def coneFaceComms (edge: TArr): Boolean =
      (proj(cod(edge)) == C.comp(edge, proj(dom(edge))))
  }

  trait Limit extends Cone {
    val univ : Cone => TArr; // c: Cone => c -> apex
    def univTriComms (c: Cone, node: TObj): Boolean = 
      (c.proj(node) == C.comp(this.proj(apex), univ(c)))
  }
}

trait CatWithLimits extends CatWithCones {
  def limit (d: Diagram): Limit
  // { final val base = d }
}
