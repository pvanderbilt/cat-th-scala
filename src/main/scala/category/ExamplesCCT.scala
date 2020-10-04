package category

// import category.Category


/*
 * The following objects implement the finite category of CCT sections 3.2.3 and 3.4.3
 */

object cat323 extends SCategory {
  override type TObj = String;
  override type TArr = String;
  val objects = Set("a", "b", "c");
  val arrows  = Set("f", "g", "h", "k",
                   /* identities */ "a", "b", "c",
                   /* closure    */ "gf", "hf");
  override def dom = {
    case "f" => "b"; case "g" => "a"; case "h" => "a"; case "k" => "b";
    case "a" => "a"; case "b" => "b"; case "c" => "c"; 
    case "gf" => "b"; case "hf" => "b"
  }
  override def cod = {
    case "f" => "a"; case "g" => "c"; case "h" => "c"; case "k" => "c";
    case "a" => "a"; case "b" => "b"; case "c" => "c";
    case "gf" => "c"; case "hf" => "c"
  }
  override def id = (s: TObj) => s;
  override def comp = {
    // Note: This is longer but more exact
    // // direct ones
    // case ("g","f") => "gf"; case ("h","f") => "hf";
    // // double identities
    // case("a","a") => "a"; case("b","b") => "b"; case("c","c") => "c"; 
    // // right identities
    // case ("f","b") => "f"; case ("k","b") => "k";
    // case ("g","a") => "g"; case ("h","a") => "h"; 
    // // left identities
    // case ("a","f") => "f";
    // case ("c","g") => "g"; case ("c","h") => "h"; case ("c","k") => "k";
    // // identities with closed arrows
    // case ("gf","b") => "gf"; case ("hf","b") => "hf";
    // case ("c","gf") => "gf"; case ("c","hf") => "hf";

    // direct ones
    case ("g","f") => "gf"; case ("h","f") => "hf";
    // identities
    case ("a",x) => x; case ("b",x) => x; case ("c",x) => x; 
    case (x,"a") => x; case (x,"b") => x; case (x,"c") => x; 
    // error
    case(_,_) => "error"
  }
}


/* 
 *  Alternate version using StringFinCat
 */

object cat323b extends StringFinCat {
  val objects = Set("a", "b", "c");
  val arrows = Set[(String, String, String)](
    // from diagram
    ("b", "f", "a"),
    ("a", "g", "c"),
    ("a", "h", "c"),
    ("b", "k", "c"),
    // closure
    ("b", "f;g", "c"),
    ("b", "f;h", "c"),
    // identities
    ("a", "a", "a"),
    ("b", "b", "b"),
    ("c", "c", "c")
  );
}


/* 
 *  Alternate version using SimpleFinCat
 */

object cat323c extends SimpleFinCat {
  type ObjId = String;
  type ArrId = String;
  val objects = Set("a", "b", "c");
  val baseArrows = Set[(String, String, String)](
    // from diagram
    ("b", "f", "a"),
    ("a", "g", "c"),
    ("a", "h", "c"),
    ("b", "k", "c"),
    // closure
    ("b", "f;g", "c"),
    ("b", "f;h", "c"));
  val arrows =
    baseArrows.map { case (d, f, c) => (d, List(f), c) } ++
    objects.map { case obj => (obj, List(), obj) } ++
    Set(("b", List("f","g"), "c"), ("b", List("f","h"), "c"));
}
