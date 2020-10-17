# Notes on implementing category theory using virtual types
### Issues with attempting to use Scala as a fully dependently-typed PL

The code given here is an attempt at implementing category theory along the lines of
Rydeheard and Burstall's
[*Computational Category Theory*](http://www.cs.man.ac.uk/~david/categories/)
(which I will refer to as __CCT__),
but using Scala's abstract type members instead of type parameters.

Scala goes a long way towards being dependently typed.
In particular, it has objects with type members and path-dependent types.
However, Scala was not designed for data-dependent types.
When coding up the category examples in the way I would if implementing in a dependently typed language like Idris or Agda,
I find that I want to do things I can't -- or that doing so is very awkward.

_However, this may be because I don't know Scala well enough.
So any help with improving the code  work would be appreciated._

Some examples follow.

#### Category

The `Category` construct is the most basic one in category theory.
In Idris, I defined it as follows:

    record Category where
      constructor IMkCategory
      Obj      : Type
      IHom     : (x, y : Obj) -> Type
      IId      : (x : Obj) -> IHom x x
      IComp    : (x, y, z : Obj) -> (g : IHom y z) -> (f : IHom x y) -> IHom x z
      IArrowEq : (x, y : Obj) -> (f, g : IHom x y) -> Type

Without going into too much detail, this defines `Category` as a
record with a type member `Obj` and a dependent type member `IHom`,
which is parameterized by two *values* of the first type and, so,
yields a family of types.
The members `IId` and `IComp` are the identity and composition
operations, whose types are dependent over values of type `Obj`.
You can ignore `IArrowEq`.
Note that there are no  functions for obtaining the domain and codomain, as they are not
needed.

While Scala is a very advanced language, it does not have
value-dependent types, so we can't have a type family like `IHom`,
nor value-dependent functions like `IId` and `IComp`.

However Scala does permit abstract type fields and I used them in my definition
(see [`Category.scala`](src/main/scala/category/Category.scala)):

    trait Category {
      type TObj;
      type TArr;
      def dom: TArr => TObj;
      def cod: TArr => TObj;
      def id:  TObj => TArr;
      def comp: (TArr, TArr) => TArr;
    }

Again without going into too much detail, this defines `Category` as a
record with a type members `TObj` and `TArr` for objects and arrows.
The remaining operations are like those in CCT, except parameterized
by the type fields `TObj` and `TArr`.
However, an implementation closer to CCT would have
`TObj` and `TArr` as type parameters.
*I may revisit this at some point*


#### Functor

In Idris, I defined the functor type as follows:

    record Functor (c : Category) (d : Category) where
      constructor MkFunctor
      OMap : (Obj c) -> (Obj d)
      IAMap : (x, y : Obj c) -> IHom c x y -> IHom d (OMap x) (OMap y)

Note that this a is a type parameterized by two `Category` *values*,
`c` and `d`.
The record contains two members whose types depend on fields *within*
these parameter.
In particular, `OMap` depends on the `Obj` fields of `c` and `d` and
`AMap` depends on `(Obj c)`, `(IHom c)` and `(IHom d)`.

So I started with  a trait parameterized by the two categories
(see [`Functor.scala`]((src/main/scala/category/Functor.scala))):

	trait Functor (val DomC: Category, val CodC: Category) {
	  def objMap: DomC.TObj => CodC.TObj;
	  def arrMap: DomC.TArr => CodC.TArr;
	}

But this gives me the error, "traits or objects may not have
parameters".

If  I had taken the path of making this an abstract class instead of a
trait, the definition ould have typed but I would have run into errors like those described below.
However, I took the path of trying to make this work as a trait.
*I may revisit this too.*

In my research on P3, I allowed objects with type parameters to be considered meta-level.
In this case, I would consider `DomC` and `CodC` to be type-level
parameters,
where the data components are ignored.
I would write `Functor` as follows (except with a double colon as they are type-level):

	trait Functor [DomC: Category, CodC: Category] {
	  def objMap: DomC.TObj => CodC.TObj;
	  def arrMap: DomC.TArr => CodC.TArr;
	}

But trying this with Scala doesn't work (as expected) and gives the error,
"traits cannot have type parameters with context bounds `: ...` nor view bounds `<% ...`".  
*What are context  and view bounds?*

However, Scala does allow these as members and
doing so gives me pretty much what I wanted in the first place:

	trait Functor {
	  val DomC: Category;
	  val CodC: Category;
	  def objMap: DomC.TObj => CodC.TObj;
	  def arrMap: DomC.TArr => CodC.TArr;
	}

An alternative would be to break out the type parameters as in CCT:

	trait Functor [C_TObj, C_TArr, D_TObj, D_TArr] {
	  def objMap: C_TObj => D_TObj;
      def arrMap: C_TArr => D_TArr;
	}

*As I mentioned above, I may reconsider this.*


#### Id Functor

Now consider the identity functor.
(This came up after writing some of the stuff below, but the story flows
better this way).
I thought it would be easy, as my definition is Idris was:

    FunctorId : (c : Category) -> Functor c c
    FunctorId c = MkFunctor id (\_,_ => id)

So I defined an `IdFunctor` class as follows:

    class IdFunctor (val C: Category) extends Functor {
      val DomC = C;
      val CodC = C ;
      val objMap = (obj: DomC.TObj) => obj;
      val arrMap = (arr: DomC.TArr) => arr; 
    }

However, this yields a bunch of `type mismatch` errors like the following:

    [error]  found   : obj.type (with underlying type IdFunctor.this.DomC.TObj)
    [error]  required: IdFunctor.this.CodC.TObj
    [error]   val objMap = (obj: this.DomC.TObj) => obj;
    [error]                                         ^

The basic problem is that Scala (correctly) doesn't remember the
definition of values.
In particular, Scala doesn't track that the values of  `DomC` and
`CodC` are both `C`.
So `DomC` and `CodC` appear to be different objects with different
`TObj` members.

I fixed it by enhancing the types of `DomC` and `CodC`
from just `Category` to one in which the types are specified:

    class IdFunctor (val C: Category) extends Functor {
      val DomC : Category { type TObj = C.TObj; type TArr = C.TArr } = C;
      val CodC : Category { type TObj = C.TObj; type TArr = C.TArr } = C ;
      val objMap = (obj: C.TObj) => obj;
      val arrMap = (arr: C.TArr) => arr;
    }

#### Product Category

The basic definition of a product category worked out well enough
(see [`CatConstructs.scala`](src/main/scala/category/CatConstructs.scala)):

    class ProdCat (val c1: Category, val c2: Category) extends Category {
      type TObj = (c1.TObj, c2.TObj)
      type TArr = (c1.TArr, c2.TArr)
      def dom  = { case (a1, a2) => (c1.dom(a1), c2.dom(a2)) };
      def cod  = { case (a1, a2) => (c1.cod(a1), c2.cod(a2)) };
      def id   = { case (o1, o2) => (c1.id(o1), c2.id(o2)) };
      def comp = { case ((g1, g2), (f1, f2)) => (c1.comp(g1, f1), c2.comp(g2, f2)) };
    }

Note that the two parameters are `val`s, which means they are members of the object created.
If they aren't `val`s, we get an error about private values escaping.

I wanted to specialize this for `FiniteCat` which is a subtype of `Category`
in which the objects and arrows are iterable.
One attempt is as follows.

    class ProdFinCat (c1: FiniteCat, c2: FiniteCat) extends ProdCat(c1, c2) with FiniteCat {
      def objIter = for {o1 <- c1.objIter; o2 <- c2.objIter} yield (o1, o2);
      def arrIter = for {a1 <- c1.arrIter; a2 <- c2.arrIter} yield (a1, a2);
      def objContains = {case (o1, o2) => c1.objContains(o1) && c2.objContains(o2)};
      def arrContains = {case (a1, a2) => c1.arrContains(a1) && c2.arrContains(a2)};
    }

However, this gives a bunch of `type mismatch` errors like:

    [error]  found   : o1.type (with underlying type ProdFinCat.this.c1.TObj)
    [error]  required: ProdFinCat.this.c1.TObj
    [error]   def objIter: Iterator[TObj] = for {o1 <- c1.objIter; o2 <- c2.objIter} yield (o1, o2);
    [error]                                                                                 ^

While the types appear to be the same, there are actually two things named `c1`:
the member defined by `ProdCat` and the parameter.
Renaming the parameter to `cp1` makes it more clear -- as then the error message has:

    [error]  found   : o1.type (with underlying type ProdFinCat.this.cp1.TObj)
    [error]  required: ProdFinCat.this.c1.TObj

Making the parameters be `val`s yields an error saying, "`override` modifier required to override concrete member",
which makes sense.
Making it an override worked:

    class ProdFinCat (cp1: FiniteCat, cp2: FiniteCat) extends ProdCat(cp1, cp2) with FiniteCat {
      override val c1 = cp1;
      override val c2 = cp2;
      def objIter = for {o1 <- c1.objIter; o2 <- c2.objIter} yield (o1, o2);
      def arrIter = for {a1 <- c1.arrIter; a2 <- c2.arrIter} yield (a1, a2);
      def objContains = {case (o1, o2) => c1.objContains(o1) && c2.objContains(o2)};
      def arrContains = {case (a1, a2) => c1.arrContains(a1) && c2.arrContains(a2)};
    }


#### Duals

I wanted to define the duals of categories (aka "Op") and functors along the lines of CCT section 3.6.
My first definition the categorical dual was as follows:

    class Dual (val c: Category) extends Category {
      type TObj = c.TObj;
      type TArr = c.TArr;
      def dom = c.cod;
      def cod = c.dom;
      def id  = c.id;
      def comp = { case (g, f) => c.comp(f, g) };
    }

Then the functorial dual was written as follows:

    class DualFunctor (val F: Functor) extends Functor {
      val C = new Dual(F.C);
      val D = new Dual(F.D);
      def objMap = F.objMap;
      def arrMap = F.arrMap;
    }

However, this gave a bunch of errors like:

    [error]  	found   : DualFunctor.this.F.C.TObj => DualFunctor.this.F.D.TObj
    [error]  	required: DualFunctor.this.C.TObj => DualFunctor.this.D.TObj

The basic problem is that `DualFunctor` yields an object where:

* `C` and `D` point to objects created by the `Dual` class,
* `F` points to the source functor and `objMap` and `arrMap` point to the same-named members in `F`.
* So `objMap` has type `F.C.TObj => F.D.TObj` and similarly for `arrMap`.

However, the `Functor` trait expects them to be typed with respect to the object being created.
Focusing on `objMap`:
* It should be that `objMap` has type `this.C.TObj => this.D.TObj`.
* Since `this.C` is assigned from `new Dual(F.C)` and `new Dual(F.C).TObj` is assigned from `F.C.TObj`, `this.C.TObj` is equal to `F.C.TObj` (and similarly for the sibling types).
* So it seems like it should type.

However the type of `new Dual(F.C)` is just `Category`
and the `new Dual(F.C).TObj = F.C.TObj` connection is lost.  *Right?*
 So `objMap` doesn't type.

After messing around a bit, I came up with the following definition of
the categorical dual that explicitly specifies the connection that was lost.

    def dualCat (c: Category): Category {
        type TObj = c.TObj;
        type TArr = c.TArr;
      } = new Category {
        type TObj = c.TObj;
        type TArr = c.TArr;
        def dom = c.cod;
        def cod = c.dom;
        def id  = c.id;
        def comp = { case (g, f) => c.comp(f, g) };
      }

Given this, the functorial dual is essentially as it was before:

      def dualFnctr (F: Functor): Functor = new Functor {
        val C = dualCat(F.C);
        val D = dualCat(F.D);
        def objMap = F.objMap;
        def arrMap = F.arrMap;
      }

*Is there a better way?*


#### Comma category

Attempting to implement the comma category ran into a lot of problems.
My first definition of a `CommaCat` class was along the following lines:

    class CommaCat (val L: Functor, val R: Functor) extends Category {
      val A = L.DomC
      val B = R.DomC
      val C = L.CodC // which should equal R.CodC
    
      case class TObj (a: A.TObj, f: C.TArr, b: B.TObj);
      case class TArr (dom: TObj, s: A.TArr, t: B.TArr, cod: TObj);
      ...
    }

Recall that `Category` starts

    trait Category {
      type TObj;
      type TArr;
      ...
    }

and `Functor` is as above.
One problem is that there's no way to specify that `L.CodC` is the
same category as `R.CodC` *(or is there?)* which yielded the following
type mismatch:

    [error]  found   : CommaCat.this.R.CodC.TArr
    [error]  required: CommaCat.this.L.CodC.TArr
    [error]     val rt : L.CodC.TArr = R.arrMap(ca.t);

The bigger problem relates to the lost connections mentiond above.
For instance, `L.CodC.TArr` and `C.TArr` are not compatible, even
though `C` is defined to be `L.CodC`:

    [error]  found   : fb.type (with underlying type CommaCat.this.L.CodC.TArr)
    [error]  required: CommaCat.this.C.TArr
    [error]     val lb = C.comp(fb, ls)

See the [`CommaCat.scala`](src/main/scala/category/CommaCat.scala)
file for a series of solutions.
