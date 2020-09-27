# Implementing category theory using virtual types
### Issues with Scala as a dependently typed PL

This implementation attempts to implement category theory along the lines of
Rydeheard and Burstall's [*Computational Category Theory*](http://www.cs.man.ac.uk/~david/categories/)
(which I will refer to as __CCT__),
but using Scala's abstract type members instead of type parameters.
Scala goes a long way towards being dependently typed.
In particular, it has objects with type members and path-dependent types.

However, Scala was not designed for data-dependent types.
When coding up the category examples in the way I would if implementing in a dependently typed language like Idris or Agda,
I find that I want to do things I can't.

*This may be because I don't know Scala well enough.
So any help with making things work would be appreciated.*

Some examples follow.

#### Product Category

The basic definition of a product category worked out well enough (see `CatConstructs.scala`):

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

However, this gives a bunch of errors like:

    [error] /Users/pv/.../CatConstructs.scala:50:81: type mismatch;
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

#### Functor

The "obvious" definition of a functor is as having a type parameterized by the two categories (see `Functor.scala`):

	trait Functor (val C: Category, val D: Category) {
	  def objMap: C.TObj => D.TObj;
	  def arrMap: C.TArr => D.TArr;
	}

But this gives me the error, "traits or objects may not have parameters".
Clearly, the body of the trait depends only on the type members of `C` and `D` (as it must),
but Scala doesn't treat these as type parameters.

In my research on P3, I allowed objects with type parameters to be considered meta-level.
In this case, we would consder `C` and `D` to be type-level parameters, where the data components would be ignored.
We would write `Functor` as follows (except with a double colon as they are type-level):

	trait Functor [C: Category, D: Category] {
	  def objMap: C.TObj => D.TObj;
	  def arrMap: C.TArr => D.TArr;
	}

But Scala isn't designed for that and gives, "traits cannot have type parameters with context bounds `: ...` nor view bounds `<% ...`".  
*What are context  and view bounds?*

I could break out the type parameters as in CCT:

	trait Functor [C_TObj, C_TArr, D_TObj, D_TArr] {
	  def objMap: C_TObj => D_TObj;
      def arrMap: C_TArr => D_TArr;
	}

But instead I made them members which does pretty much the same thing as what I wanted in the first place.

	trait Functor {
	  val C: Category;
	  val D: Category;
	  def objMap: C.TObj => D.TObj;
	  def arrMap: C.TArr => D.TArr;
	}

#### Duals

I wanted to define the dual of a category (aka "Op") and functor along the lines of CCT section 3.6.
My first attempt at defining the categorical dual was as follows:

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

	found   : DualFunctor.this.F.C.TObj => DualFunctor.this.F.D.TObj
	required: DualFunctor.this.C.TObj => DualFunctor.this.D.TObj

The basic problem is that `DualFunctor` yields an object where

* `C` and `D` point to objects created by the `Dual` class,
* `F` points to the source functor and `objMap` and `arrMap` point to the same-named members in `F`.
* So `objMap` has type `F.C.TObj => F.D.TObj` and similarly for `arrMap`.

However, the `Functor` trait expects them to be typed wrt the object being created.
Focusing on `objMap`:
* So it should be that `objMap` has type `this.C.TObj => this.D.TObj`.
* Note that `this.C` is assigned from `new Dual(F.C)` and `new Dual(F.C).TObj` is assigned from `F.C.TObj`,
* so `this.C.TObj` is equal to `F.C.TObj` and similarly for the sibling types.
* So it seems that it should type.

BUT the type of `new Dual(F.C)` is just `Category`
and the `new Dual(F.C).TObj = F.C.TObj` connection is lost.  *Right?*
 So `objMap` doesn't type.

After messing around a bit, I came up with the following two functions:

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

Note that `dualCat` explicitly specifies the connection that was lost as described above.

      def dualFnctr (F: Functor): Functor = new Functor {
        val C = dualCat(F.C);
        val D = dualCat(F.D);
        def objMap = F.objMap;
        def arrMap = F.arrMap;
      }

*Is there a better way?*
