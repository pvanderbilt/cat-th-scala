# cat-theory-scala

Some category theory coded in Scala, based on on
[*Computational Category Theory*](http://www.cs.man.ac.uk/~david/categories/)
by Rydeheard and Burstall.
For the
[SFTTLP meetup](https://www.meetup.com/SF-Types-Theorems-and-Programming-Languages)
track (which started July 2020).

Inspired by and drawing code from Young-il Choo's
[Scala Categories](https://gitlab.com/youngil/scala-categories)
implementation,
but using traits with type members for objects and arrows.
The file [`Scala-issues.md`](./Scala-issues.md) mentions some problems
I ran into with this implementation.

There is also an alternative version, in package `pcategory`, that
goes back to
type parameters instead of type members, as the latter started to get
unwieldy
(as discussed to some extent in `Scala-issues`).
