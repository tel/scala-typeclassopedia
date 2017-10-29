
# 2 Introduction

Have you ever had any of the following thoughts?

- What the heck is a monoid, and how is it different from a monad?
- I finally figured out how to use Future with `for`-comprehensions, and
  someone told me I should use something called Applicative and `zip3` instead.
  Um, what?
- I saw someone use (`|@|`), and when I asked Lambdabot to tell me its type, it
  printed out scary gobbledygook that didn’t even fit on one line!
- When I asked how to do something I thought was really complicated, people
  started typing things like `flatTraverse`, `map`, `flatMap`, `map3` and the
  scary thing is that they worked! Anyway, I think those people must actually
  be robots because there’s no way anyone could come up with that in two
  seconds off the top of their head.
- I tried reading Scalaz and Cats and 10 other "functional" libraries and they
  all assume I know what a Functor or Validation or Applicative is and the
  library documentation didn't help.

If you have, look no further! You, too, can write and understand concise,
elegant, FP-ish Scala code with the best of them.

There are two keys to an FP-expert Scala hacker’s wisdom:

- Understand the types. 
- Gain a deep intuition for each type class and its relationship to other type
  classes, backed up by familiarity with many examples.

It’s impossible to overstate the importance of the first; the patient student
of type signatures will uncover many profound secrets. Conversely, anyone
ignorant of the types in their code is doomed to eternal uncertainty. “Hmm, it
doesn’t compile ... maybe I’ll stick in an `map` here ... nope, let’s see ...
maybe I need another `andThen` somewhere? ... um ...”

The second key—gaining deep intuition, backed by examples—is also important,
but much more difficult to attain. A primary goal of this document is to set
you on the road to gaining such intuition. However—

> There is no royal road to FP-in-Scala. —Euclid

This document can only be a starting point, since good intuition comes from
hard work, [not from learning the right
metaphor](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/).
Anyone who reads and understands all of it will still have an arduous journey
ahead—but sometimes a good starting point makes a big difference.

It should be noted that this is not a Scala tutorial; it is assumed that the
reader is already familiar with the basics of Scala, including the standard
library, the type system, data types, implicits, and, most importantly, the
type class pattern for using implicits.

The type classes we will be discussing and their interrelationships:

![Typeclassopedia Diagram](../images/Typeclassopedia-diagram.png)

- Solid arrows point from the general to the specific; that is, if there is an
  arrow from `Foo` to `Bar` it means that every `Bar` is (or should be, or can
  be made into) a `Foo`.
- Dotted lines indicate some other sort of relationship.
- `Monad` and `ArrowApply` are equivalent.

One more note before we begin. The original spelling of “type class” is with
two words, as evidenced by, for example, the [Haskell 2010 Language
Report](http://www.haskell.org/onlinereport/haskell2010/), early papers on type
classes like [Type classes in
Haskell](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.103.5639) and
[Type classes: exploring the design
space](http://research.microsoft.com/en-us/um/people/simonpj/papers/type-class-design-space/),
and [Hudak et al.’s history of
Haskell](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.168.4008).
However, as often happens with two-word phrases that see a lot of use, it has
started to show up as one word (“typeclass”) or, rarely, hyphenated
(“type-class”). When wearing my prescriptivist hat, I prefer “type class”, but
realize (after changing into my descriptivist hat) that there's probably not
much I can do about it.

*Ed. Note on notation for Scala.* The Typeclassopedia makes use of
higher-kinded types which is a Scala language feature that's guarded by an
import: to make the compiler happy you have to write

```scala
import scala.language.higherKinds
```

in the imports section of each file which uses higher-kinded types.
Additionally, we often want to talk about "partially applied" types. By
default, the notation for discussion partial application in Scala becomes
incredibly burdensome in complex examples, so we will use a simplifying
notation. Given a type `F[_, _]` that takes two parameters and some concrete
type `A`, the type `F[A, ?]` is a new type which takes a single parameter such
that if you fill it in with a new concrete type `B` its equivalent to `F[A,
B]`. We can use `?`s in any "slot" of a type constructor we want to partially
apply all type parameters except the one marked by the `?`.

This syntax parallels the partial application syntax for functions where `_` is
used as a placeholder. Unfortunately, it's not available in Scala naturally,
but there exists a very useful compiler plugin
[non/kind-projector](https://github.com/non/kind-projector) which makes this
syntax and other related ones available in Scala. This compiler plugin is
*highly recommended* if you're working with higher-rank types in Scala.

*End of note.*

We now begin with the simplest type class of all: `Functor`.
