
# Scala Typeclassopedia

_A direct "translation" of [Brent Yorgey's
_Typeclassopedia_](https://wiki.haskell.org/Typeclassopedia) as if it were
written for Scala._

Brent Yorgey's _Typeclassopedia_ is a document thoroughly describing a
hierarchy of commonly used typeclasses in the Haskell standard libraries. It is
standard learning material for becoming comfortable with more advanced Haskell
idioms and has been very successful in that role.

As third-party libraries for Scala have adopted more and more of the typeclass
machinery developed in the Haskell community, it's popular to suggest someone
reads the Typeclassopedia while learning Scala as well. Unfortunately, this
involves learning a new syntax and manually figuring out how to map Haskell
concepts into Scala.

I am translating the Typeclassopedia to Scala in order to reduce this friction
and make recommending the Typeclassopedia to students of Scala easy. My hope is
that this document can provide similar benefit that the original version of the
Typeclassopedia has to Haskell learners since 2009.

## Note on libraries

My goal with this document is to make a direct translation of the
Typeclassopedia for Scala-the-language. This is not straightforward since
Scala-the-language does not include most of these typeclasses! I _could_ choose
to use a particular library as an example, but the information here is not
dependent on a library so I will not.

Instead, each description will have a "See also" section which notes
implementations of this typeclass in Scala.

## Table of Contents

- [1 Abstract](./contents/1_Abstract.md)
- [2 Introduction](./contents/2_Introduction.md)
- [3 Functor](./contents/3_Functor.md)
- [4 Applicative](./contents/4_Applicative.md)
- [5 Monad](./contents/5_Monad.md)
- 6 TailRec
- 7 MonadFail
- 8 Monad transformers
- 9 MonadFix
- 10 Semigroup
- 11 Monoid
- 12 Failure and choice: Alternative, MonadPlus, ArrowPlus
- 13 Foldable
- 14 Traversable
- 15 Category
- 16 Arrow
- 17 Comonad
- 18 Acknowledgements
- [19 About the author](./contents/18_About_the_author.md)
