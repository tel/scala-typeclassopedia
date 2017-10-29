
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
  - 3.1 Definition
  - 3.2 Instances
  - 3.3 Laws
  - 3.4 Intuition
  - 3.5 Utility functions
  - 3.6 See also
- 4 Applicative
  - 4.1 Definition
  - 4.2 Laws
  - 4.3 Instances
  - 4.4 Intuition
  - 4.5 Utility functions
  - 4.6 Alternative formulation
  - 4.7 Further reading
- 5 Monad
  - 5.1 Definition
  - 5.2 Instances
  - 5.3 Intuition
  - 5.4 Utility functions
  - 5.5 Laws
  - 5.6 do notation
  - 5.7 Further reading
- 6 MonadFail
  - 6.1 Definition
  - 6.2 Law
- 7 Monad transformers
  - 7.1 Standard monad transformers
  - 7.2 Definition and laws
  - 7.3 Transformer type classes and "capability" style
  - 7.4 Composing monads
  - 7.5 Further reading
- 8 MonadFix
  - 8.1 do rec notation
  - 8.2 Examples and intuition
  - 8.3 mdo syntax
  - 8.4 Further reading
- 9 Semigroup
  - 9.1 Definition
  - 9.2 Laws
- 10 Monoid
  - 10.1 Definition
  - 10.2 Laws
  - 10.3 Instances
  - 10.4 Further reading
- 11 Failure and choice: Alternative, MonadPlus, ArrowPlus
  - 11.1 Definition
  - 11.2 Instances
  - 11.3 Laws
  - 11.4 Utility functions
  - 11.5 Further reading
- 12 Foldable
  - 12.1 Definition
  - 12.2 Instances and examples
  - 12.3 Derived folds
  - 12.4 Utility functions
  - 12.5 Foldable actually isn't
  - 12.6 Further reading
- 13 Traversable
  - 13.1 Definition
  - 13.2 Intuition
  - 13.3 Instances and examples
  - 13.4 Laws
  - 13.5 Further reading
- 14 Category
  - 14.1 Further reading
- 15 Arrow
  - 15.1 Definition
  - 15.2 Intuition
  - 15.3 Instances
  - 15.4 Laws
  - 15.5 ArrowChoice
  - 15.6 ArrowApply
  - 15.7 ArrowLoop
  - 15.8 Arrow notation
  - 15.9 Further reading
- 16 Comonad
  - 16.1 Definition
  - 16.2 Further reading
- 17 Acknowledgements
- 18 About the author
- 19 Colophon
