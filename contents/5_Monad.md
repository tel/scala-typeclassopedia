# 5 Monad

It’s a safe bet that if you’re reading this, you’ve heard of monads—although
it’s quite possible you’ve never heard of `Applicative` before, or `Arrow`, or
even `Monoid`. Why are monads such a big deal in Scala? There are several
reasons.

- Haskell has singled out monads for special attention by making them the
  framework in which to construct I/O operations. The functional programming
  community at large and functional programming in Scala specifically are both
  influenced by Haskell and have inherited this bias.

- Scala also singles out monads for special attention by providing a special
  syntactic sugar for monadic expressions: the `for`-comprehension. 

- Monad has been around longer than other abstract models of computation such
  as `Applicative` or `Arrow`.

- The more monad tutorials there are, the harder people think monads must be,
  and the more new monad tutorials are written by people who think they finally
  “get” monads (the monad tutorial fallacy).

I will let you judge for yourself whether these are good reasons.

In the end, despite all the hoopla, Monad is just another type class. Let’s
take a look at its definition.

# 5.1 Definition

```scala
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(k: A => F[B]): F[B]

  def seq[A, B](fa: F[A], fb: F[B]): F[B] = 
    flatMap(fa)(_ => fb)

  def ap[A, B](ff: F[A => B], fa: F[A]): F[B] =
    flatMap(ff)(f => flatMap(fa)(a => pure(f(a))))
}
```

*Note* "seq" is a non-standard name. You might also call it "semicolon".

Let’s examine the methods in the `Monad` class one by one.

We can see that `seq` is a specialized version of `flatMap`, with a default
implementation given. It is only included in the type class declaration so that
specific instances of `Monad` can override the default implementation of `seq`
with a more efficient one, if desired. Also, note that although 

```scala
def seq[A, B](_: F[A], n: F[B]) = n
```

would be a type-correct implementation of `seq`, it would not correspond to
the intended semantics: the intention is that `seq(m, n)` ignores the
*result* of `m`, but not its *effects*.

The only really interesting thing to look at—and what makes `Monad` strictly
more powerful than `Applicative`—is `flatMap`, which is also sometimes called
"bind".

We could spend a while talking about the intuition behind `flatMap`—and we will.
But first, let’s look at some examples.

# 5.2 Instances

Even if you don’t understand the intuition behind the `Monad` class, you can
still create instances of it by just seeing where the types lead you. You may
be surprised to find that this actually gets you a long way towards
understanding the intuition; at the very least, it will give you some concrete
examples to play with as you read more about the `Monad` class in general.

## `Id`

The simplest possible instance of `Monad` is `Id`, which is described in Dan
Piponi’s highly recommended blog post on [The Trivial
`Monad`](http://blog.sigfpe.com/2007/04/trivial-monad.html). Despite being
“trivial”, it is a great introduction to the `Monad` type class, and contains
some good exercises to get your brain working.

```scala
// note: this is sometimes defined as a type synonym which can be convenient,
// but is also slightly tricky. We'll make it a concrete class.
final case class Id[A](value: A)

object Id {
  implicit object IdMonad extends Monad[Id] {
    def pure[A](a: A): Id[A] = Id(a)
    def flatMap[A, B](fa: Id[A])(k: A => Id[B]): Id[B] =
      k(fa.value)
  }
}
```

## `Option`

The next simplest instance of `Monad` is `Option`. We already know how to write
`pure` for `Option`. So how do we write `flatMap`? Well, let’s think about its
type. Specializing for `Option`, we have

```scala
def flatMap[A, B](fa: Option[A])(k: A => Option[B]): Option[B]
```

If the first argument to `flatMap` is `Some(x)`, then we have something of type `A`
(namely, `x`), to which we can apply the second argument—resulting in a `Option[B]`,
which is exactly what we wanted. What if the first argument to `flatMap` is
`None`? In that case, we don’t have anything to which we can apply the `A =>
Option[B]` function, so there’s only one thing we can do: yield `None`. This
instance is:

```scala
implicit object OptionMonad extends Monad[Option] {
  def pure[A](a: A): Option[A] = Some(a)
  def flatMap[A, B](fa: Id[A])(k: A => Option[B]): Option[B] = fa match {
    case None => None
    case Some(x) => k(x)
  }
}
```

We can already get a bit of intuition as to what is going on here: if we build
up a computation by chaining together a bunch of functions with `flatMap`, as soon
as any one of them fails, the entire computation will fail (because
`flatMap(None)(f)` is `None` , no matter what `f` is). The entire computation
succeeds only if all the constituent functions individually succeed. So the
`Option` monad models computations which may fail.

## `Stream`

The Monad instance for `Stream` is similar to its `Applicative` instance; see the
exercise below.

## `Future`

Of course, the `Future` constructor is famously a `Monad`---or it at least
implements the `Monad` interface. Some people argue that it doesn't satisfy the
laws in a very fundamental way, but in practical use it often gets close
enough. `Future` represents values which will "eventually" be available and
`flatMap`-ing `Future`s represents chaining callbacks which transform
"eventual" values once they "arrive".

## Reader

```scala
type Reader[E, A] = E => A
```

`Reader[E, ?]` is known as the "reader" monad, since it describes computations
in which a value of type `E` is available as a read-only environment. Here,
`flatMap` sequences pure computation passing the read-only context to each.

Associated functionality with the reader monad includes `def ask: Reader[E, E]`
which retreives the read-only context into the current computation and `def
local[E](xf: E => E)(ma: Reader[E, A]): Reader[E, A]` which lets you "locally"
transform the read-only context.

*Note*: The monad functionality for `E => ?` has a close connection to SKI
combinator calculus: `pure` is `K` while `flatMap` is `S`.

## Writer

The type constructor `(W, ?)` is known as the "writer" monad because a result
of type `A` can be seen as a computation of type `A` with an associated
annotation or "log" of type `W`. This type constructor is _only_ a `Monad` when
`W` is a `Monoid` (see section on `Monoid`s).

Associated functionality with the writer monad includes `def tell(w: W):
Writer[W, Unit]` which performs logging.

## State

```scala
final case class State[S, A](run: S => (A, S))

object State {
  implicit object StateMonad extends Monad[State] {
    def pure[A](a: A): State[A] = State(s => (a, s))
    def flatMap[A, B](fa: State[A])(k: A => State[B]): State[B] =
      State { s0 => 
        val (a, s1) = fa.run(s)
        val (b, s2) = k(a).run(s1)
        (b, s2)
      }
  }
}
```

The state monad is a function `S => (A, S)`. Something of type `State[S, A]`
represents a stateful computation which produces an `A` but can access and modify
the state of type `S` along the way. 

Associated `State`-specific functions include `def get: State[S, S]` which
reads the current state, `def put(newS: S): State[S, Unit]` which overwrites
the current state, and `def modify(f: S => S)(ma: State[S, A]): State[S, A]`
which applies a function to the state.

## Cont

```scala
final case class Cont[R, A](run: (A => R) => R)
```

The `Cont` monad represents computations in continuation-passing style. It can
be used to suspend and resume computations, and to implement non-local
transfers of control, co-routines, other complex control structures—all in a
functionally pure way. Cont has been called the ["mother of all
monads"](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html) because of
its universal properties.

*Note*: In Haskell, due to its lazy evaluation semantics, `Cont` is a useful
abstraction. In Scala due to strictness and poor tail call optimization,
continuation passing style cannot be well-optimized and can easily blow the
stack. For this reason, you rarely see `Cont`, despite its incredible power.

# Exercises

- Implement a `Monad` instance for `Stream`. Follow the types!
- Implement a `Monad` instance for `Reader[E, ?]` or `E => ?`.
- Implement `Functor` and `Monad` instances for `Free[F, ?]` defined below. You
  may assume that `F` has a `Functor` instance. This is known as the free monad
  built from the functor `F` (*Note*: this version is simple, but not
  stack-safe in Scala. Stack-safe free monads are also possible, but slightly
  more complex.)

```scala
sealed trait Free[F[_], A]

object Free {
  final case class Var[F[_], A](a: A) extends Free[F, A]
  final case class Node[F[_], A](wrapped: F[Free[F, A]]) extends Free[F, A]
}
```

# 5.3 Intuition

Let’s look more closely at the type of `flatMap`. The basic intuition is that
it combines two computations into one larger computation. The first argument,
`F[A]`, is the first computation. However, it would be boring if the second
argument were just an `F[B]`; then there would be no way for the computations
to interact with one another (actually, this is exactly the situation with
`Applicative`).  So, the second argument to `flatMap` has type `A => F[B]`: a
function of this type, given a result of the first computation, can produce a
second computation to be run. In other words, `flatMap(x)(k)` is a computation
which runs `x`, and then uses the result(s) of `x` to decide what computation
to run second, using the output of the second computation as the result of the
entire computation.

Intuitively, it is this ability to use the output from previous computations to
decide what computations to run next that makes `Monad` more powerful than
`Applicative`. The structure of an `Applicative` computation is fixed, whereas
the structure of a `Monad` computation can change based on intermediate
results. This also means that parsers built using an `Applicative` interface
can only parse context-free languages; in order to parse context-sensitive
languages a `Monad` interface is needed.∗

(*Note*: This is _slightly_ untrue. Through careful use of general recursion
and laziness we can recursively construct infinite grammars, and hence
`Applicative` (together with `Alternative`) is enough to parse any
context-sensitive language with a finite alphabet. See [Parsing
context-sensitive languages with
Applicative](http://byorgey.wordpress.com/2012/01/05/parsing-context-sensitive-languages-with-applicative/).

To see the increased power of `Monad` from a different point of view, let’s see
what happens if we try to implement `flatMap` in terms of `map`, `pure`, and
`ap`. We are given a value `x` of type `F[A]`, and a function `k` of type `A =>
F[B]`, so the only thing we can do is apply `k` to `x`. We can’t apply it
directly, of course; we have to use `map` to lift it over the `F`. But what is the
type of `map(k)`? Well, it’s `F[A] => F[F[A]]`. So after we apply it to `x`, we are
left with something of type `F[F[B]]`—but now we are stuck; what we really want
is an `F[B]`, but there’s no way to get there from here. We can add `F`’s using
`pure`, but we have no way to collapse multiple `F`’s into one.

This ability to collapse multiple `F`’s is exactly the ability provided by the
function `def flatten[A](ffa: F[F[A]]): F[A]`, and it should come as no surprise that
an alternative definition of `Monad` can be given in terms of `flatten`:

```scala
trait Monad2[F[_]] extends Applicative[F] {
  def flatten[A](ffa: F[F[A]]): F[A]
}
```
  
In fact, the canonical definition of monads in category theory is in terms of
`pure`, `map`, and `flatten` (often called `η`, `T`, and `μ` in the
mathematical literature). Scala uses an alternative formulation with `flatMap`
instead of `flatten` since it is more convenient to use. However, sometimes it
can be easier to think about `Monad` instances in terms of flatten, since it is
a more “atomic” operation.

(*Note*: You might hear some people claim that the definition in terms of
`pure`, `map`, and `flatten` is the “math definition” and the definition in terms
of `pure` and `flatMap` is something specific to Scala. In fact, both
definitions were known in the mathematics community long before Scala picked up
monads.)

# Exercises

- Implement `flatMap` in terms of `map` and `flatten`.
- Now implement `flatten` and `map` in terms of `flatMap` and `pure`.

# 5.4 Common utility functions

Monads provide enough of an interface to provide a number of convenient utility
functions, all implemented in terms of the basic `Monad` operations (`pure` and
`flatMap` in particular). We have already seen one of them, namely, `flatten`.
We also mention some other noteworthy ones here; implementing these utility
functions oneself is a good exercise. For a more detailed guide to these
functions, with commentary and example code, see Henk-Jan van Tuyl’s
[tour](http://members.chello.nl/hjgtuyl/tourdemonad.html) of the Haskell
`Control.Moand` module.

- `def sequence[F[_]: Monad](fs: List[F[A]]): F[List[A]]` takes a list of
  computations and combines them into one computation which collects a list of
  their results. We talk about `sequence` in relation to `Monad`s as this is
  often the convention, but it actually only needs an `Applicative` constraint
  (see the exercise at the end of the Utility Functions section for
  Applicative). Note that the actual type of `sequence` is more general, and
  works over any `Traverse` rather than just lists; see the section on
  `Traverse`.

- `def replicateM[F[_]: Monad, A](n: Int, fa: F[A]): F[List[A]]` is simply a
  combination of `List.fill` and `sequence`. Again note that it only really
  needs an `Applicative` constraint.

- `def mapM[F[_]: Monad, A, B](k: A => F[B])(as: List[a]): F[B]` maps its first
  argument over the second, and `sequence`s the results. The `forM` function is
  just `mapM` with its arguments reversed; it is called `forM` since it models
  generalized `for` loops: the list `List[A]` provides the loop indices, and the
  function `A => F[B]` specifies the "body" of the loop for each index. Again,
  these functions actually work over any `Traverse`, not just lists, and they
  can also be defined in terms of Applicative, not Monad: the analogue of mapM
  for Applicative is called traverse.

- `def extend[F[_]: Monad, A, B](f: A => F[B])(fa: F[A]): F[B]` is just `flatMap`
  with its arguments reversed; sometimes this direction is more convenient
  since it corresponds more closely to function application.

- `def andThen[F[_]: Monad, A, B, C](fab: A => F[B], fbc: B => F[C])(a: A):
  F[C]` is sort of like function composition, but with an extra `F` on the result
  type of each function, and the arguments swapped. We’ll have more to say
  about this operation later. There is also a flipped variant, `composeM`.

Many of these functions also have “underscored” variants, such as `sequence_`
and `mapM_`; these variants throw away the results of the computations passed
to them as arguments, using them only for their side effects.

Other monadic functions which are occasionally useful include `filterM`,
`zipWithM`, `foldM`, and `forever`.

# 5.5 Laws

There are several laws that instances of `Monad` should satisfy. The standard presentation is:

```scala
flatMap(pure(a))(k)                =  k a
flatMap(m)(pure)                   =  m
flatMap(m)(x => flatMap(k(x))9h))  =  flatMap(flatMap(m)(k))(h)
```

The first and second laws express the fact that `pure` behaves nicely: if we
inject a value a into a monadic context with `pure`, and then bind to `k`, it
is the same as just applying `k` to a in the first place; if we bind a
computation `m` to `pure`, nothing changes. The third law essentially says that
`flatMap` is associative, sort of.

However, the presentation of the above laws, especially the third, is marred by
the asymmetry of `flatMap`. It’s hard to look at the laws and see what they’re
really saying. I prefer a much more elegant version of the laws, which is
formulated in terms of `andThen` ∗. Recall that `andThen` “composes” two
functions of type `A => F[B]` and `B => F[C]`. You can think of something of
type `A => F[B]` (roughly) as a function from `A` to `B` which may also have
some sort of effect in the context corresponding to `F`. `andThen` lets us
compose these “effectful functions”, and we would like to know what properties
`andThen` has. The monad laws reformulated in terms of `andThen` are:

```scala
andThen(pure, g) = g
andThen(g, pure) = g
andThen(andThen(f, g), h) = andThen(f, andThen(g, h))
```

Ah, much better! The laws simply state that `pure` is the identity of
`andThen`, and that `andThen` is associative.

*Note*: ∗ As fans of category theory will note, these laws say precisely that
functions of type `A => F[B]` are the arrows of a category with `andThen` as
composition! Indeed, this is known as the Kleisli category of the monad `F`. It
will come up again when we discuss `Arrow`s.

There is also a formulation of the monad laws in terms of fmap, return, and
join; for a discussion of this formulation, see the [Haskell wikibook page on
category theory](https://en.wikibooks.org/wiki/Haskell/Category_theory#The_third_and_fourth_laws).

# Exercises

- Given the definition `def andThen[F[_]: Monad, A, B, C](g: A => F[B], h: B
  => F[C])(a: A) = flatMap(g(a))(h)`, prove the equivalence of the above laws
  and the usual monad laws.

# `for` notation

Scala’s special `for` notation supports an “imperative style” of programming by
providing syntactic sugar for chains of monadic expressions. The genesis of the
notation lies in realizing that something like `flatMap(a)(x => flatMap(b)(_ =>
flatMap(c)(y => d)))` can be more readably written by putting successive
computations on separate lines:

```scala
flatMap(a) { x =>
flatMap(b) { _ => 
flatMap(c) { y =>
d } } }
```

This emphasizes that the overall computation consists of four computations `a`,
`b`, `c`, and `d`, and that `x` is bound to the result of `a`, and `y` is bound
to the result of `c` (`b`, `c`, and `d` are allowed to refer to `x`, and `d` is
allowed to refer to `y` as well). From here it is not hard to imagine a nicer
notation:

```scala
for { 
  x <- a
  _ <- b
  y <- c
  out <- d
} yield out
```

This discussion should make clear that `for` notation is just syntactic sugar.

*Note*: Very, very importantly, `for` notation desugars to the use of methods
on values of type `F[X]` as opposed to function calls from the relevant `Monad`
typeclass instance. This means that `for` *will not* work just because you've
got a `Monad` instance in scope---you must implement `flatMap` and `map` in
`F`'s class directly.

Alternatively, you can use an implicit "syntax" class like the following

```scala
implicit class MonadOps[F[_]: Monad, A](fa: F[A]) {
  private val tc = implicitly[Monad[F]]
  def flatMap[B](k: A => F[B]): F[B] = tc.flatMap(fa)(k)
  def map[B](f: A => B): F[B] = tc.map(f)
}
```

A final note on intuition: `for` notation plays very strongly to the
“computational context” point of view rather than the “container” point of
view, since the binding notation `x <- m` is suggestive of “extracting” a
single `x` from `m` and doing something with it. But `m` may represent some
sort of a container, such as a list or a tree; the meaning of `x <- m` is
entirely dependent on the implementation of `flatMap`. For example, if `m` is a
list, `x <- m` actually means that `x` will take on each value from the list in
turn. Scala calls `for` notation "for" notation to relate it back to the
"container" intuition, and shows how the "computational context" intuition can
be seen as a generalization of it.

Sometimes, the full power of `Monad` is not needed to desugar `for`-notation. For example,

```scala
for {
  x <- foo1
  y <- foo2
  z <- foo3
} yield g(x)(y)(z)
```

would normally be desugared to `foo1.flatMap(x => foo2.flatMap(y => foo3.map(z
=> g(x, y, z))))`, but this is equivalent to `ap(ap(ap(pure(g), foo1), foo2),
foo3)`. Sometimes it is better to use the applicative syntax (or an equivalent
one that's less messy due to using methods and infix notation) as (a) there are
more valid `Applicative`s than `Monad`s and (b) `Applicative` operations can
sometimes be more efficient or more parallelizable than `Monad` ones.

For example, consider

```scala
def g(x: Int)(y: Int): F[Int] = ???

// These could be expensive
def bar: F[Int] = ???
def baz: F[Int] = ???
 
def foo: F[Int] = for {
  x <- bar
  y <- baz
} g(x)(y)
```

`foo` definitely depends on the `Monad` instance of `F`, since the effects
generated by the whole computation may depend (via `g`) on the `Int` outputs of
`bar` and `baz`. However, `foo` can also be defined as

```scala
flatten(ap(ap(pure(g), bar), baz))
```

which may allow `bar` and `baz` to be computed in parallel, since they at least
do not depend on each other.

# 5.7 Further reading

Philip Wadler was the first to propose using monads to structure functional
programs. [His paper](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html)
is still a readable introduction to the subject.

There are, of course, numerous monad tutorials of varying quality.

A few of the best include Cale Gibbard’s [Monads as
containers](https://wiki.haskell.org/Monads_as_Containers) and [Monads as
computation](https://wiki.haskell.org/Monads_as_computation); Jeff Newbern’s
[All About Monads](https://wiki.haskell.org/All_About_Monads), a comprehensive
guide with lots of examples; and Dan Piponi’s [You Could Have Invented
Monads!](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html),
which features great exercises. Even this is just a sampling; [the monad
tutorials timeline](https://wiki.haskell.org/Monad_tutorials_timeline) is a
more complete list. (All these monad tutorials have prompted parodies like
[think of a monad](https://koweycode.blogspot.com/2007/01/think-of-monad.html)
... as well as other kinds of backlash like [Monads! (and Why Monad Tutorials
Are All
Awful)](https://ahamsandwich.wordpress.com/2007/07/26/monads-and-why-monad-tutorials-are-all-awful/)
or [Abstraction, intuition, and the “monad tutorial
fallacy”](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/).)

Other good monad references which are not necessarily tutorials include
[Henk-Jan van Tuyl’s tour](http://members.chello.nl/hjgtuyl/tourdemonad.html)
of the functions in Control.Monad, [Dan Piponi’s field
guide](http://blog.sigfpe.com/2006/10/monads-field-guide.html), Tim Newsham’s
[What’s a Monad?](http://www.thenewsh.com/~newsham/haskell/monad.html), and
Chris Smith's excellent article [Why Do Monads
Matter?](https://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/). There
are also many blog posts which have been written on various aspects of monads;
a collection of links can be found under [Blog
articles/Monads](https://wiki.haskell.org/Blog_articles/Monads).

For help constructing monads from scratch, and for obtaining a "deep embedding"
of monad operations suitable for use in, say, compiling a domain-specific
language, see the [free monad in
Cats](https://typelevel.org/cats/datatypes/freemonad.html).

One of the quirks of the `Monad` class and the Scala type system is that it is
not possible to straightforwardly declare `Monad` instances for types which
require a class constraint on their data, even if they are monads from a
mathematical point of view. For example, you can define a `Set` type which
requires an `Ordered` constraint on its data (and uses an ordered tree as its
private representation) so it cannot be easily made an instance of `Monad`. A
solution to this problem was first [described by Eric
Kidd](http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2007/03/15/data-set-monad-haskell-macros/).

There are many good reasons for eschewing `for` notation; in Haskell some have
gone so far as to [consider it
harmful](https://wiki.haskell.org/Do_notation_considered_harmful).

Monads can be generalized in various ways; for an exposition of one
possibility, see Robert Atkey’s paper on [parameterized
monads](https://bentnib.org/paramnotions-jfp.pdf), or Dan Piponi’s [Beyond
Monads](http://blog.sigfpe.com/2009/02/beyond-monads.html).

For the categorically inclined, monads can be viewed as monoids ([From Monoids
to Monads](http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html)) and
also as closure operators ([Triples and
Closure](https://blog.plover.com/math/monad-closure.html)). Derek Elkins’
article in [issue 13 of the
Monad.Reader](https://wiki.haskell.org/wikiupload/8/85/TMR-Issue13.pdf)
contains an exposition of the category-theoretic underpinnings of some of the
standard Monad instances, such
as State and Cont. Jonathan Hill and Keith Clarke have [an early
paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.53.6497)
explaining the connection between monads as they arise in category theory and
as used in functional programming. There is also a web page by Oleg Kiselyov
explaining the [history of Haskell's IO
monad](http://okmij.org/ftp/Computation/IO-monad-history.html).

Links to many more research papers related to monads can be found under
[Research papers/Monads and
arrows](https://wiki.haskell.org/Research_papers/Monads_and_arrows).
