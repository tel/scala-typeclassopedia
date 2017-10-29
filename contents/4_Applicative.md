
# 4 Applicative

A somewhat newer addition to the pantheon of standard Haskell type classes, but
totally standardized by the time type classes began to be used in Scala,
applicative functors represent an abstraction lying in between `Functor` and
`Monad` in expressivity, first described by McBride and Paterson. The title of
their classic paper, [Applicative Programming with
Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html), gives a hint
at the intended intuition behind the `Applicative` type class. It encapsulates
certain sorts of “effectful” computations in a functionally pure way, and
encourages an “applicative” programming style. Exactly what these things mean
will be seen later.

# 4.1 Definition

Recall that `Functor` allows us to lift a “normal” function to a function on
computational contexts. But `map` doesn’t allow us to apply a function which is
itself in a context to a value in a context. Applicative gives us just such a
tool, `ap` (variously pronounced as "apply", "app", or "splat"). It also
provides a method, `pure`, for embedding values in a default, “effect free”
context. Here is the type class declaration for `Applicative`:

```scala
trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def ap[A, B](ff: F[A => B], fa: F[A]): F[B] // can exhibit poor inference
}
```

Note that every `Applicative` must also be a `Functor`. In fact, as we will
see, fmap can be implemented using the `Applicative` methods, so every
`Applicative` is a functor whether we like it or not; the `Functor` constraint
forces us to be honest.

As always, it’s crucial to understand the type signatures. First, consider
`ap`: the best way of thinking about it comes from noting that the type of `ap`
is similar to the type of `identity[A => B]: (A => B) => (A => B)`, but with
everything on the right side enclosed in an f. In other words, `ap` lifts
function application into function application for a computational context.
The type of `ap` is also very similar to the type of `map`; the only difference
is that the first parameter is `F[A => B]`, a function in a context, instead of
a “normal” function `A => B`.

`pure` takes a value of any type `A`, and returns a context/container of type
`F[A]`.  The intention is that `pure` creates some sort of “default” container
or “effect free” context. In fact, the behavior of `pure` is quite constrained
by the laws it should satisfy in conjunction with `ap`. Usually, for a given
implementation of `ap` there is only one possible implementation of `pure`.

# 4.2 Laws

Traditionally, there are four laws that `Applicative` instances should satisfy.
In some sense, they are all concerned with making sure that `pure` deserves its
name:

- The identity law: `ap(pure(x => x), v) = v
- Homomorphism: `ap(pure(f), pure(x)) = pure(f(x))`
  - Intuitively, applying a non-effectful function to a non-effectful argument
    in an effectful context is the same as just applying the function to the
    argument and then injecting the result into the context with `pure`.
- Interchange: `ap(u, pure(y)) = ap(pure(f => f(y)), u)
  - Intuitively, this says that when evaluating the application of an effectful
    function to a `pure` argument, the order in which we evaluate the function
    and its argument doesn't matter.
- Composition: `ap(u, ap(v w)) = ap(ap(ap(pure(f => g => g andThen f), u), v), w)
  - This one is the trickiest law to gain intuition for. In some sense it is
    expressing a sort of associativity property of `ap`. The reader may wish
    to simply convince themselves that this law is type-correct.

Considered as left-to-right rewrite rules, the homomorphism, interchange, and
composition laws actually constitute an algorithm for transforming any
expression using `pure` and `ap` into a canonical form with only a single use
of `pure` at the very beginning and only left-nested occurrences of `ap`.
Composition allows reassociating `ap`; interchange allows moving occurrences of
`pure` leftwards; and homomorphism allows collapsing multiple adjacent
occurrences of `pure` into one.

There is also a law specifying how `Applicative` should relate to `Functor`:

```scala
map(g)(x) = ap(pure(g), x)
```

It says that mapping a `pure` function `g` over a context `x` is the same as first
injecting `g` into a context with `pure`, and then applying it to `x` with `ap`. In
other words, we can decompose `map` into two more atomic operations: injection
into a context, and application within a context.

**Exercises**

- (Tricky) One might imagine a variant of the interchange law that says
  something about applying a pure function to an effectful argument. Using the
  above laws, prove that `ap(pure(f), x) = ap(ap(pure(x => f => f(x)), x, f))`.

# 4.3 Instances

Most of the standard types which are instances of `Functor` are also instances of
`Applicative`.

Maybe can easily be made an instance of `Applicative`; writing such an instance
is left as an exercise for the reader.

The lazy stream type constructor `Stream` can actually be made an instance of
`Applicative` in two ways; essentially, it comes down to whether we want to
think of streams as ordered collections of elements, or as contexts
representing multiple results of a nondeterministic computation (see Wadler’s
[How to replace failure by a list of
successes](http://www.springerlink.com/content/y7450255v2670167/)).

(*Ed.* Originally this used the `List` type constructor... but in Scala `List`
is _strict_ whereas in Haskell its _lazy_.)

Let’s first consider the collection point of view. 

```scala
implicit object ZippingStreamApplicative extends Applicative[Stream] {
  def pure[A](a: A): Stream[A] = ??? // exercise
  def ap[A, B](ff: Stream[A => B], fa: Stream[A]): Stream[B] =
    (ff, fa) match {
      (Stream.Empty, _) => Stream.Empty
      (_, Stream.Empty) => Stream.Empty
      (f #:: fs, a #:: as) => f(a) +: ap(fs, as)
    }
}
```

To apply a stream of functions to a stream of inputs with `ap`, we just match up
the functions and inputs elementwise, and produce a stream of the resulting
outputs. In other words, we “zip” the streams together with function application.

The other `Applicative` instance for streams, based on the nondeterministic
computation point of view, is:

```scala
implicit object NondeterministicStreamApplicative extends Applicative[Stream] {
  def pure[A](a: A): Stream[A] = Stream(a)
  def ap[A, B](ff: Stream[A => B], fa: Stream[A]): Stream[B] = ff match {
    case Stream.Empty => Stream.Empty
    case f #:: fs => fa.map(a => f(a)) ++ ap(fs, fa)
  }
}
```

Instead of applying functions to inputs pairwise, we apply each function to all
the inputs in turn, and collect all the results in a stream.

Now we can write nondeterministic computations in a natural style. To add the
numbers 3 and 4 deterministically, we can think of there being a function

```scala
def plus(x: Int)(y: Int): Int = x + y
```

and write `plus(3)(4)`. But suppose instead of 3 we have a nondeterministic
computation that might result in 2, 3, or 4; then we can write

```scala
ap(ap(pure(plus), Stream(2, 3, 4)), pure(4))
```

There are several other `Applicative` instances as well:

- `Future` is an instance of `Applicative`, and behaves exactly as you would
  think: to execute `ap(m1, m2)`, first `m1` is executed, eventually resulting
  in a function `f`, then `m2` is executed, eventually resulting in a value
  `x`, and finally the value `f(x)` is returned as the result of executing
  `ap(m1, m2)`.
- `(A, ?)` is an `Applicative`, as long as `A` is an instance of `Monoid` (see
  section `Monoid`). The a values are accumulated in parallel with the
  computation.
- One can consider a `Constant` type `final case class Constant[A, B](a: A)`; a
  value of type `Constant[A, B]` simply contains an `A`. This is an instance of
  `Applicative` for any `Monoid` `A`; this instance becomes especially useful
  in conjunction with things like `Foldable` (see section `Foldable`).

**Exercises**

- Implement an instance of `Applicative` for `Option`.
- Determine the correct definition of `pure` for the `ZippingStreamApplicative`
  instance of `Applicative[Stream]`—there is only one implementation that
  satisfies the law relating `pure` and `ap`.
  - *Hint*: you can't write this instance for `List`, only `Stream`.

# 4.4 Intuition

McBride and Paterson’s paper introduces the notation `[[g x_1 x_2 ... x_n]]` to
denote function application in a computational context. If each `x_i`  has type
`F[t_i]` for some applicative functor `F`, and `G` has type `t_1 => t_2 => ...
=> t_n => t`, then the entire expression `[[g x_1 ... x_n]]` has type `F[T]`.
You can think of this as applying a function to multiple “effectful” arguments.
In this sense, the double bracket notation is a generalization of `map`, which
allows us to apply a function to a single argument in a context.

Why do we need `Applicative` to implement this generalization of `map`? Suppose
we use `map` to apply `g` to the first parameter `x`. Then we get something of
type `F[t2 => ... t]`, but now we are stuck: we can’t apply this
function-in-a-context to the next argument with `map`. However, this is
precisely what `ap` allows us to do.

This suggests the proper translation of the idealized notation `[[g x_1 x_2 ... x_n]]` into Scala, namely

```scala
ap(ap(... ap(pure(g), x1), x2), ...), xn)
```

If we could write `ap` using infix syntax, this would be even more convenient.
In fact, often libraries implementing `Applicative` use implicit classes to
create an infix syntax. Unfortunately, due to how inference works in Scala
these must be much more complex than the typical implicit class and are out of
scope of this document.

This is what is meant by an “applicative style”—effectful computations can
still be described in terms of function application; the only difference is
that we have to use the special function `ap` for application instead of
parentheses.

Note that `pure` allows embedding “non-effectful” arguments in the middle of an idiomatic application, like

```scala
ap(ap(ap(g, x1), pure(x2)), x3)
```

which has type `F[D]`, given

```scala
def g(a: A)(b: B)(c: C): D
val x1: F[A]
val x2: B
val x3: F[B]
```

The double brackets are commonly known as “idiom brackets”, because they allow
writing “idiomatic” function application, that is, function application that
looks normal but has some special, non-standard meaning (determined by the
particular instance of `Applicative` being used).

# 4.5 Utility functions

In general, it's very painful to use `ap` directly. It can be useful in very
generic code, but more commonly you just numbered generalizations of `map` directly:

```scala
trait Applicative[F[_]] extends Functor[F] {

  def pure[A](a: A): F[A]
  def ap[A, B](ff: F[A => B], fa: F[A]): F[B]

  def map2[A, B, R](f: (A, B) => R)(a: F[A], b: F[B]): F[R] = ???
  def map3[A, B, C, R](f: (A, B, C) => R)(a: F[A], b: F[B], c: F[C]): F[R] = ???
  def map4[A, B, C, D, R](f: (A, B, C, D) => R)(a: F[A], b: F[B], c: F[C], d: F[D]): F[R] = ???

  ...
}
```

Each of these `mapN` functions can be defined in terms of `pure` and `ap`. As
an exercise, try defining `map2` and `map3` to get the pattern.

# 4.6 Alternative formulation

An alternative, equivalent formulation of `Applicative` is given by

```scala 
trait Monoidal[F[_]] extends Functor[F] {
  def unit: F[Unit]
  def prod[A, B](fa: F[A])(fb: F[B]): F[(A, B)]
}
```

Intuitively, this states that a monoidal functor is one which has some sort of
"default shape" and which supports some sort of "combining" operation. `pure`
and `ap` are equivalent in power to `unit` and `prod` (see the Exercises
below). More technically, the idea is that `f` preserves the "monoidal
structure" given by the tuple constructor `(,)` and unit type `Unit`. This can
be seen even more clearly if we rewrite the types of `unit` and `prod` as

```scala
unit: Unit => F[Unit]
prod: (F[A], F[B]) -> F[(A, B)]
```

Furthermore, to deserve the name "monoidal" (see the section on `Monoid`s),
instances of `Monoidal` ought to satisfy the following laws, which seem much more
straightforward than the traditional `Applicative` laws:

- Left identity: `prod(unit, v) ~ v`
- Right identity: `prod(u, unit) ~ u`
- Associativity: `prod(u, prod(v, w)) ~ prod(prod(u, v), w)`

where `~`, read "isomorphic to", indicates "equivalent up to re-association of
tuples and addition/elimination of `Unit` values.

These turn out to be equivalent to the usual `Applicative` laws. In a category
theory setting, one would also require a naturality law:

- Naturality: `map({ case (x, y) => (g(x), h(y)) })(prod(u, v)) = prod(map(g)(u), map(h)(v))`

but in Scala this holds as a free theorem.

Much of this section was taken from [a blog post by Edward Z.
Yang](http://blog.ezyang.com/2012/08/applicative-functors/); see his actual
post for a bit more information.

As a final note, the `Monoidal` formulation provides much better inference for
Scala so oftentimes when `Applicative` is actually implemented most of the core
operations are the `Monoidal` ones. This is not true in Haskell where inference
works equally well for both formulations and `Applicative` is able to provide
more convenient syntax. This probably explains the prevalence of the
`Applicative` formulation instead of the `Monoidal` one.

**Exercises**

- Implement `pure` and `ap` in terms of `unit` and `prod`, and vice versa.
- Are there any `Applicative` instances for which there are also functions `f(()) = ()`
  and `f((a,b)) = (f(a), f(b))`, satisfying some "reasonable" laws?
- (Tricky) Prove that given your implementations from the first exercise, the
  usual `Applicative` laws and the `Monoidal` laws stated above are equivalent.

# 4.7 Further reading

[McBride and Paterson’s original
paper](http://www.soi.city.ac.uk/~ross/papers/Applicative.html) is a
treasure-trove of information and examples, as well as some perspectives on the
connection between `Applicative` and category theory. Beginners will find it
difficult to make it through the entire paper, but it is extremely
well-motivated—even beginners will be able to glean something from reading as
far as they are able.

Conal Elliott has been one of the biggest proponents of Applicative. For
example, the [Pan library for functional
images](http://conal.net/papers/functional-images/) and the [reactive
library](http://conal.net/papers/push-pull-frp/) for functional reactive
programming (FRP) make key use of it; his blog also contains [many examples of
`Applicative` in action](http://conal.net/blog/tag/applicative-functor).
Building on the work of McBride and Paterson, Elliott
also built the [TypeCompose](https://wiki.haskell.org/TypeCompose) library,
which embodies the observation (among others) that `Applicative` types are closed
under composition; therefore, `Applicative` instances can often be automatically
derived for complex types built out of simpler ones.

Gershom Bazerman's
[post](http://comonad.com/reader/2012/abstracting-with-applicatives/) contains
many insights into applicatives.
