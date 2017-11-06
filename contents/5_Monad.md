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

  def andThen[A, B](fa: F[A], fb: F[B]): F[B] = 
    flatMap(fa)(_ => fb)

  def ap[A, B](ff: F[A => B], fa: F[A]): F[B] =
    flatMap(ff)(f => flatMap(fa)(a => pure(f(a))))
}
```

*Note* "andThen" is a non-standard name. You might also call it "semicolon".

Let’s examine the methods in the `Monad` class one by one.

We can see that `andThen` is a specialized version of `flatMap`, with a default
implementation given. It is only included in the type class declaration so that
specific instances of `Monad` can override the default implementation of `andThen`
with a more efficient one, if desired. Also, note that although 

```scala
def andThen[A, B](_: F[A], n: F[B]) = n
```

would be a type-correct implementation of `andThen`, it would not correspond to
the intended semantics: the intention is that `andThen(m, n)` ignores the
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
`flatMap(None, f)` is `None` , no matter what `f` is). The entire computation
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
