
# 6 TailRec

(*Note*: this section is not in the original Typeclassopedia, it is specific to
Scala and was written by Joseph Abrahamson.)

`TailRec` is an extension of `Monad` which is especially important in Scala.
Generally, functional programming uses a lot of recursion and can therefore
face stack overflows if tail recursion optimizations cannot be performed.
Monads often capture a "computation within a context" but by default the
`Monad` typeclass prevents you from writing efficient tail-recursive loops.

For instance, a monadic left fold:

```scala
final def fold[F[_]: Monad, A, R](r0: R, k: (A, R) => F[R])(as0: List[A]): F[R] = {
  val tc = implicitly[Monad[F]]
  def go(r: R, as: List[A]): F[R] = as match {
    case Nil => tc.pure(r)
    case head :: tail => k(head, r).flatMap(newR => go(newR, tail)) // ***
  }
  go(r0, as0)
}
```

Normally, the inner worker function `go` would be tail-recursive and this is an
efficient, stack safe algorithm. However, in this case the line marked `***`
contains a tail-call that isn't in the final position: it can't be optimized.

This is a general problem with writing generic monadic code in Scala (and other
strict langauges with limited tail call optimization). Monadic code often has
the effect of interleaving layers of Scala calls with calls to `flatMap`.

That said, it's often easy to write efficient tail loops for specific monads.

```scala
final case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  def pure[A](a: A): Id[A] = Id(a)

  final def fold[A, R](r0: R, k: (A, R) => Id[R])(as0: List[A]): Id[R] = {
    @tailrec def go(r: R, as: List[A]): Id[R] = as match {
      case Nil => pure(r)
      case head :: tail => go(k(head, r).value, tail) // ***
    }
    go(r0, as0)
  }
}
```

Now line `***` exploits the ability to "run" the `Id` computation and return an
"unwrapped" value to the tail-call to `go`.

The `TailRec` class captures the notion of monads which offer a general strategy
for writing tail-recursive, stack-safe loops.

# 6.1 Definition

```scala
trait TailRec[F[_]] extends Monad[F] {
  def tailRecM[S, R](step: S => F[Either[S, R]])(s: S): F[R]
}
```

So `TailRec` offers exactly one new method for our monadic type `F[_]`. What
does `tailRecM` offer? We can get an intuition by following the types.

- You give `tailRecM` a `step` function that reports results in the
  computational context of `F`. Given a value of type `S` it either gives us
  another `S` or a `R`.
- Ultimately, `tailRecM` returns a value `R` wrapped in the context of
  `F`. Immediately, this must come from the step function returning `Right(b)`
  as there's no other way to obtain values of type `R`.
- We finally provide `tailRecM` a starting value of type `S` which lets it kick
  off calling `step`.

So the intuition is that `tailRecM` repeatedly calls `step` on values of type
`S` that it has. Each time we'll either get a new "seed" value of type `S` or a
"return" value of type `R`. We can only stop if we get the `R` type, otherwise
we have to loop.

Another way of looking at it is to instantiate the type of `tailRecM` where
`F[A] = A` (another way of seeing the identity monad). We can even give a
`@tailrec` implementation.

```scala
@tailrec def tailRecM[S, R](step: S => Either[S, R])(s: S): R =
  step(s) match {
    case Left(s1) => tailRecM(step)(s1)
    case Right(r) => r
  }
```

In this form it's clear that it is just a kind of generalized loop---just the
thing we want our monads to provide fast implementations of.

So, here's the key part: instance of `TailRec` must provide instances of
`tailRecM` which use at most constant stack space outside of calls to `step`.
Let's see some examples.

# 6.2 Instances

## Identity

We just showed the instance for identity above, but to make it utterly clear we
can use the concrete implementation of `Id`.

```scala
implicit object IdTailRec extends TailRec[Id] {
  @tailrec def tailRecM[S, R](step: S => Id[Either[S, R]])(s: S): Id[R] = 
    step(s).value match {
      case Left(s1) => tailRecM(step)(s1)
      case Right(r) => r
    }
}
```

Again, notice that we use `.value` to "run" the identity computation in each
loop. Generally, if you have a means to "run" your monadic type then you can
exploit it to make fast tail recursive loops.

## Option

`Option` isn't much more complex than `Id`, so we ought to be able to write a
`TailRec` instance for it.

```scala
implicit object OptionTailRec extends TailRec[Option] {
  @tailrec def tailRecM[S, R](step: S => Option[Either[S, R]])(s: S): Option[R] = 
    step(s) match {
      case Some(Left(s1)) => tailRecM(step)(s1)
      case Some(Right(r)) => Some(r)
      case None           => None
    }
}
```

Here we cannot "run" an `Option` value directly (it might fail), but we can
expand the pattern match to cover all of the cases.

## Reader

`Reader` can be handled in essentially the same way `Id` was: we "run" the computation.

```scala
implicit def ReaderTailRec[E] extends TailRec[E => ?] {
  @tailrec def tailRecM[S, R](step: S => E => Either[S, R])(s: S): E => R = e =>
    step(s)(e) match {
      case Left(s1) => tailRecM(step)(s1)(e)
      case Right(r) => r
    }
}
```

## Writer 

Writer requires some new strategies, but they should be familiar: it's the
standard accumulator transform used to make tail-recursive loops in normal
code.

```scala
implicit def WriterTailRec[E](implicit Mon: Monoid[E]) = new TailRec[(E, ?)] {
  def tailRecM[S, R](step: S => (E, Either[S, R]))(s: S): (E, R) = {

    // we could also use a while loop
    @tailrec def go(s: S, ann: E): (E, R) = step(s) match {
      case (ann1, Left(s1)) => go(s1, Mon.plus(ann, ann1))
      case (ann1, Right(r)) => (Mon.plus(ann, ann1), r)
    }
    
    go(s, Mon.zero)
}
```

## Unit

A silly but law abiding monad can be written like so

```scala
final case class Ignore[A]() {
  def map[B](f: A => B): Ignore[B] = Ignore()
  def flatMap[B](f: A => Ignore[B]): Ignore[B] = Ignore()
}

object Ignore {
  def pure[A](a: A): Ignore[A] = Ignore()
}
```

The `TailRec` instance of `Ignore` is also particularly silly

```scala
implicit object IgnoreTailRec[E] extends TailRec[(E, ?)] {
  def tailRecM[S, R](step: S => (E, Either[S, R]))(s: S): (E, R) = Ignore()
}
```

This just goes to show that `tailRecM` may not even loop if the underlying
monad doesn't support it.

## Continuation passing style

As mention before, continuation passing style is not popular in Scala since
it's not thought to be possible to get CPS code to tail call optimize (though
it is theoretically valid).

For this reason, you cannot write an instance of `TailRec` for `Cont`.

# Exercises

- Write a `TailRec` instance for `State`.
- Attempt to write a `TailRec` instance for `Cont`. Make an argument for why
  it's impossible.

# 6.3 Laws

The only law for `TailRec` has already been stated: `tailRecM` cannot add more
than constant stack usage to a computation. This law is different than the
others that we've described in the Typeclassopedia in that it's
*non-equational*.

Generally, however, it's easy to validate that an implementation of `tailRecM`
is ok: it should either be `@tailrec` or non-recursive and consisting only of
tail-recursive calls. It's also a good style to implement `tailRecM` using
things like while loops or even very careful mutability. Users of the `TailRec`
interface will not be exposed to these "unsafe" implementation techniques.

# 6.4 Common utility functions

Most of the utility functions from Chapter 5 *actually* work best when
`TailRec` is provided. In paricular, nearly all monad generic recursive
computation should be performed using `TailRec`.

In particular, many libraries providing the `Monad` typeclass will just roll
`TailRec` into it directly reasoning that any non-`TailRec` monad should not be
special-cased.

# 6.5 Further reading

General tools for stack safety have been explored for a while under the name of
"trampolines". A slight variation on the standard trampoline is even built into
Scala in
[`scala.util.control.TailCalls.TailRec`](https://www.scala-lang.org/api/current/scala/util/control/TailCalls$$TailRec.html).

In general, `tailRecM` lets you encode very general recursion into a monad and
so the academic literature on computational effects has also long studied this
design under the name ["completely iterative
monad"](https://www.cs.ox.ac.uk/ralf.hinze/WG2.8/22/slides/tarmo.pdf).

The trampoline method can be seen as a specific instantiation of the stack safe
"free" monad suggesting that the free monad can be the basis for general
stack-safe monadic computation. [Runar Bjarnason writes about this
technique](http://blog.higher-order.com/assets/trampolines.pdf) and then Phil
Freeman [extended it](http://functorial.com/stack-safety-for-free/index.pdf) to
the generalized, type class encoded method explored here.

Continuation passing style *ought* to be able to admit an instance of `TailRec`
as CPS is often used specifically as a technique for compiling heavily
recursive code to run without using the stack. Unfortunately, for this to work
the compiler has to recognize that it can compile away intermediate stack
frames not only when a function calls itself in tail position but also when it
calls *any* function which will not need a return. CPS in effect translates *all*
functions to this form, but this optimization does not appear to be possible
currently on the JVM.
