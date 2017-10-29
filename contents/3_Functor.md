
# 3 Functor

The `Functor` class is the most basic and ubiquitous type class in the Scala
libraries. A simple intuition is that a `Functor` represents a “container” of
some sort, along with the ability to apply a function uniformly to every
element in the container. For example, a list is a container of elements, and
we can apply a function to every element of a list, using map. As another
example, a binary tree is also a container of elements, and it’s not hard to
come up with a way to recursively apply a function to every element in a tree.

Another intuition is that a `Functor` represents some sort of “computational
context”. This intuition is generally more useful, but is more difficult to
explain, precisely because it is so general. Some examples later should help to
clarify the `Functor`-as-context point of view.

In the end, however, a `Functor` is simply what it is defined to be; doubtless
there are many examples of `Functor` instances that don’t exactly fit either of
the above intuitions. The wise student will focus their attention on
definitions and examples, without leaning too heavily on any particular
metaphor. Intuition will come, in time, on its own.

# 3.1 Definition

Here is the type class declaration for `Functor`:

```scala
trait Functor[F[_]] {
  def map[A, B](f: A => B)(fa: F[A]): F[B]
}
```

To understand `Functor`, then, we really need to understand `map`.

First, the `F[A]` and `F[B]` in the type signature for `map` tell us that `F`
isn’t a concrete type like `Int`; it is a sort of type function which takes
another type as a parameter. More precisely, the kind of `F` must be `* => *`.
For example, `Option` is such a type with kind `* => *`: `Option` is not a
concrete type by itself (that is, there are no values of type `Option`), but
requires another type as a parameter, like `Option[Int]`. So it would not make
sense to say instance `Functor[Integer]`, but it could make sense to say
instance `Functor[Option]`.

Now look at the type of `map`: it takes any function from `A` to `B`, and a
value of type `F[A]`, and outputs a value of type `F[B]`. From the container
point of view, the intention is that `map` applies a function to each element
of a container, without altering the structure of the container. From the
context point of view, the intention is that `map` applies a function to a
value without altering its context. Let’s look at a few specific examples below.

# 3.2 Instances

As noted before, the list constructor `List` is a functor; we can use the
standard list method `map` to apply a function to each element of a list. The
`Option` type constructor is also a functor, representing a container which
might hold a single element. The function `_.map(g)` has no effect on `None`
(there are no elements to which `g` can be applied), and simply applies `g` to
the single element inside a `Some`. Alternatively, under the context
interpretation, the list functor represents a context of nondeterministic
choice; that is, a list can be thought of as representing a single value which
is nondeterministically chosen from among several possibilities (the elements
of the list). Likewise, the `Option` functor represents a context with possible
failure. These instances are:

```scala
implicit object ListFunctor extends Functor[List] {
  def map[A, B](f: A => B)(fa: List[A]): List[B] = fa match {
    case Nil => Nil
    case a :: as => f(a) :: map(f)(as)
  }
}
```

```scala
implicit object OptionFunctor extends Functor[Option] {
  def map[A, B](f: A => B)(fa: Option[A]): Option[B] = fa match {
    case None => None
    case Some(a) => Some(f(a))
  }
}
```

As an aside, in idiomatic Scala code you will often see the letter `F` used to
stand for both an arbitrary `Functor` and an arbitrary function. In this
document, `F` represents only Functors, and `g` or `h` always represent
functions, but you should be aware of the potential confusion. In practice,
what `f` stands for should always be clear from the context, by noting whether it
is part of a type or part of the code.

There are other Functor instances in the standard library as well:

- `Either[E, ?] is an instance of `Functor`; `Either[E, A]` represents a
  container which can contain either a value of type `A`, or a value of type
  `E` (often representing some sort of error condition). It is similar to
  `Option` in that it represents possible failure, but it can carry some extra
  information about the failure as well.

- `(E, ?)` or `Tuple2[E, ?]` represents a container which holds an “annotation”
  of type `E` along with the actual value it holds. 

- `E => ?` or `Function1[E, ?]`, the type of functions which take a value of
  type `E` as a parameter, is a `Functor`. As a container, `E => A` represents
  a (possibly infinite) set of values of `A`, indexed by values of `E`.
  Alternatively, and more usefully, `E => ?` can be thought of as a context in
  which a value of type `E` is available to be consulted in a read-only
  fashion.  This is also why `E => ?` is sometimes referred to as the reader
  monad; more on this later.

- `Future` is a `Functor`; a value of type `Future` a represents a computation
  producing a value of type a "eventually". If `m: Future[A]` computes the
  value `x` "eventually", then `map(m)(g)` will "eventually" compute the value
  `g(x)` by waiting for `x` to be computed and then applying `g`.

- Many standard types from the containers library (such as `Tree`, `Map`, and
  `Vector`) are instances of `Functor`. 

**Exercises**

- Implement `Functor` instances for `Either[E, ?]` and `E => ?`.
- Implement `Functor` instances for `(E, ?)` and for `Pair`, defined as `final case class Pair[A](v1: A, v2: A)`.
  - Explain their similarities and differences.
- Implement a `Functor` instance for the type `ITree`, defined as

```scala
sealed trait ITree[A]
final case class Leaf[A](f: Int => A) extends ITree[A]
final case class Node[A](children: List[ITree[A]]) extends ITree[A]
```

- Give an example of a type of kind `* => *` which cannot be made an instance
  of `Functor` (without using `_.asInstanceOf`).
- Is this statement true or false? "The composition of two `Functor` is also a
  `Functor`."
  - If false, give a counterexample; if true, prove it by exhibiting some
    appropriate Scala code.

# 3.3 Laws

As far as the Haskell language itself is concerned, the only requirement to be
a `Functor` is an implementation of fmap with the proper type. Any sensible
`Functor` instance, however, will also satisfy the functor laws, which are part
of the definition of a mathematical functor. There are two:

```scala
map(x => x) = x => x
map(h andThen g) = map(h) andThen map(g)
```

Together, these laws ensure that `fmap g` does not change the structure of a
container, only the elements. Equivalently, and more simply, they ensure that
`fmap g` changes a value without altering its context.

(*Advanced note* Technically, these laws make f and fmap together an
endofunctor on Sca, the category of Scala types.)

The first law says that mapping the `identity` function over every item in a
container has no effect. The second says that mapping a composition of two
functions over every item in a container is the same as first mapping one
function, and then mapping the other.

As an example, the following code is a “valid” instance of `Functor` (it
typechecks), but it violates the functor laws. Do you see why?

```scala
implicit object EvilFunctorInstanceForList extends Functor[List] {
  def map[A, B](f: A => B)(fa: List[A]): List[B] = fa match {
    case Nil => Nil
    case a :: as => f(a) :: f(a) :: map(f)(as)
  }
}
```

Any Scalite worth their salt would reject this code as a gruesome abomination.

Unlike some other type classes we will encounter, a given type has at most one
valid instance of `Functor`. This can be proven via the free theorem for the
type of `map`. 

A similar argument also shows that any `Functor` instance satisfying the first
law (`map(x => x) = x => x`) will automatically satisfy the second law as well.
Practically, this means that only the first law needs to be checked (usually by
a very straightforward induction) to ensure that a `Functor` instance is valid.

(*Advanced note* This obviously wouldn't be true if we allow the use of
`asInstanceOf`. This entire article assumes no coercions, safe or otherwise.)

**Exercises**

- Although it is not possible for a `Functor` instance to satisfy the first
  `Functor` law but not the second (excluding undefined), the reverse is
  possible. Give an example of a (bogus) `Functor` instance which satisfies the
  second law but not the first.
- Which laws are violated by the evil `Functor` instance for list shown above:
  both laws, or the first law alone? Give specific counterexamples.

# 3.4 Intuition

There are two fundamental ways to think about `map`. The first has already been
mentioned: it takes two parameters, a function and a container, and applies the
function “inside” the container, producing a new container. Alternately, we can
think of `map` as applying a function to a value in a context (without altering
the context).

As with all functions in Scala, we can think of them as taking multiple
arguments at once or one at a time.  In this case, `map` by default is
"curried": it does not really take two parameters, but takes a single parameter
and returns a function. For emphasis, we can write `map`’s type with extra
parentheses: `map _: (A => B) => (F[A] => F[B])`. Written in this form, it is
apparent that `map` transforms a “normal” function (`g: A => B`) into one which
operates over containers/contexts (`map(g): F[A] => F[B]`). This transformation
is often referred to as a lift; fmap “lifts” a function from the “normal world”
into the “`F` world”.

# 3.5 Utility functions

There are a few more `Functor`-related functions which show up from time to time

# 3.6 See also

- Many standard library types in Scala have a `.map` method which does the same
  thing as the `Functor` type class. It's merely a matter of whether you want
  the definition to exist on the object as a method or "free floating" in a
  type class instance. Often, you can define a type class just by defining the
  type class methods in terms of an object method of one of their arguments and
  this is idiomatic in practice.
  - That said, we will quickly run into type classes which cannot be described
    by object methods.
  - Further, whenever a type class has multiple methods, defining them as
    object methods obscures that they should relate according to laws.
- `Functor` is a critical building-block for many FP techniques so libraries
  such as `cats` and `scalaz` both offer it and oftentimes when another library
  doesn't want to incur extra dependencies they'll define it themselves.
- "Mapping over" and "lifting" are such common ideas you'll see them all over
  by a variety of names. See if you can identify instances of `Functor`
  floating around that don't even know that they're instances of `Functor`.
