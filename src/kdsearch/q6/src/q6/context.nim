## One of the problems in writing monadic libraries is
## the mismatch between generic types and higher-order kind types.
##
## Since `m[a]` is a concrete type with such a name,
## so it is not easy to decompose the constructor (or `* -> *` type)
## and generic variables like in `m a`.
##
## So "q6" provides a concept of [Q6] .
##
## There are 2 methods returning types, named `peel` and `wrap`.
##
## `peel` returns `a` from `m[a]`,
## and `wrap` creates `m[b]` from `m[a]` and `b`
## (which is equivalent to taking out `m`).
##
## We are on the start line.
##
##
## .. tip:: we have small macros that automatically
##  create the methods for types which kind is:
##    * `* -> *`: [q6for1]
##    * `* -> * -> *`: [q6for2]
##  **See also**
##    + [Automatic implementation of Q6 from sub-concept]

runnableExamples:
  import q6
  import std/options

  Option.q6for1

  assert 3.some.peel is int
  assert 3.some.wrap(string) is Option[string]

runnableExamples:
  import q6

  seq.q6for1

  assert @[2, 3, 4].peel is int
  assert @[2, 3, 4].wrap(string) is seq[string]

runnableExamples:
  import q6
  type Coord[T] = tuple[x, y: T]
  proc xy[T](x, y: T): Coord[T] = (x, y)

  Coord.q6for1

  assert 2.xy(3).peel is int
  assert 2.xy(3).wrap(string) is Coord[string]

runnableExamples:
  import q6
  import std/tables

  Table.q6for2

  assert initTable[int, seq[string]]().peel is seq[string]
  assert initTable[int, seq[string]]().wrap(float) is Table[int, float]

runnableExamples:
  import q6
  import std/sugar

  type Reader[A, B] = object
    run: A -> B

  proc initReader[A, B](f: A -> B): Reader[A, B] =
    result.run = f
  Reader.q6for2

  let r = initReader((a: int) => ("value: " & $a))

  assert r.peel is string
  assert r.wrap(float) is Reader[int, float]

runnableExamples:
  import q6
  import std/sugar
  import std/options

  type
    RwsInput[R, S] = tuple[env: R, state: S]
    RwsOutput[W, S, A] = tuple[value: A, state: S, log: W]
    RwsInner[R, W, S, A] = RwsInput[R, S] -> RwsOutput[W, S, A]
    Rws[R, W, S, A] = object
      runRws: RwsInner[R, W, S, A]

  proc initRws[R, W, S, A](f: RwsInner[R, W, S, A]): Rws[R, W, S, A] =
    result.runRws = f

  template peel[R, W, S, A](rws: Rws[R, W, S, A]): untyped = A
  template wrap[R, W, S, A](rws: Rws[R, W, S, A], B: typedesc): untyped =
    Rws[R, W, S, B]

  let calc = initRws((es: RwsInput[float, int]) => (
    value: (es.env / es.state.float).some.filter(_ => es.state != 0),
    state: es.state + 1,
    log: @[es.state != 0]
  ))

  assert calc.peel is Option[float]
  assert calc.wrap(string) is Rws[float, seq[bool], int, string]

  assert calc is Q6

## Automatic implementation of Q6 from sub-concept
## ===============================================
##
## There are three ways to adjust a given type to [Q6].
##
## 1. [implicit definition of Q6] via its sub-concept
##
## 2. [explicit definition of Q6] via kind-determined generator macro
##
## 3. [manual definition of Q6]: write `peel` and `wrap`
##
## implicit definition of Q6
## -------------------------
##
## If you are going to define [Q6]'s
## sub-concepts using `-Impl-` macros,
## `-Impl-Deriving` macros helps to replace
## boring definitions of `peel` and `wrap`.
##
## + macros which enables automatic implementation of [Q6]
##     + [jNapImpl1Deriving]
##     + [fPureImpl1Deriving]
##     + [fDriftImpl1Deriving]
##     + [dBindImpl1Deriving]
##
## .. Attention:: Avoid re-definition carefully.
##
## .. Tip:: `-Impl-Deriving` macro given [dcQ6]
##  internally calls [explicit definition of Q6]
##
## Laziness is a virtue.

runnableExamples:
  import q6
  import std/[options]

  Option.fPureImpl1Deriving [dcQ6]:
    a.option

  assert 3.some.peel is int

## explicit definition of Q6
## -------------------------
##
## The two methods that make up [Q6],
## `wrap` and `peel`,
## are very simple.
## They can be derived mechanically,
## depending on the kind of type (number of its generic)
## and the position of the type variable
## you want to focus on.
##
## + macros for auto-deriving:
##   + [q6for1]
##   + [q6for2]
runnableExamples:
  import q6
  import std/[sugar]

  type
    StateInner[S, T] = tuple[state: S, value: T]
    State[S, T] = object
      runS: proc(initS: S): StateInner[S, T]

  State.q6for2

  proc initState[S, T](f: S -> StateInner[S, T]): State[S, T] =
    result.runS = f

  let s = initState((i: int) => (i + 1, $i))

  assert s.peel is string
  assert s.wrap(char) is State[int, char]

## manual definition of Q6
## -----------------------
##
## For types that do not fall under the above macros,
## we unfortunately have to define [Q6] manually.
## Fortunately, the definitions of `peel` and `wrap` are very simple.
## It is equivalent to selecting and declaring the type variables inside
## the generics that you want to change
## when you apply a higher-order function such as `map`.
##
## Good luck.
##
## .. tip:: Using `template` and leaving the return type
##  `untyped` is less likely to cause compile-time problems.

runnableExamples:
  import q6

  type
    Container[A, B] = tuple[a: A, b: B]
    ContainerFirst[A, B] = distinct Container[A, B]
    ContainerLast[A, B] = distinct Container[A, B]

  template peel[A, B](cf: ContainerFirst[A, B]): untyped = A
  template peel[A, B](cl: ContainerLast[A, B]): untyped = B

  template wrap[A, B](
      cf: ContainerFirst[A, B], T: typedesc): untyped =
    ContainerFirst[T, B]
  template wrap[A, B](
      cl: ContainerLast[A, B], T: typedesc): untyped =
    ContainerLast[A, T]

  let c: Container[string, int] = ("q", 6)

  assert ContainerFirst(c) is Q6
  assert ContainerLast(c) is Q6

import std/macros

macro q6for1*(container: untyped): untyped =
  ## make [seq], [option] or any constructor that takes one generic
  ## variant into [Q6].
  runnableExamples:
    import q6
    import std/options

    Option.q6for1

    assert 3.some.peel is int
    assert 3.some.wrap(string) is Option[string]

  runnableExamples:
    import q6

    seq.q6for1

    assert @[2, 3, 4].peel is int
    assert @[2, 3, 4].wrap(string) is seq[string]

  runnableExamples:
    import q6
    type Coord[T] = tuple[x, y: T]
    proc xy[T](x, y: T): Coord[T] = (x, y)

    Coord.q6for1

    assert 2.xy(3).peel is int
    assert 2.xy(3).wrap(string) is Coord[string]

  quote do:
    template peel[T](a: `container`[T]): untyped {.used.} = T
    template wrap[T](a: `container`[T], b: typedesc): untyped {.used.} =
      `container`[b]

macro q6for2*(container: untyped): untyped =
  ## [table], [state] or any constructor that takes two generics
  ## into [Q6].
  ##
  ## Last one of generics are target.
  ##
  ## if you apply this into another one, you need to
  ## define another type.
  ##
  ## cf: `TableOpKey[V, K] = Table[K, V]`
  runnableExamples:
    import q6
    import std/tables

    Table.q6for2

    assert initTable[int, seq[string]]().peel is seq[string]
    assert initTable[int, seq[string]]().wrap(float) is Table[int, float]

  runnableExamples:
    import q6

    type
      EitherKind = enum
        ekLeft, ekRight
      Either[L, R] = object
        case kind: EitherKind
        of ekLeft:
          left: L
        else:
          right: R

    Either.q6for2

    proc initRight[R](v: R, L: typedesc): Either[L, R] =
      Either[L, R](kind: ekRight, right: v)

    let c = 3.initRight(string)

    assert c.peel is int
    assert c.wrap(float) is Either[string, float]

  runnableExamples:
    import q6
    import std/sugar
    type Reader[A, B] = object
      run: A -> B
    proc initReader[A, B](f: A -> B): Reader[A, B] =
      result.run = f

    Reader.q6for2
    let r = initReader((a: int) => ("value: " & $a))

    assert r.peel is string
    assert r.wrap(float) is Reader[int, float]

  quote do:
    template peel[T, S](a: `container`[T, S]): untyped {.used.} = S
    template wrap[T, S](a: `container`[T, S], b: typedesc): untyped {.used.} =
      `container`[T, b]

## .. importdoc:: concepts.nim, donut.nim, fly.nim, fricative.nim, helpers.nim, junctor.nim, q6unit.nim
## .. include:: links.rst
