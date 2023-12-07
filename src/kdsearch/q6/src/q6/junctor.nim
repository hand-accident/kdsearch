## [Junctor] is a substitute for Functor.
##
## [napIt] is equivalent for `mapIt` for Functor,
## but q6 uses
## `genNap: (m[a], typedesc[b]) -> ((a -> b) -> m[b])`
## to define it, instead of `map` itself.
##
##
## q6 lacks `nap` on purpose
## because phantom types are so scary.
## Define `map`(or `nap`) for each of concrete types if you need.
##
##
## Plus it's also scary to overload
## existing `mapIt` from [std/sequtils].
runnableExamples:
  import std/[options, sugar]
  import q6

  Option.jNapImpl1Deriving anyConcept:
    ins.map(op)

  assert 5.some.napIt(it * 2) == 10.some

  type Coord[T] = tuple[x, y: T]

  Coord.jNapImpl1Deriving anyConcept:
    result.x = op(ins.x)
    result.y = op(ins.y)

  let
    ocf: Option[Coord[float]] = (2.0, 3.0).some
    ocs = ocf.napTo(c):
      c.napIt:
        $it

  assert ocs == ("2.0", "3.0").some

## Automatic implementation of super-concept from Junctor
## ======================================================
##
## .. note:: TODO: documentation [issue #58]
##
## .. note:: TODO: runnableExamples [issue #59]
##
## Automatic implementation of Junctor from sub-concept
## ====================================================
##
## There are three ways to adjust a given type to [Junctor].
##
## 1. [implicit definition of Junctor] via its sub-concept
##
## 2. [explicit definition of Junctor] via kind-determined generator macro
##
## 3. [manual definition of Junctor]: define `genNap` directly.
##
## implicit definition of Junctor
## ------------------------------
##
## If you are going to define [Junctor]'s
## sub-concepts using `-Impl-` macros,
## `-Impl-Deriving` macros helps to replace
## boring definitions of `genNap`.
##
## + macros which enables automatic implementation of [Junctor]
##     + [dBindImpl1Deriving]: only when the type is already acceptable for [Pointed].
##
## .. Attention:: Avoid re-definition carefully.
##
## .. Tip:: `-Impl-Deriving` macro given [dcJunctor]
##  internally calls [explicit definition of Junctor]
##
## .. Attention:: automatic implementation may be
##  inferior to [explicit definition of Junctor]
##  or [manual definition of Junctor] in terms of performance.
##  The result, derived to be logically equivalent,
##  is often inefficient and roundabout.
##
## Laziness is a virtue.
##
## .. note:: TODO: runnableExamples [issue #59]
##
## explicit definition of Junctor
## ------------------------------
##
## .. note:: TODO: documentation [issue #58]
##
## .. note:: TODO: runnableExamples [issue #59]
##
## manual definition of Junctor
## ----------------------------
##
## .. note:: TODO: documentation [issue #58]
##
## .. note:: TODO: runnableExamples [issue #59]

import std/[
  macros,
  sugar,
  ]
import
  context,
  q6unit,
  helpers,
  concepts


macro napTo*(ja: Junctor, v: untyped, code: untyped): untyped =
  ## napping with arbitary identifier `v`.
  runnableExamples:
    import std/[options, sugar, sequtils]
    import q6
    Option.jNapImpl1Deriving anyConcept:
      ins.map(op)

    seq.jNapImpl1Deriving anyConcept:
      ins.map(op)

    let
      soi: seq[Option[int]] = (0..5).toSeq.napTo(a):
        a.some.filter(it => it mod 2 == 0)

    assert soi == @[
      0.some, int.none, 2.some, int.none, 4.some, int.none]

    let
      soi2 = soi.napTo(oi):
        oi.napTo(i):
          i * 2

    assert soi2 == @[
      0.some, int.none, 4.some, int.none, 8.some, int.none]

  quote do:
    type
      a = `ja`.peel
      b = typeof(block:
        var `v` {.inject, used.}: a
        `code`)
    `ja`.genNap(b).applyF (it: a) => (block:
      let `v`{.inject, used.}: a = it
      `code`)

template napIt*(j: Junctor, code: untyped): untyped =
  ## napping with injected identifier `it`
  runnableExamples:
    import std/[options, sugar, sequtils]
    import q6

    seq.jNapImpl1Deriving anyConcept:
      ins.map(op)

    let
      soi: seq[Option[int]] = (0..5).toSeq.napIt:
        it.some.filter(i => i mod 3 != 0)

    assert soi == @[
      int.none, 1.some, 2.some, int.none, 4.some, 5.some]

  j.napTo(it):
    code

template voiden*(j: Junctor): untyped =
  ## `Junctor m => m a -> m unit`.
  ##
  ## equivalent for [purescript's void]
  ##
  ## .. note:: TODO: runnableExamples [issue #44]
  j.napIt:
    unit

template voidLeft*(j: Junctor, x, f: untyped): untyped =
  ## `(j: m[a], x: b, f: a -> sometype) -> m[b]`.
  ##
  ## `j.nap(f)` is evaluated and discarded.
  ##
  ## .. note:: TODO: runnableExamples [issue #45]
  j.napIt:
    discard f(it)
    x

macro jNapImpl1*(constructor, statements: untyped): untyped =
  ## macro to generate `genNap` for types that have 1 generic.
  ##
  ## * injected variables
  ##    * `ins`: `constructor[A]` object that is mapped
  ##    * `result`: `constructor[B]` object of mapping result
  ##    * `op`: `proc(a: A): B` proc to apply
  ##    * `A`: inner type before mapping
  ##    * `B`: inner type after mapping
  ##
  runnableExamples:
    import std/[options, sugar]
    import q6
    Option.q6for1
    Option.jNapImpl1:
      ins.map(op)

    assert 5.some.napIt(it * 2) == 5.some.map(it => it * 2)

  let
    ins = ident"ins"
    op = ident"op"
    A = ident"A"
    B = ident"B"
  quote do:
    template genNap[`A`](
        `ins`: `constructor`[`A`], `B`: typedesc): untyped {.used.} =
      block:
        proc napImpl(`op`: `A` -> `B`): `constructor`[`B`]
                      {.closure, gensym, effectsOf: `op`.} =
          `statements`
        napImpl

macro jNapImpl1Deriving*(constructor: untyped, deriving: openArray[
    DerivingConcept], statements: untyped): untyped =
  ## macro to generate `genNap` for types that have 1 generic.
  ##
  ## Laziness is a virtue.
  ##
  ## When `deriving` includes [dcQ6],
  ## it also defines automatically `wrap` and `peel` for [Q6].
  ##
  ## * injected variables
  ##    * `ins`: `constructor[A]` object that is mapped
  ##    * `result`: `constructor[B]` object of mapping result
  ##    * `op`: `proc(a: A): B` proc to apply
  ##    * `A`: inner type before mapping
  ##    * `B`: inner type after mapping
  ##
  runnableExamples:
    import std/[options, sugar]
    import q6
    Option.jNapImpl1Deriving anyConcept:
      ins.map(op)

    assert 5.some.napIt(it * 2) == 5.some.map(it => it * 2)

  quote do:
    when dcQ6 in `deriving`:
      `constructor`.q6for1
    `constructor`.jNapImpl1:
      `statements`

## Feature in future
## =================
##
## .. note:: TODO: jNapImpl2 [issue #46]
##
## .. note:: TODO: jNapImpl2Deriving [issue #47]

## .. importdoc:: context.nim, concepts.nim, donut.nim, fly.nim, fricative.nim, helpers.nim, q6unit.nim
## .. include:: links.rst
