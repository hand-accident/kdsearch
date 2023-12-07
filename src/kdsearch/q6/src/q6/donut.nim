## [Donut] acts like monad.
##
## [bindIt] is like `>>= \it ->`,
## but q6's definition is based on
## `genBind: (m[a], typedesc[m[b]]) -> ((a -> m[b]) -> m[b])`,
## not on `>>=` itself.
##
## [Donut] must be equipped `genPure`(see: [q6/fricative]) also.
##
## .. Warning:: q6 does not check whether
##  the structure actually adaptable to monad rules;
##  e.g. `ZipList` is known that it cannot be monad,
##  but can be [Donut].
##  Then some features of monad (associative law and so on)
##  might be not available for the type.
## 
## .. Warning:: q6 does not check whether
##  the return type of bind is really `typeof m[b]`;
##  any type can be returned, but not recommended...
##  returning random type, [Donut] will be nonsense
##  and useless spaghetti.
##
## q6 lacks `bind` on purpose
## because phantom types are so scary
## and `bind` is reserved word of Nim.
##
## If you need it, you should define it
## for each of concrete types in appropriate name.
##
## And, sorry, there's nothing like "do notation".
## I found the offside rule is convenient enough.
##
## If you need it, you should write it.
runnableExamples:
  import q6
  import std/[options]

  Option.fPureImpl1:
    a.option

  assert int.none is Pointed

  Option.dBindImpl1Deriving anyConcept:
    if ins.isSome:
      result = op(ins.unsafeGet)

  assert int.none is Bindable
  assert int.none is Donut

  template filterIt[T](o: Option[T], code: untyped): untyped =
    o.bindIt:
      if code:
        o
      else:
        T.none

  let
    oStr0 = "q6".some
    oStr1 = "izquierda".some

  assert oStr0.filterIt(it.len > 2) == string.none
  # oStr0.filter(proc(it: string): bool = it.len > 2) == string.none
  assert oStr1.filterIt(it.len > 2) == oStr1
  # oStr1.filter(proc(it: string): bool = it.len > 2) == oStr1

  proc `[]`(s: Option[string], i: int): Option[char] =
    s.bindIt:
      if i in 0..<it.len:
        it[i].some
      else:
        char.none

  assert oStr0[0] == oStr1[2] # 'q'.some
  assert oStr0[2] == char.none

## Automatic implementation of super-concept from Donut
## ====================================================
##
## .. note:: TODO: documentation [issue #52]
##
## memo: if the type is [Bindable] and [Pointed],
##
##   ```nim
##   # pseudo code!
##   import std/sugar
##
##   template genNap[your, generics, moreAnd, Target](
##       ins: Constructor[your, generics, moreAnd, Target],
##       Converted: typedesc): untyped =
##     block:
##       proc napImpl(
##           op: Target -> Converted
##         ): Constructor[your, generics, moreAnd, Converted] {.closure, used.} =
##         ins.bindIt:
##           ins.purify:
##             op(it)
##       napImpl
##
##   template genDrift[your, generics, moreAnd, Target0](
##       ins0: Constructor[your, generics, moreAnd, Target0],
##       Target1, Converted: typedesc): untyped =
##     block:
##       proc driftImpl(
##           ins1: Constructor[your, generics, moreAnd, Target0],
##           op: (a: Target0, b: Target1) -> Converted
##         ): Constructor[your, generics, moreAnd, Converted] {.closure, used.} =
##         ins0.bindTo(a):
##           ins1.bindTo(b):
##             ins0.purify:
##               op(a, b)
##       driftImpl
##   ```
##
## .. note:: TODO: runnableExamples [issue #53]

import std/[macros, sugar]
import
  concepts,
  junctor,
  fly,
  fricative,
  helpers

macro bindTo*(d: Bindable, v: untyped, code: untyped): untyped =
  ## binding with arbitary identifier `v`.
  ##
  ## .. warning:: result of `code` should be type `d.wrap(SomeType)`
  ##  ([q6/context]). No validation is excuted.
  runnableExamples:
    import q6

    seq.dBindImpl1Deriving [dcQ6]:
      for a in ins:
        for b in op(a):
          result &= b

    proc duplicateElements[T](s: seq[T]): seq[T] =
      s.bindTo(elem):
        @[elem, elem]

    assert @[1, 2, 3].duplicateElements == @[1, 1, 2, 2, 3, 3]

  quote do:
    type
      a = `d`.peel
      B = typeof(block:
        var `v` {.inject, used.}: a
        `code`)
    `d`.genBind(B).applyF (it: a) => (block:
      let `v` {.inject, used.}: a = it
      `code`)

template bindIt*(d: Bindable, code: untyped): untyped =
  ## binding with injected identifier `it`.
  ##
  ## .. warning:: result of `code` should be type `d.wrap(SomeType)`
  ##  ([q6/context]). No validation is excuted.
  runnableExamples:
    import q6
    import std/[options, math]

    Option.fPureImpl1:
      a.option
    Option.dBindImpl1Deriving anyConcept:
      if ins.isSome:
        result = op(ins.unsafeGet)

    proc positiveSub(a, b: int): Option[int] =
      if a >= b:
        result = (a - b).some

    proc onlyTrueDiv(a, b: int): Option[int] =
      let (c, d) = a.divmod b
      if d == 0:
        result = c.some

    let calcResult = 11.some.bindTo(a):
      a.positiveSub(2).bindIt:
        it.onlyTrueDiv 3

    assert calcResult == 3.some

  d.bindTo(it):
    code

template dJoin*(dd: Bindable): untyped =
  ## Make `constructor[constructor[A]]` into `constructor[A]`.
  ##
  ## How [dJoin] works depends on `constructor`'s `genBind` implementation.
  runnableExamples:
    import q6
    import std/[options]

    Option.fPureImpl1:
      a.option
    Option.dBindImpl1Deriving anyConcept:
      if ins.isSome:
        result = op(ins.unsafeGet)

    assert 6.some.some.dJoin == 6.some
    assert Option[int].none.dJoin == int.none

  runnableExamples:
    import q6

    seq.fPureImpl1:
      @[a]
    seq.dBindImpl1Deriving anyConcept:
      for a in ins:
        for b in op(a):
          result &= b

    assert @[@[1, 2, 3], @[4, 5]].dJoin == @[1, 2, 3, 4, 5]

  assert dd.peel is Bindable
  dd.bindIt:
    it

macro dBindImpl1*(constructor, statements: untyped): untyped =
  ## macro to generate `genBind` for types that have 1 generic.
  ##
  ## * injected variables
  ##    * `ins`: `constructor[A]` object that is mapped
  ##    * `op`: `proc(a: A): QB` proc to apply
  ##    * `A`: inner type before binding
  ##    * `QB`: type of binding result. It should be `constructor[SomeType]`
  ##    * `result`: `QB` binding result
  ##
  runnableExamples:
    import q6
    import std/options
    Option.fDriftImpl1Deriving anyConcept:
      if insA.isSome and insB.isSome:
        result = op(insA.unsafeGet, insB.unsafeGet)

    Option.fPureImpl1:
      a.option

    Option.dBindImpl1:
      if ins.isSome:
        result = op(ins.unsafeGet)

    assert 3.some is Donut

  let
    ins = ident"ins"
    op = ident"op"
    A = ident"A"
    QB = ident"QB"
  quote do:
    template genBind[`A`](
        `ins`: `constructor`[`A`], `QB`: typedesc): untyped {.used.} =
      block:
        proc bindImpl(`op`: `A` -> `QB`): `QB`
                      {.closure, gensym, effectsOf: `op`.} =
          `statements`
        bindImpl

macro dBindImpl1Deriving*(constructor: untyped, deriving: openArray[
    DerivingConcept], statements: untyped): untyped =
  ## macro to generate `genBind` for types that have 1 generic.
  ##
  ## Laziness is a virtue.
  ##
  ## + When `deriving` includes [dcQ6],
  ##  it also defines automatically `wrap` and `peel` for [Q6]. (
  ##  [Automatic implementation of Q6 from sub-concept])
  ##
  ## [Pointed] cannot be generated from here.
  ##
  ## Use [fPureImpl1] for make the type [Pointed] and [Donut].
  ##
  ## + When the type is [Pointed] and
  ##
  ##   + `deriving` includes [dcJunctor],
  ##    it also defines automatically `genNap` for [Junctor]. (
  ##    [Automatic implementation of Junctor from sub-concept])
  ##
  ##   + `deriving` includes [dcFly],
  ##    it also defines automatically `genDrift` for [Fly]. (
  ##    [Automatic implementation of Fly from sub-concept])
  ##
  ## * injected variables
  ##    * `ins`: `constructor[A]` object that is mapped
  ##    * `op`: `proc(a: A): QB` proc to apply
  ##    * `A`: inner type before binding
  ##    * `QB`: type of binding result. It should be `constructor[SomeType]`
  ##    * `result`: `QB` binding result
  ##
  runnableExamples:
    import q6
    import std/options
    Option.fPureImpl1:
      a.option

    Option.dBindImpl1Deriving anyConcept:
      if ins.isSome:
        result = op(ins.unsafeGet)

    assert 3.some is Donut

  let
    ins = ident"ins"
    i0 = ident"insA"
    i1 = ident"insB"
    op = ident"op"
    it = ident"it"
    a = ident"a"
    b = ident"b"
  quote do:
    when dcQ6 in `deriving`:
      `constructor`.q6for1

    `constructor`.dBindImpl1:
      `statements`

    when dcJunctor in `deriving`:
      `constructor`.jNapImpl1:
        `ins`.bindIt:
          `ins`.purify:
            `op`(`it`)

    when dcFly in `deriving`:
      `constructor`.fDriftImpl1:
        `i0`.bindTo(`a`):
          `i1`.bindTo(`b`):
            `i0`.purify:
              `op`(`a`, `b`)

## Feature in future
## =================
##
## .. note:: TODO: dBindImpl2 [issue #19]
##
## .. note:: TODO: dBindImpl2Deriving [issue #20]

## .. importdoc:: context.nim, concepts.nim, fly.nim, fricative.nim, helpers.nim, junctor.nim, q6unit.nim
## .. include:: links.rst
