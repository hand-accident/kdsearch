## [Fly] is substitute for `Apply`.
##
## [driftAB] works as `liftAB`, but q6 uses
## `genDrift: (m[a], m[b], typedesc[c]) -> (((a,b) -> c) -> m[c])`
## to define it, instated of `<*>`.
runnableExamples:
  import q6
  import std/[options, math]

  Option.fDriftImpl1Deriving [dcQ6]:
    if insA.isSome and insB.isSome:
      result = op(insA.unsafeGet, insB.unsafeGet).option

  proc weirdCalc(i: int, s: string): float =
    (i xor s.len).toFloat

  let
    p = (10 ^ 3).some
    q = some"The quick brown fox jumps over the lazy dog"
    res = p.driftAB(q):
      (a + 1).weirdCalc b

  proc almostEqual(a, b: Option[float]): bool =
    let ob = driftTo(a, b, c, d):
      c.almostEqual d
    ob.get false

  assert res.almostEqual 962.0.option

## Automatic implementation of Fly from sub-concept
## ================================================
##
## Automatic implementation of super-concept from Fly
## ==================================================
##
## .. note:: TODO: documentation [issue #54]
##
## .. note:: TODO: runnableExamples [issue #55]

import std/[
  macros,
  sugar,
  ]
import
  concepts

macro driftTo*(fl0: Fly, fl1: distinct Fly, a, b, code: untyped): untyped =
  ## drifting with arbitary identifier `a`, `b`.
  runnableExamples:
    import std/[options, sugar]
    import q6

    Option.fDriftImpl1Deriving anyConcept:
      if insA.isSome and insB.isSome:
        result = op(insA.unsafeGet, insB.unsafeGet).option

    let
      a = 2.some
      b = int.none
      c = "izquierda".some
      d = string.none

    proc `[]`(s: Option[string], i: Option[int]): Option[char] =
      driftTo(s, i, bareS, bareI):
        bareS[bareI]

    assert c[a] == 'q'.some
    assert c[b] == char.none
    assert d[a] == char.none
    assert d[b] == char.none

  quote do:
    type
      A = `fl0`.peel
      B = `fl1`.peel
      C = typeof(block:
        var
          `a` {.inject, used.}: A
          `b` {.inject, used.}: B
        `code`)
    (`fl0`.genDrift(B, C))(`fl1`, (p: A, q: B) => (block:
      let
        `a` {.inject, used.}: A = p
        `b` {.inject, used.}: B = q
      `code`))

template driftAB*(fl0: Fly, fl1: distinct Fly, code: untyped): untyped =
  ## drifting with injected identifier `a` and `b`
  runnableExamples:
    import std/[options, sugar]
    import q6

    Option.fDriftImpl1Deriving anyConcept:
      if insA.isSome and insB.isSome:
        result = op(insA.unsafeGet, insB.unsafeGet).option

    proc `[]`(s: Option[string], i: Option[int]): Option[char] =
      s.driftAB(i):
        a[b]

    assert ("izquierda".some)[2.some] == 'q'.some
    assert (string.none)[2.some] == char.none

  fl0.driftTo(fl1, a, b):
    code

macro fDriftImpl1*(constructor, statements: untyped): untyped =
  ## macro to generate `genDrift` for types that have 1 generic.
  ##
  ## * injected variables
  ##    * `insA`: `constructor[A]` object that is drifting
  ##    * `insB`: `constructor[B]` object that is drifting
  ##    * `result`: `constructor[C]` object that is the drifting result
  ##    * `op`: `proc(a: A, b: B): C` proc to apply
  ##    * `A`: inner type before drifting
  ##    * `B`: inner type before drifting
  ##    * `C`: inner type after drifting
  ##
  runnableExamples:
    import std/[options, sugar]
    import q6
    Option.q6for1
    Option.jNapImpl1:
      ins.map(op)

    Option.fDriftImpl1:
      if insA.isSome:
        # napIt is valid here
        result = insB.napIt(op(insA.unsafeGet, it))

    let example = 5.some.driftAB(4.some):
      a * b
    assert example == 20.some

  let
    i0 = ident"insA"
    i1 = ident"insB"
    op = ident"op"
    A = ident"A"
    B = ident"B"
    C = ident"C"
  quote do:
    template genDrift[`A`](`i0`: `constructor`[`A`],
                           `B`, `C`: typedesc): untyped {.used.} =
      block:
        proc driftImpl(`i1`: `constructor`[`B`],
                       `op`: (a: `A`, b: `B`) -> `C`, ): `constructor`[`C`]
                       {.closure, gensym, effectsOf: `op`.} =
          `statements`
        driftImpl

macro fDriftImpl1Deriving*(constructor: untyped,
                           deriving: openArray[DerivingConcept],
                           statements: untyped): untyped =
  ## macro to generate `genDrift` for types that have 1 generic.
  ##
  ## Not able to generate Junctor from here.
  ## **See:** [fPureImpl1Deriving]
  ##
  ## * injected variables
  ##    * `insA`: `constructor[A]` object that is drifting
  ##    * `insB`: `constructor[B]` object that is drifting
  ##    * `result`: `constructor[C]` object that is the drifting result
  ##    * `op`: `proc(a: A, b: B): C` proc to apply
  ##    * `A`: inner type before drifting
  ##    * `B`: inner type before drifting
  ##    * `C`: inner type after drifting
  ##
  runnableExamples:
    import std/[options, sugar]
    import q6

    Option.fDriftImpl1Deriving [dcQ6]:
      if insA.isSome and insB.isSome:
        # napIt is invalid here
        result = op(insA.unsafeGet, insB.unsafeGet).option

    let example = 5.some.driftAB(4.some):
      a * b
    assert example == 20.some

  quote do:
    when dcQ6 in `deriving`:
      `constructor`.q6for1
    `constructor`.fDriftImpl1:
      `statements`

## Feature in future
## =================
##
## .. note:: TODO: fDriftImpl2 [issue #25]
##
## .. note:: TODO: fDriftImpl2Deriving [issue #26]

## .. importdoc:: context.nim, concepts.nim, donut.nim, fricative.nim, helpers.nim, junctor.nim, q6unit.nim
## .. include:: links.rst
