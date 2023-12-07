## [Fricative] is substitute for `Applicative`.
##
## Being [Fly] and having
## `genPure: (ins: m[a], b: typedesc) -> (b -> m[b])` is needed.
##
## [Pointed] [Fly] is [Fricative].
##
## .. note:: TODO: runnableExamples [issue #27]

## Automatic implementation of Pointed from sub-concept
## ====================================================
##
## Automatic implementation of super-concept from Pointed
## ======================================================
##
## .. note:: TODO: documentation [issue #56]
##
## .. note:: TODO: runnableExamples [issue #57]

import std/[
  macros,
  ]
import
  concepts,
  q6unit,
  junctor,
  fly

macro purify*(fr: Pointed, code: untyped): untyped =
  ## .. note:: TODO: Documentation [issue #28]
  ##
  ## .. note:: TODO: runnableExamples [issue #29]
  quote do:
    (`fr`.genPure(typeof(`code`)))(`code`)

template ap*(fr0, fr1: distinct Fricative): untyped =
  ## .. note:: TODO: Documentation [issue #30]
  ##
  ## .. note:: TODO: runnableExamples [issue #31]
  fr0.driftAB(fr1):
    a(b)

macro fPureImpl1*(constructor, statements: untyped): untyped =
  ## macro to generate `genPure` for types that have 1 generic.
  ##
  ## * injected variables
  ##    * `a`: variant that is purified
  ##    * `A`: typeof `a`
  ##    * `result`: `constructor[A]` purified result
  ##
  ## .. note:: TODO: runnableExamples [issue #32]
  let
    a = ident"a"
    A = ident"A"
  quote do:
    template genPure[T](
        ins: `constructor`[T], `A`: typedesc): untyped {.used.} =
      block:
        proc pureImpl(`a`: `A`): `constructor`[`A`] {.closure, gensym.} =
          `statements`
        pureImpl


macro fPureImpl1Deriving*(constructor: untyped,
                         deriving: openArray[DerivingConcept],
                         statements: untyped): untyped =
  ## macro to generate `genPure` for types that have 1 generic.
  ##
  ## * injected variables
  ##    * `a`: variant that is purified
  ##    * `A`: typeof `a`
  ##    * `result`: `constructor[A]` purified result
  ##
  ## Not able to generate `Fly`.
  ## **See:** [fDriftImpl1Deriving]
  ##
  ## If you generate `Nap` from here, performance can be declined.
  ##
  ## .. note:: TODO: runnableExample [issue #33]
  let
    ins = ident"ins"
    op = ident"op"
    a = ident"a"
  quote do:
    when dcQ6 in `deriving`:
      `constructor`.q6for1
    `constructor`.fPureImpl1:
      `statements`
    when dcJunctor in `deriving`:
      `constructor`.jNapImpl1:
        `ins`.driftAB(`ins`.purify(unit)):
          `op`(`a`)

## Feature in future
## =================
##
## .. note:: TODO: fPureImpl2 [issue #34]
## .. note:: TODO: fPureImpl2Deriving [issue #35]

## .. importdoc:: context.nim, concepts.nim, donut.nim, fly.nim, helpers.nim, junctor.nim, q6unit.nim
## .. include:: links.rst
