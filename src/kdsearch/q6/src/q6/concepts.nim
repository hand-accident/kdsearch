## Type classes and deriving flags.

import std/[sugar, sequtils]

type
  Q6* = concept x, type T
    ## the concept to extract `m` and `a` from `m[a]`
    ##
    ## **See also:**
    ## * [q6/context]
    x.peel is type
    x.wrap(x.peel) is T

  Nap[A, B, QB] = proc(f: A -> B): QB
  Junctor* = concept x of Q6
    ## mappable.
    ##
    ## **See also:**
    ## * [q6/junctor]
    x.genNap(typedesc) is Nap

  Drift[A, B, C, QB, QC] = proc(q2: QB, f: (a: A, b: B) -> C): QC
  Fly* = concept x of Q6
    ## appliable.
    ##
    ## **See also:**
    ## * [q6/fly]
    x.genDrift(typedesc, typedesc) is Drift

  Pure[A, QA] = proc(a: A): QA
  Pointed* = concept x of Q6
    ## purable type for Fricative.
    ##
    ## **See also:**
    ## * [q6/fricative]
    x.genPure(typedesc) is Pure
  Fricative* = concept x
    ## purable, appliable.
    ##
    ## **See also:**
    ## * [q6/fricative]
    ## * [q6/fly]
    x is Pointed
    x is Fly
  DBind[A, QB] = proc(f: A -> QB): QB
  Bindable* = concept x of Q6
    ## bindable type for Donut.
    ##
    ## **See also:**
    ## * [q6/donut]
    x.genBind(typedesc) is DBind
  Donut* = concept x
    ## applicative, bindable.
    ##
    ## **See also:**
    ## * [q6/donut]
    x is Bindable
    x is Pointed

  DerivingConcept* = enum
    ## Specifies which subconcepts should be automatically
    ## implemented by macros that generate "gen-" template.
    ##
    ## **See also:**
    ## * [jNapImpl1Deriving]
    ## * [fPureImpl1Deriving]
    ## * [dBindImpl1Deriving]
    dcQ6, dcJunctor, dcFly, dcPointed, dcFricative, dcBindable, dcDonut

const anyConcept* = DerivingConcept.toSeq

## .. importdoc:: context.nim, donut.nim, fly.nim, fricative.nim, helpers.nim, junctor.nim, q6unit.nim
## .. include:: links.rst
