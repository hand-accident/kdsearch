#TODO: #50

import unittest

import q6
import std/sugar
import std/options

type
  HasZero = concept x, type t
    x.qZero is t
  Magma = concept x, type t
    x.qAdd(x) is t
  SemiGroup = concept x, y, z, type t
    x is Magma
    #[
      # why?this?fails?
      x.qAdd(y.qAdd(z)) == (x.qAdd(y)).qAdd(z)
    ]#
  Monoid = concept x
    x is SemiGroup
    x is HasZero

proc qZero[T](s: seq[T]): seq[T] = @[]
proc qAdd[T](s, t: seq[T]): seq[T] = s & t

check @[2, 3, 4] is Monoid

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

Option.fPureImpl1:
  a.option

Option.dBindImpl1Deriving anyConcept:
  if ins.isSome:
    op(a)
  else:
    QB.default

# this will be included in `monoid` like project
template genGuard[T](o: Option[T]): untyped =
  block:
    proc guardImpl(cond: bool): Option[T] =
      o.bindIt:
        if cond:
          o.purify(it)
        else:
          T.none
    guardImpl

proc isNotZero(i: int): bool = i != 0
proc nonInfDiv(a: float, b: int): Option[float] =
  # (a / b.float).some.filter(_ => b.isNotZero)
  ((a / b.float).some.genGuard)(b.isNotZero)

let calc = initRws((es: RwsInput[float, int]) => (
  value: es.env.nonInfDiv(es.state),
  state: es.state + 1,
  log: @[es.state.isNotZero]
))
test "type construction":
  check calc.runRws((3.0, 5)) == (3.0.some, 6, @[true])
  check calc.runRws((3.0, 0)) == (float.none, 1, @[false])

# I prefer (w is monoid) and initial value of log is W.getZero,
# but there is no implementations here now.
template genPure[R, W, S, A](rws: Rws[R, W, S, A], B: typedesc): untyped =
  block:
    proc pureImpl(b: B): Rws[R, W, S, B] =
      initRws((es: RwsInput[R, S]) => (
        value: b,
        state: es.state,
        log: W.default.qZero
      ))
    pureImpl

template genBind[R, W, S, A](rws: Rws[R, W, S, A],
    QB: typedesc): untyped {.used.} =
  block:
    assert W is SemiGroup
    proc bindImpl(op: A -> QB): QB =
      type B = typeof(op(rws.runRws(
          es).value).runRws(es).value)
      proc inner(es: RwsInput[R, S]): rws.wrap(B) =
        let
          (a, s0, w) = rws.runRws(es)
          (b, s1, w1) = op(a).runRws((env: r, state: s0))
        (value: b, state: s1, log: w.qAdd(w1))
      initRws(inner)

template genNap[R, W, S, A](ins: Rws[R, W, S, A],
    B: typedesc): untyped {.used.} =
  block:
    proc napImpl(op: A -> B): Rws[R, W, S, B] =
      ins.bindIt:
        ins.purify:
          op(it)

template genDrift[R, W, S, A](ins0: Rws[R, W, S, A],
    B, C: typedesc): untyped {.used.} =
  block:
    proc driftImpl(ins1: Rws[R, W, S, B], op: (a: A, b: B) -> C): Rws[R, W, S, C] =
      ins0.bindTo(a):
        ins1.bindTo(b):
          ins0.purify:
            op(a, b)

test "concept":
  check calc is Q6
  check calc is Pointed
  #[
    # all fails
  check calc is Bindable
  check calc is Junctor
  check calc is Fly
  check calc is Fricative
  check calc is Donut
  ]#

test "Pointed":
  check calc.purify("test").runRws((3.0, 5)) == ("test", 5, @[])

#[
test "bindable":
  echo (block:
    calc.bindIt:
      initRws((es: RwsInput[float, int]) => (
        value: es.state * it,
        state: es.state + 1,
        log: @[es.state.isNotZero])))
]#
