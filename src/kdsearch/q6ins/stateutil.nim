import ../q6
import std/[sugar]

type
  StateInner[S, T] = tuple[state: S, value: T]
  State[S, T] = object
    runS*: proc(initS: S): StateInner[S, T]

State.q6for2

proc initState[S, T](f: S -> StateInner[S, T]): State[S, T] =
  result.runS = f

proc getState[T](): State[T, T] =
  initState((t: T) => (t, t))

proc putState[T](t: T): State[T, Unit] =
  initState((_: T) => (state: t, value: unit))

proc runState[S, T](st: State[S, T], s: S): StateInner[S, T] =
  (st.runS)(s)

proc evalState[S, T](st: State[S, T], s: S): T =
  st.runState(s).value

proc execState[S, T](st: State[S, T], s: S): S =
  st.runState(s).state

template genPure[S, T](sg: State[S, T], A: typedesc): untyped =
  block:
    proc pureImpl(a: A): State[S, A] {.closure, gensym.} =
      initState((s: S) => (state: s, value: a))
    pureImpl

template genBind[S, A](sg: State[S, A], QB: typedesc): untyped =
  block:
    proc bindImpl(op: A -> QB): QB {.closure, gensym, effectsOf: op.} =
      initState((s: S) => (block:
        let (s0, a) = sg.runState s
        op(a).runState s0))
    bindImpl

template genNap[S, A](ins: State[S, A], B: typedesc): untyped =
  block:
    proc napImpl(op: A -> B): State[S, B] {.closure, gensym, effectsOf: op.} =
      ins.bindIt:
        ins.purify:
          op(it)
    napImpl

template genDrift[S, A](ins0: State[S, A], B, C: typedesc): untyped =
  block:
    proc driftImpl(ins1: State[S, B], op: (a: A, b: B) -> C): State[S,
        C] {.closure, gensym, effectsOf: op.} =
      proc driftInner(s: S): StateInner[S, C] =
        let
          (s0, a0) = ins0.runState(s)
          (s1, a1) = ins1.runState(s0)
        (s1, op(a0, a1))
      initState(driftInner)
    driftImpl

export State
export StateInner
export initState
export getState
export putState
export runState
export evalState
export execState

export wrap
export peel
export genNap
export genDrift
export genPure
export genBind
