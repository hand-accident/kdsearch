import ../q6ins/[
  stateutil,
]

import std/[
  options,
]

proc safeHead*[T](s: seq[T]): Option[T] =
  if s.len != 0:
    return s[0].some

proc getTail*[T](s: seq[T]): seq[T] =
  if s.len >= 2:
    return s[1..^1]

proc safeHeadTail*[T](s: seq[T]): tuple[head: Option[T], tail: seq[T]] =
  (s.safeHead, s.getTail)

template dfsLoopWith*[S, T](
    step: State[seq[S], T],
    initial: S,
    RetType: typedesc,
    body: untyped): untyped =
  block:
    var
      loopResult {.inject.}: RetType
      stack: seq[S]
      current: Option[S]

    current = initial.option
    while current.isSome:
      let
        sv: tuple[state: seq[S], value: T] = step.runState @[current.unsafeGet]
        descendants = sv.state
        stepResult {.inject.} = sv.value
      if descendants.len > 0:
        stack = descendants & stack
      body
      (current, stack) = stack.safeHeadTail
    loopResult

template dfsLoop*[S, T](
    step: State[seq[S], T],
    initial: S,
    body: untyped): untyped =
  step.dfsLoopWith(initial, seq[T]):
    body

proc prepareDfsState*[S, T](
    calcDfsStep: proc(h: S): StateInner[seq[S], T]): State[seq[S], T] =
  initState do (s: seq[S]) -> StateInner[seq[S], T]:
    calcDfsStep(s[0])
