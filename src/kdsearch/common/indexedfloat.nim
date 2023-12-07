import std/[math]

import q6

type IndexedValue[T] = tuple[i: int, value: T]

IndexedValue.jNapImpl1Deriving [dcq6]:
  (ins.i, op(ins.value))

IndexedValue.fDriftImpl1:
  (insA.i, op(insA.value, insB.value))

proc getValue[T](p: IndexedValue[T]): T =
  p.value

type IndexedFloat* = IndexedValue[float]

proc `+`*(p, q: IndexedFloat): IndexedFloat =
  p.driftAB(q):
    a + b

proc `-`*(p, q: IndexedFloat): IndexedFloat =
  p.driftAB(q):
    a - b

proc `*`*(p, q: IndexedFloat): IndexedFloat =
  p.driftAB(q):
    a * b

proc `<`*(p, q: IndexedFloat): bool =
  getValue block:
    p.driftAB(q):
      a < b

proc `==`*(p, q: IndexedFloat): bool =
  getValue block:
    p.driftAB(q):
      a.almostEqual b

proc `<=`*(p, q: IndexedFloat): bool =
  p == q or p < q

proc sqrt*(p: IndexedFloat): IndexedFloat =
  p.napIt:
    it.sqrt

proc th*(i: int, v: float): IndexedFloat =
  (i, v)
