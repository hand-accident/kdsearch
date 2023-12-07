import unittest
import q6
import sugar

type Coord[T] = tuple[x, y: T]

proc xy[T](x, y: T): Coord[T] = (x, y)

Coord.fPureImpl1:
  (a, a)
Coord.dBindImpl1Deriving anyConcept:
  (op(ins.x).x, op(ins.y).y)

let c = 2.0.xy 3.0

test "concept":
  check c is Q6
  check c is Junctor
  check c is Fly
  check c is Pointed
  check c is Fricative
  check c is Bindable
  check c is Donut

test "context":
  check c.peel is float
  check c.wrap(string) is Coord[string]

test "junctor":
  check c.napIt(it * it) == 4.0.xy 9.0
  check c.napIt($it) == "2.0".xy "3.0"

let d: Coord[float] = (4.0, 5.5)
let e: Coord[int] = (1, 2)

test "fly":
  check c.driftAB(d, a + b) == 6.0.xy 8.5
  check d.driftAB(e, a.toInt * b) == 4.xy 12

test "pointed":
  check c.purify(unit) == unit.xy unit
  check c.purify("testString") == "testString".xy "testString"

test "bindable":
  check (block:
    c.bindIt:
      it.xy (it * 2)) == (2.0, 6.0)
  check c.xy(d).dJoin == (2.0, 5.5)
