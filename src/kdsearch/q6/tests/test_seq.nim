import unittest
import q6

import std/[sequtils]

seq.fPureImpl1:
  result.add a
seq.dBindImpl1Deriving anyConcept:
  for a in ins:
    for b in op(a):
      result.add b

let c: seq[float] = @[2.0, 3.0]

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
  check c.wrap(string) is seq[string]

test "junctor":
  check c.napIt(it * it) == @[4.0, 9.0]
  check c.napIt($it) == @["2.0", "3.0"]

let d: seq[float] = @[4.0, 5.5]
let e: seq[int] = @[1, 2]

test "fly":
  check c.driftAB(d, a + b) == @[6.0, 7.5, 7.0, 8.5]
  check d.driftAB(e, a.toInt * b) == @[4, 8, 6, 12]

const testString = "testString"

test "pointed":
  check c.purify(unit) == @[unit]
  check c.purify(9'u) == @[9'u]
  check c.purify(testString) == @[testString]

test "bindable":
  check (block:
    c.bindIt:
      if it > 2.5:
        @[it * 2, it * 3]
      else:
        @[]) == @[6.0, 9.0]
  check @[c, d].dJoin == @[c, d].concat
