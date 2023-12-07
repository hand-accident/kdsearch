import q6
import std/[options]
import unittest

Option.fPureImpl1:
  a.option

check int.none is Pointed

Option.dBindImpl1Deriving anyConcept:
  if ins.isSome:
    result = op(ins.unsafeGet)

check int.none is Bindable
check int.none is Donut

template filterIt[T](o: Option[T], code: untyped): untyped =
  o.bindIt:
    if code:
      o
    else:
      T.none

let oStr = "q6".some

check oStr.filterIt(it.len > 1) == oStr.filter(
  proc(it: string): bool = it.len > 1)

let oStr1 = "izquierda".some

proc `[]`(s: Option[string], i: int): Option[char] =
  s.bindIt:
    if i in 0..<it.len:
      it[i].some
    else:
      char.none

check oStr[0] == oStr1[2]

check oStr[2] == char.none
