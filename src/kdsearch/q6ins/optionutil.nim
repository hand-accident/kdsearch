import ../q6
import std/[options]

Option.fPureImpl1:
  a.option

Option.dBindImpl1Deriving anyConcept:
  if ins.isSome:
    result = op(ins.unsafeGet)

export peel
export wrap
export genNap
export genDrift
export genPure
export genBind
