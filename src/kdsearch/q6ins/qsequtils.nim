import ../q6

seq.fPureImpl1:
  @[a]

seq.dBindImpl1Deriving [dcQ6, dcFly]:
  for a in ins:
    for b in op(a):
      result.add b

seq.jNapImpl1:
  for a in ins:
    result.add op(a)

export peel
export wrap
export genNap
export genDrift
export genPure
export genBind
