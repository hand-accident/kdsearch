import q6
type Coord*[T] = tuple[x, y: T]

Coord.fPureImpl1:
  (a, a)

Coord.dBindImpl1Deriving [dcQ6]:
  (op(ins.x).x, (op(ins.y).y))

Coord.jNapImpl1:
  (op(ins.x), op(ins.y))

Coord.fDriftImpl1:
  (op(insA.x, insB.x), op(insA.y, insB.y))

export wrap
export peel
export genNap
export genDrift
export genPure
export genBind
