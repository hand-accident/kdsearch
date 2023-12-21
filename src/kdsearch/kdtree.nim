import std/[
  algorithm,
  math,
  options,
  sequtils,
  strformat,
  sugar,
  tables,
]
import q6ins/[
  stateutil,
  coord
]
import q6
import dfs/dfsloop
import common/types

const ROOTINDEX* = 1.NodeIndex

const PRECISION = 24

func spread(i: SomeInteger): int =
  (i shl PRECISION).int

const NORMALIZER = 1.spread

func spreadCeilDiv(a, b: SomeInteger): int =
  a.uint64.spread.ceilDiv(b.int)

func spreadFloorDiv(a, b: SomeInteger): int =
  a.uint64.spread.floorDiv(b.int)

proc `==`*(x, y: NodeIndex): bool {.borrow.}
proc `$`*(x: NodeIndex): string = fmt":{x.int:X}:"

proc other(kind: NodeKind): NodeKind =
  case kind
  of X: Y
  of Y: X

proc nextMove(ms: seq[MoveKind], m: MoveKind): seq[MoveKind] =
  result = ms
  result &= m

proc recentMove(ms: seq[MoveKind]): Option[MoveKind] =
  if ms.len > 0:
    ms[^1].some
  else:
    MoveKind.none

proc getValue[T](c: Coord[T], kind: NodeKind): T =
  case kind
  of X:
    c.x
  of Y:
    c.y

proc catOptions[T](s: seq[Option[T]]): seq[T] =
  for o in s:
    if o.isSome:
      result &= o.unsafeGet

proc memoize[A, B](f: proc(a: A): B): proc(a: A): B =
  ## Returns a memoized version of the given procedure.
  ## from https://github.com/andreaferretti/memo/blob/master/memo.nim
  var cache = initTable[A, B]()

  result = proc(a: A): B =
    if cache.hasKey(a):
      result = cache[a]
    else:
      result = f(a)
      cache[a] = result

proc genNextIndex(p: int): proc(arg: DirectedIndex): NodeIndex =
  return memoize do (arg: DirectedIndex) -> NodeIndex:
    let (x, s) = arg
    case s:
    of Lower:
      ((x.int + 1).spreadCeilDiv(p) - 1).NodeIndex
    else:
      x.int.spreadFloorDiv(NORMALIZER - p).NodeIndex

proc genEncoding(
      nextIndex: proc(arg: DirectedIndex): NodeIndex
    ): proc(ss: seq[MoveKind]): NodeIndex =
  return proc(ss: seq[MoveKind]): NodeIndex =
    ss.foldl (a, b).nextIndex, ROOTINDEX

proc genParams(ss: seq[MoveKind]): int =
  ss.countIt(it == Lower).spreadFloorDiv(ss.len)

proc medianOfMedians[T: Arithable](ss: seq[T]): T =
  proc select(s: seq[T]): T =
    if s.len == 1:
      s[0]
    elif s.len == 2:
      s[1]
    elif s.len mod 2 == 0:
      (s.sorted)[s.len div 2]
    else:
      (s.sorted)[s.len div 2 + 1]
  var temp = ss
  while temp.len > 5:
    temp = ss.distribute(5).mapIt(it.select)
  temp.select

proc sieve[T: Arithable](cs: seq[Coord[T]], kind: NodeKind): tuple[
    l, h: seq[Coord[T]],
    pivot: Coord[T]] =
  let
    pivotValue = cs.mapIt(it.getValue(kind)).medianOfMedians
    pivot = cs.filterIt(it.getValue(kind) == pivotValue)[0]
    pivotIndex = cs.find pivot

  result.pivot = pivot
  for i, c in cs:
    if likely(i != pivotIndex):
      if c.getValue(kind) > pivotValue:
        result.h &= c
      else:
        result.l &= c

proc tighten[T](bound: Bound[T], value: Coord[T], kind: NodeKind,
    move: MoveKind): Bound[T] =
  case kind
    of X:
      case move
      of Lower:
        (bound.l, (value.x, bound.h.y))
      else:
        ((value.x, bound.l.y), bound.h)
    of Y:
      case move
      of Lower:
        (bound.l, (bound.h.x, value.y))
      else:
        ((bound.l.x, value.y), bound.h)

proc nextSeivingProgress[T](
    p: SeivingProgress[T], m: MoveKind, pivot: Coord[T]): SeivingProgress[T] =
  let (path, remains, bound, kind) = p
  (path.nextMove(m), remains, bound.tighten(pivot, kind, m), kind.other)

proc toPathNode[T](props: ConstructionProps[T]): PathNode[T] =
  let
    (pivot, path, bound, kind) = props
    lastMove = path.recentMove
    optionalBound = lastMove.map(it => bound.tighten(pivot, kind, it))
    newBound: Bound[T] = optionalBound.get(bound)
  (path, (kind, pivot, newBound))

proc createPathNodes[T: Arithable](
    cs: seq[Coord[T]], initialBound: Bound[T]): MovesPathNodes[T] =
  type
    TheState = State[seq[SeivingProgress[T]], ConstructionProps[T]]
    TheSI = StateInner[seq[SeivingProgress[T]], ConstructionProps[T]]
  let
    initialState: SeivingProgress[T] = (@[], cs, initialBound, X)
    step: TheState = prepareDfsState do (h: SeivingProgress[T]) -> TheSI:
      let
        (path, remains, bound, kind) = h
        (lowers, highers, pivot) = remains.sieve kind
      if highers.len > 0:
        result.state &= (path, highers, bound, kind).nextSeivingProgress(Higher, pivot)
      if lowers.len > 0:
        result.state &= (path, lowers, bound, kind).nextSeivingProgress(Lower, pivot)
      result.value = (pivot, path, bound, kind)

  step.dfsLoopWith(initialState, MovesPathNodes[T]):
    for m in stepResult.path:
      loopResult.moves &= m
    loopResult.pathNodes &= stepResult.toPathNode

proc toKD*[T: Arithable](
      cs: seq[Coord[T]], initialBound: Bound[T]
    ): tuple[tree: Table[NodeIndex, KDNode[T]], getChild: GetChild] =
  let
    (moves, pathNodes) = cs.createPathNodes initialBound
    param = moves.genParams
    nextIndex = genNextIndex(param)
    encoding = genEncoding(nextIndex)

  var ks: seq[NodeIndex]
  for (path, node) in pathNodes:
    let i = path.encoding
    result.tree[i] = node
    ks &= i

  result.getChild = memoize do (arg: DirectedIndex) -> Option[NodeIndex]:
    arg.nextIndex.option.filter code => code in ks

proc square[T: Arithable](a: T): T =
  a * a

proc addXY[T: Arithable](c: Coord[T]): T =
  c.x + c.y

proc distance[T: Arithable](p, q: Coord[T]): T =
  let c = addXY(block:
    p.driftAB(q):
      (a - b).square)

  when T is SomeNumber:
    c.float.sqrt
  else:
    c.sqrt

proc neighborhoodArea[T: Arithable](d: T, c: Coord[T]): Bound[T] =
  result.l = c.napIt:
    it - d
  result.h = c.napIt:
    it + d

proc findBottomArea[T: Arithable](
    fs: SearchHelpers[T], c: Coord[T]): BottomCacheArea[T] =
  type
    IndexedDistance = tuple[distance: T, i: NodeIndex]
    TheState = State[seq[NodeIndex], IndexedDistance]
    TheSI = StateInner[seq[NodeIndex], IndexedDistance]
  let
    (getChild, i2d, extract) = fs
    step: TheState = prepareDfsState do (i: NodeIndex) -> TheSI:
      let
        (_, kind, value) = i.extract
        d = i.i2d
        direction = block:
          if value < c.getValue(kind):
            Higher
          else:
            Lower
        child = getChild((i, direction))

      (@[child].catOptions, (d, i))

  var tempDistance = ROOTINDEX.i2d
  result = step.dfsLoopWith(ROOTINDEX, BottomCacheArea[T]):
    if stepResult.distance <= tempDistance:
      tempDistance = stepResult.distance
      loopResult.cache[stepResult.i] = tempDistance.option
      loopResult.bottom = stepResult.i
    else:
      loopResult.cache[stepResult.i] = T.none

  result.area = result.bottom.i2d.neighborhoodArea c

proc findNearestHelper[T: Arithable](
    fs: SearchHelpers[T], bca: BottomCacheArea[T]): Coord[T] =
  type
    CoordDistance = tuple[value: Coord[T], distance: T]
    TheState = State[seq[NodeIndex], CoordDistance]
    TheSI = StateInner[seq[NodeIndex], CoordDistance]
  let
    (getChild, i2d, extract) = fs
    (bottom, cache, area) = bca
    bottomV = bottom.extract.v
    step: TheState = prepareDfsState do (i: NodeIndex) -> TheSI:
      let
        (coord, kind, value) = i.extract
        d = i.i2d
      var children: seq[Option[NodeIndex]]
      if area.h.getValue(kind) > value:
        children &= getChild((i, Higher))
      if area.l.getValue(kind) <= value:
        children &= getChild((i, Lower))

      result.state = children.catOptions
      result.value = (coord, cache.getOrDefault(i, d.option).get(d))

  var tempDistance = ROOTINDEX.i2d
  result = bottomV
  result = step.dfsLoopWith(ROOTINDEX, Coord[T]):
    if stepResult.distance < tempDistance:
      loopResult = stepResult.value
      tempDistance = stepResult.distance

proc findNearest*[T: Arithable](
    n: tuple[tree: Table[NodeIndex, KDNode[T]], getChild: GetChild],
    c: Coord[T]): Coord[T] =
  let
    (tree, getChild) = n
    i2d = memoize do (i: NodeIndex) -> T:
      tree[i].value.distance(c)
    extract = memoize do (i: NodeIndex) -> tuple[
        v: Coord[T], k: NodeKind, value: T]:
      let
        node = tree[i]
        (v, k) = (node.value, node.kind)
        value = v.getValue k
      (v, k, value)

  let fs = (getChild, i2d, extract)

  fs.findNearestHelper fs.findBottomArea c
