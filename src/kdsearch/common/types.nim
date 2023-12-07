import std/[
  options,
  tables,
]
import ../q6ins/[
  coord
]

type
  NodeKind* = enum
    X, Y
  NodeIndex* = distinct int
  MoveKind* = enum
    Lower, Higher
  HasHighLow[T] = tuple[l, h: T]
  Bound*[T] = HasHighLow[Coord[T]]
  KDNode*[T] = tuple[kind: NodeKind, value: Coord[T], boundary: Bound[T]]

  DirectedIndex* = tuple[x: NodeIndex, s: MoveKind]

  ConstructionProps*[T] = tuple[
    pivot: Coord[T],
    path: seq[MoveKind],
    bound: Bound[T],
    kind: NodeKind]
  SeivingProgress*[T] = tuple[
    path: seq[MoveKind],
    remains: seq[Coord[T]],
    bound: Bound[T],
    kind: NodeKind]

  PathNode*[T] = tuple[path: seq[MoveKind], node: KDNode[T]]
  MovesPathNodes*[T] = tuple[
    moves: seq[MoveKind],
    pathNodes: seq[PathNode[T]]]

  GetChild* = proc(arg: DirectedIndex): Option[NodeIndex] {.closure.}
  Extract*[T] = proc(i: NodeIndex): tuple[
      v: Coord[T], k: NodeKind, value: T] {.closure.}
  BottomCacheArea*[T] = tuple[
    bottom: NodeIndex,
    cache: Table[NodeIndex, Option[T]],
    area: Bound[T]]
  SearchHelpers*[T] = tuple[
    getChild: GetChild,
    i2d: proc(i: NodeIndex): T,
    extract: Extract[T]]

  Arithable* = concept x, type T
    cmp(x, x) is int
    x + x is T
    x - x is T
    x * x is T
