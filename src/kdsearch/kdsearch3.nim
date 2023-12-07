import std/[
  algorithm,
  options,
  os,
  parsecsv,
  sequtils,
  strutils,
  tables,
]
from std/streams import newFileStream

import kdpartial
import common/[
  types,
  indexedfloat
]
import mycsv/mycsv
import q6ins/[
  coord
]
import q6

type Equalable = concept x
  x == x is bool

func addHeight(a: Coord[float], h: float): tuple[x, y, z: float] =
  (x: a.x, y: a.y, z: h)

func rowToCoord(r: CsvRow): Coord[float] =
  (x: r[0].parseFloat, y: r[1].parseFloat)

proc all(c: Coord[bool]): bool =
  c.x and c.y

proc `==`(c, d: Coord[Equalable]): bool =
  all block:
    c.driftAB(d):
      a == b

proc get(cif: Coord[IndexedFloat]): Coord[float] =
  cif.napIt:
    it.value

proc getIndex(cif: Coord[IndexedFloat]): int =
  cif.x.i

proc `$`*(c: Coord[IndexedFloat]): string =
  $(i: c.getIndex, x: c.x.value, y: c.y.value)

proc th(i: int, c: Coord[float]): Coord[IndexedFloat] =
  c.napIt:
    i.th it

const ZEROBOUND: Bound[IndexedFloat] = (0.th (NegInf, NegInf), 0.th (Inf, Inf))

proc compoundNearest(
      t: tuple[
        tree: Table[NodeIndex, KDNode[IndexedFloat]],
        getChild: GetChild],
      c: Coord[IndexedFloat],
      heights: seq[float]
    ): tuple[x, y, z: float] =
  c.get.addHeight(heights[t.findNearest(c).getIndex])

when isMainModule:
  var
    pts, txts: seq[Coord[IndexedFloat]]
    hs: seq[float]

  let rootPath = getHomeDir() / "desktop" / "ksgt" / "source" / "csv"

  indexedCachedReadLoop(rootPath / "text_object.txt"):
    txts &= i.th x.row.rowToCoord
    hs &= x.row[2].parseFloat

  indexedCachedReadLoop(rootPath / "object.txt"):
    pts &= i.th x.row.rowToCoord

  let t = txts.toKD ZEROBOUND

  pts.mapIt(t.compoundNearest(it, hs)).seqToCsv(rootPath / "result_test3.csv")
