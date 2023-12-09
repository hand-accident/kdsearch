# import unittest

import kdsearch/kdtree
import kdsearch/common/[types]
import kdsearch/q6ins/[coord, qsequtils, optionutil]
import kdsearch/q6

import std/[
  random,
  options,
  tables,
  sequtils
]

proc xy[T](x, y: T): Coord[T] =
  (x, y)

proc iota(r: Slice[int]): seq[int] =
  ## same with toSeq but limit type
  for i in r:
    result &= i

let
  s = iota(0..<10)
  cs = s.napIt:
    rand(0..25).xy rand(0..25)

echo (cs: cs)

let (tree, getChild) = cs.toKD((-1.xy(-1), 26.xy(26)))

# echo (tree: tree)

let tkeysMax = tree.keys.toSeq.mapIt(it.int).max

let keymap = iota(1..tkeysMax).napIt:
  let
    i = it.NodeIndex
    v = (i.some).bindTo(j):
      if j in tree:
        tree[j].value.some
      else:
        Coord[int].none
  (i: i, v: v, h: (i, Higher).getChild, l: (i, Lower).getChild)

for ihl in keymap:
  echo ihl
