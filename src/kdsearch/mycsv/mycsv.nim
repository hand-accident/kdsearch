from std/streams import newFileStream
import std/[
  os,
  parsecsv,
  strformat,
  strutils,
]

template withCsv*(filename: string, body: untyped): untyped =
  var s = filename.newFileStream fmRead
  if s == nil:
    quit("cannot open the file " & filename)
  block:
    var x {.inject.}: CsvParser
    defer:
      x.close
    x.open s, filename
    body

proc seqToCsv*(s: seq[tuple[x, y, z: float]], filename: string) =
  var f: File
  if f.open(filename, fmWrite):
    defer:
      f.close
    for item in s:
      let line = fmt"{item.x}, {item.y}, {item.z}"
      f.writeLine line

template indexedCachedReadLoop*(filename: string, body: untyped): untyped =
  filename.withCsv:
    var
      i {.inject.} = 0
      strCache: seq[string] = @[]
    while x.readRow:
      let r = x.row.join""
      if r notin strCache:
        body
        strCache &= r
        inc i
