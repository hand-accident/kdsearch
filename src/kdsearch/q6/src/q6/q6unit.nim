## .. note:: TODO: Documentation [issue #48]

type Unit* = distinct bool
  ## unit type. only for [const unit]

func initUnit: Unit = return
  ## internal use for generate [unit]

const unit*: Unit = initUnit()
  ## [Unit] singleton.

proc `$`*(u: Unit): string =
  "()"

proc `==`*(u0, u1: Unit): bool = true

## .. importdoc:: context.nim, concepts.nim, donut.nim, fly.nim, fricative.nim, helpers.nim, junctor.nim
## .. include:: links.rst
