## general abstract functions to simplify implementation of q6
##
## .. note:: TODO: runnableExamples [issue #37]

import std/[sugar]

proc compose*[A, B, C](f: A -> B, g: B -> C): A -> C =
  ## `a => g(f(a))`.
  ##
  ## two one-argument proc composition.
  ##
  ## `f` is inner(executed first).
  ##
  ## .. note:: TODO: runnableExamples [issue #39]
  return (a => g(f(a)))

proc applyF*[A, B](f: A -> B, a: A): B =
  ## `f(a)`. Just a `proc` application.
  ##
  ## Why I wrote this is the syntax of executing
  ## result of "proc-returning proc/template"
  ## to concrete value is not beautiful.
  ##
  ## And all of `gen-` template returns `proc`.
  ##
  ## .. note:: TODO: runnableExamples [issue #41]
  f(a)

## .. importdoc:: context.nim, concepts.nim, donut.nim, fly.nim, fricative.nim, junctor.nim, q6unit.nim
## .. include:: links.rst
