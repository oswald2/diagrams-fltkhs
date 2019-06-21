# diagrams-fltkhs
A backend for diagrams, which draws to FLTKHS widgets. Uses the SVG backend as intermediate representation

To build it, simply call

> stack build

Currently, these are only examples (from the Diagrams library), which can be then started with:

> stack exec DragonFractal

> stack exec Kaleidoscope

Note that the Kaleidoscope does not look the same, as probably FLTK does not support all SVG capabilities

> stack exec TreeMap

> stack exec PythagoreanTree

