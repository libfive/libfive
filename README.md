# Disclaimer
This generation of Ao is not yet ready for mass consumption.

It's coming along nicely –
and if you've found your way here,
you should try it out! –
but there's very little documentation,
no stability guarantees,
and limited support.

Please enjoy it responsibly and refrain from posting it to the wider web.

# About
Ao is a framework for solid modeling using
[functional representations](https://en.wikipedia.org/wiki/Function_representation).

It includes several layers, ranging from infrastructure to GUI:

- `libao-kernel` is a shared library to build, manipulate, and render f-reps.
A great deal of work has gone into the meshing algorithm,
which produces watertight, manifold,
hierarchical, feature-preserving triangle meshes.
The library is written in C++ and exposes a C API in `ao/ao.h`.
- `libao-guile` is a [Guile](https://www.gnu.org/software/guile/)
binding to `libao-kernel`.
It exposes a high-level API to construct shapes,
and includes a standard library
of shapes, transforms, and CSG operations.
- *Studio* is a GUI application in the style of
[OpenSCAD](http://www.openscad.org/).
It wraps `libao-guile` and allows for live-coding of solid models.
The interface also includes direct modeling,
where the user can push and pull on the model's surface
to change variables in the script.

# Building
## Dependencies
### Geometry Kernel
- [Eigen 3.x](http://eigen.tuxfamily.org/index.php?title=Main_Page)
- [`libpng`](http://www.libpng.org/pub/png/libpng.html)

### Bindings and GUI
- [Qt 5.7 or later](https://www.qt.io)
- [Guile 2.2 or later](https://www.gnu.org/software/guile/)

## Invocation
```
mkdir build
cd build
cmake -DCMAKE_PREFIX_PATH=/usr/local/Cellar/qt5/5.7.0  ..
make
```
Adjust based on your Qt installation path, and consider using [`ninja`](https://ninja-build.org/) for faster builds.

# License
(c) 2015-2017 Matthew Keeter; all rights reserved.

This project will eventually be released under an open-source license,
something like
- LGPL for `ao-kernel`,
- GPL for `guile-ao` and `Studio`
and an option for commercial licensing.

Stay tuned for further details.

# Using the standalone Guile module
```
scheme@(guile-user)> (load-extension "ao/bind/libguile-ao" "scm_init_ao_modules")
scheme@(guile-user)> (use-modules (ao csg) (ao transforms) (ao shapes))
```

Note that the standalong module does not include
the minimal OpenGL-based GUI described
[here](https://mattkeeter.com/projects/ao).
That GUI is no longer maintained,
but can be found in the [`ao-guile-repl` branch](https://github.com/mkeeter/ao/tree/ao-guile-repl).
