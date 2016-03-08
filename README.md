## About
See the TURTLE of enormous girth!

## Dependencies

On a Mac with [Homebrew](http://brew.sh/), run
```
brew install cmake ninja boost libpng glfw3 libepoxy guile
```

### Compilation
- [cmake](https://cmake.org/)
- [Ninja](https://ninja-build.org/) (recommended)

### Kernel
- [Boost](http://www.boost.org/)
- [`libpng`](http://www.libpng.org/pub/png/libpng.html)

### UI
- [GLFW](http://www.glfw.org/)
- [`libepoxy`](https://github.com/anholt/libepoxy)

### Bindings
- [Guile](http://www.gnu.org/software/guile/)

## Building
```
git clone git@github.com:mkeeter/ao
cd ao
mkdir build
cd build
cmake -G Ninja ..
ninja
```
This will produce a library named `libao`.

To start a shell, run the executable named `ao-guile`
(in the `bin` directory).

## Usage
- `(ao-shapes)` lists available shape functions.
- `(ao-show ...)` shows a shape in the 3D viewport.
- `(ao-watch ...)` watches a script and re-runs it on changes.

## License
Copyright (C) 2016 Matthew Keeter (matt.j.keeter@gmail.com)

Ao is free software, with the core library licensed under the LGPL
and bindings licensed under the GPL.

The Ao shared library (named `libao` and built from the `kernel` and
`ui` folders plus `bind/api.cpp`) is licensed under the LGPL.

The Guile buildings and library of shapes (in the `bind/guile`
subfolder) are licensed under the GPL.

For details, see `COPYING.md` and the specific license files.
