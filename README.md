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
- `(ao-list-shapes)` lists available shape functions.
- `(ao-show ...)` shows a shape in the 3D viewport.
- `(ao-watch ...)` watches a script and re-runs it on changes.

## License
Copyright (C) 2016 Matthew Keeter (matt.j.keeter@gmail.com)

Ao is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.
