## About
See the TURTLE of enormous girth!

## Dependencies
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
This will drop an executable named `ao-guile` in the top-level directory.
Use `cd ..` to go up a level then `./ao-guile` to run it.

You'll need to be in the root directory to run `ao-guile`, as it looks
for the `ao` folder to find various Scheme scripts.  Alternatively, you
can [add `ao` to your load path](https://www.gnu.org/software/guile/manual/html_node/Load-Paths.html)
in your `.guile` [init file](http://www.gnu.org/software/guile/manual/guile.html#Init-File).
