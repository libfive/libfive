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
Adjust based on your Qt installation path, and use `ninja` for faster builds.

# License
(c) 2015-2017 Matthew Keeter; all rights reserved.

This project will eventually be released under an open-source license,
something like
- LGPL for `ao-kernel`,
- GPL for `guile-ao-kernel` and `Studio`
and an option for commercial licensing.

Stay tuned for further details.

# Using the standalone Guile module
```
scheme@(guile-user)> (load-extension "ao/bind/libguile-ao-kernel" "scm_init_ao_modules")
scheme@(guile-user)> (use-modules (ao csg) (ao transforms) (ao shapes))
```

Note that the standalong module does not include
the minimal OpenGL-based GUI described
[here](https://mattkeeter.com/projects/ao).
That GUI is no longer maintained,
but can be found in the [`ao-guile-repl` branch](https://github.com/mkeeter/ao/tree/ao-guile-repl).
