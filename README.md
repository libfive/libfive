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
cmake -DCMAKE_PREFIX_PATH=/usr/local/Cellar/qt5/5.7.0 -GNinja ..
ninja
```
Adjust based on your Qt installation path.

# Using the standalone Guile module
```
scheme@(guile-user)> (load-extension "ao/bind/libguile-ao-kernel" "scm_init_ao_modules")
scheme@(guile-user)> (use-modules (ao csg) (ao transforms) (ao shapes))
```
