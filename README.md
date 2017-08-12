# Building
## Dependencies
### Geometry Kernel
- [Eigen 3.x](http://eigen.tuxfamily.org/index.php?title=Main_Page)

### Bindings and GUI
- [Qt 5.7 or later](https://www.qt.io)
- [Guile 2.2 or later](https://www.gnu.org/software/guile/)

## Invocation
```
mkdir build
cd build
cmake -DCMAKE_PREFIX_PATH=/usr/local/Cellar/qt5/5.7.0 -GNinja ..
```
Adjust based on your Qt installation path.
