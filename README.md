# libfive
*Infrastructure for solid modeling.*

(formerly known as [mkeeter/ao](https://github.com/mkeeter/ao))

## About
- [Project homepage](https://libfive.com) (with demo videos!)
- [API Examples](https://libfive.com/examples)
- [Downloads](https://libfive.com/download)

`libfive` is a framework for solid modeling using
[functional representations](https://en.wikipedia.org/wiki/Function_representation).

It includes several layers, ranging from infrastructure to GUI:

- The `libfive` shared library contains functions to build, manipulate, and render f-reps.
A great deal of work has gone into the meshing algorithm,
which produces watertight, manifold,
hierarchical, feature-preserving triangle meshes.
The library is written in C++ and exposes a C API in `libfive.h`.
- `libfive-guile` is a [Guile](https://www.gnu.org/software/guile/)
binding to `libfive`.
It exposes a high-level API to construct shapes,
and includes a standard library
of shapes, transforms, and CSG operations.
- **Studio** is a GUI application in the style of
[OpenSCAD](http://www.openscad.org/).
It wraps `libfive-guile` and allows for live-coding of solid models.
The interface also includes direct modeling,
where the user can push and pull on the model's surface
to change variables in the script.

## Other projects using `libfive`
### Language bindings
- [Tovero](https://common-lisp.net/project/tovero/): A 3D modeling system for Common Lisp
- [`libfivepy`](https://gitlab.com/rcmz0/libfivepy): A Python CAD library (work in progress)
- [Bindings for Unity](https://github.com/zalo/libfive-unity)
- Unpublished [Stanza](http://lbstanza.org/) bindings (email for details)

### Viewers
- [Inspekt3D](https://gitlab.com/kavalogic-inc/inspekt3d): Lightweight pure-Guile viewer
- [PyFive3D](https://gitlab.com/kavalogic-inc/pyfive3d): Lightweight pure-Python viewer (work in progress)
- [C5H12 (Pentane)](https://gitlab.com/kavalogic-inc/C5H12): Lightweight C viewer

## License
(c) 2015-2018 Matthew Keeter

Different layers of this project are released under different licenses:
- The `libfive` dynamic library is released under the
  [Mozilla Public License, version 2](https://www.mozilla.org/en-US/MPL/2.0/).
- `libfive-guile` and `Studio` are released under the
  [GNU General Public License, version 2](https://www.gnu.org/licenses/old-licenses/gpl-2.0-standalone.html)
  [or later](https://www.gnu.org/licenses/gpl-3.0-standalone.html).

[Contact the author](matt.j.keeter@gmail.com)
to discuss custom development, integration,
or commercial support.

## Compiling from source
The full system (`libfive` + `libfive-guile` + **Studio**)
has been successfully compiled on Mac and Linux.

The `libfive` kernel builds on MSVC,
and should also build with MinGW (though this is untested).

### Dependencies
- [`cmake`](https://cmake.org/)
- [`pkg-config`](https://www.freedesktop.org/wiki/Software/pkg-config/)
- [Eigen 3.3.x](http://eigen.tuxfamily.org/index.php?title=Main_Page)
- [`libpng`](http://www.libpng.org/pub/png/libpng.html)
- [Boost 1.65 or later](https://www.boost.org)
- [Qt 5.7 or later](https://www.qt.io)
- [Guile 2.2.1 or later](https://www.gnu.org/software/guile/)

Qt and Guile are optional; if they aren't present, then
the Guile bindings and Studio will not be included in the build
(and `cmake` will print a message to that effect).

### Mac
With `homebrew` installed, run
```
brew install cmake pkg-config eigen libpng qt guile boost
```

Then, from the `libfive` folder, run something like:
```
mkdir build
cd build
cmake -DCMAKE_PREFIX_PATH=/usr/local/Cellar/qt5/5.7.0  ..
make
```
(adjust based on your Qt installation,
and consider using [`ninja`](https://ninja-build.org/) for faster builds.


### Ubuntu

Ubuntu __18.04 or later__ should have all dependencies available through the package manager
```
sudo apt-get install cmake pkg-config libeigen3-dev libpng-dev libboost-all-dev qtbase5-dev guile-2.2-dev 
```

Ubuntu releases __before 18.04__ do not provide `guile-2.2-dev`, so omit that from the above package install command.
To build guile 2.2.3 from source, run
```
sudo apt-get install libgmp-dev libltdl-dev libunistring-dev libgc-dev libffi-dev pkg-config
wget https://ftp.gnu.org/gnu/guile/guile-2.2.3.tar.gz 
tar -xf guile-2.2.3.tar.gz
cd guile-2.2.3
./configure
make -j4
sudo make install
```

Ubuntu releases __before 17.04__ do not have high enough Qt versions, so also omit `qtbase5-dev` from the above package install command.
To install Qt 5.7 or later, use the Qt provided Online Installer: https://www.qt.io/download
The installer will prompt for the install path, which defaults to `$HOME/Qt`.

Building is similar as on Mac: clone the repository, then run something like
```
mkdir build
cd build
cmake -DCMAKE_PREFIX_PATH=<Qt Install Path>/5.7/gcc_64 ..
make -j4
```
(adjusting the Qt path to your install location)

### Windows (MSVC)
With Visual Studio 2017 installed, run from libfive folder
```
git clone https://github.com/Microsoft/vcpkg.git
vcpkg\bootstrap-vcpkg.bat
vcpkg\vcpkg install eigen3:x86-windows-static boost:x86-windows-static libpng:x86-windows-static
md build
cd build
cmake -DCMAKE_TOOLCHAIN_FILE="..\vcpkg\scripts\buildsystems\vcpkg.cmake" -DVCPKG_TARGET_TRIPLET="x86-windows-static" -G "Visual Studio 15 2017" ..
```
Now open `build\libfiv.sln` and build the solution. Check that `libfive-test` runs correctly.
