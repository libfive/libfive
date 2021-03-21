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
- The `libfive` standard library is a library of common shapes, transforms, and CSG operations.
  It is implemented in C++ and exposes a C API in `libfive/stdlib/stdlib.h`
- The standard library is parsed and used to generate bindings for both
  [Guile Scheme](https://www.gnu.org/software/guile/)
  and [Python](https://python.org),
  for use in the REPL or as part of larger applications.
- **Studio** is a GUI application in the style of
[OpenSCAD](http://www.openscad.org/).
It uses the Python and Guile bindings and allows for live-coding of solid models.
The interface also includes direct modeling,
where the user can push and pull on the model's surface
to change variables in the script.

## Other projects using `libfive`
### Language bindings
- [Tovero](https://gitlab.com/kavalogic-inc/tovero): A 3D modeling system for Common Lisp
- [`libfivepy`](https://gitlab.com/rcmz0/libfivepy): A Python CAD library (work in progress)
- [Bindings for Unity](https://github.com/zalo/libfive-unity)
- Unpublished [Stanza](http://lbstanza.org/) bindings (email for details)

### Viewers
- [Inspekt3D](https://gitlab.com/kavalogic-inc/inspekt3d): Lightweight pure-Guile viewer
- [PyFive3D](https://gitlab.com/kavalogic-inc/pyfive3d): Lightweight pure-Python viewer (work in progress)
- [C5H12 (Pentane)](https://gitlab.com/kavalogic-inc/C5H12): Lightweight C viewer

### Research
- [Massively Parallel Rendering of Complex Closed-Form Implicit Surfaces](https://www.mattkeeter.com/research/mpr/):
  a technical paper extending `libfive` to render on the GPU
  ([reference implementation](https://github.com/mkeeter/mpr/))

## License
(c) 2015-2021 Matthew Keeter

Different layers of this project are released under different licenses:
- The `libfive` library, `libfive-stdlib` library, and Python bindings
  are released under the
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

#### libfive
- [`cmake 1.65 or later`](https://cmake.org/)
- [`pkg-config`](https://www.freedesktop.org/wiki/Software/pkg-config/)
- [Eigen 3.3.x](http://eigen.tuxfamily.org/index.php?title=Main_Page)
- [`libpng`](http://www.libpng.org/pub/png/libpng.html)
- [Boost 1.65 or later](https://www.boost.org)

#### Guile bindings (optional)
- [Guile 2.2.1 or later](https://www.gnu.org/software/guile/)

If Guile isn't present, the Guile bindings won't be built.

#### Python bindings (optional)
- [Python 3.7 or later](https://www.python.org/)

If Python isn't present, the Python bindings won't be built.

#### Studio (optional, requires Guile bindings)
- [Qt 5.12 or later](https://www.qt.io)

If Qt and Guile aren't present, Studio will not be included in the build
(and `cmake` will print a message to that effect).

### Mac
With `homebrew` installed, run
```
brew install cmake pkg-config eigen libpng boost guile python3 qt
```
Omit `guile`, `python3`, or `qt` to avoid building bindings and/or the UI.

Then, from the `libfive` folder, run something like:
```
mkdir build
cd build
cmake -DCMAKE_PREFIX_PATH=/usr/local/Cellar/qt5/5.12.0  ..
make
```
(adjust based on your Qt installation,
and consider using [`ninja`](https://ninja-build.org/) for faster builds.

### Ubuntu
As a rule of thumb, `libfive` targets packages available in the latest Ubuntu LTS,
(currently 20.04 LTS).  This is not automatically tested;
if you find it's not the case, please open an issue.

#### 20.04 or later
Ubuntu __20.04 or later__ should have all dependencies available
through the package manager
```
sudo apt-get install cmake pkg-config libeigen3-dev libpng-dev libboost-all-dev guile-2.2-dev qtbase5-dev python3
```
Omit `guile-2.2-dev` and/or `qtbase5-dev` if you do not want Guile bindings and/or Studio to be built too.

Building is similar as on Mac: clone the repository, then run something like
```
mkdir build
cd build
cmake -DCMAKE_PREFIX_PATH=<QT INSTALL PATH>/5.7/gcc_64 ..
make -j4
```
(adjusting the Qt path to your install location)

Running `sudo make install` will install `libfive.so` (the core shared library)
and the `libfive` headers.
If Guile bindings are enabled,
they will be pre-compiled and installed to Guile's `(%site-ccache-dir)` directory.
If Studio was also built, the `Studio` executable will installed as well.
[`Studio.desktop`](https://github.com/libfive/libfive/blob/master/studio/deploy/linux/Studio.desktop)
may be used to put the program on your desktop (untested as of yet).

#### Before 20.04
On Ubuntu 18.04, you may need to update to a newer version of CMake,
which is possible using the Snappy package manager:
```
sudo snap install cmake --classic
```
(untested)

CMake downloads are also available on [their website](https://cmake.org/download/)

18.04 _also_ does not have a new enough Qt in the package manager,
so omit `qtbase5-dev` from the above package install command.
To install Qt 5.12 or later,
use the [Qt Online Installer](https://www.qt.io/download-qt-installer).
The installer will prompt for the install path, which defaults to `$HOME/Qt`.

#### Before 18.04
Ubuntu releases __before 18.04__ do not provide `guile-2.2-dev`,
so omit that from the above package install command.
To build Guile 2.2.3 from source, run
```
sudo apt-get install libgmp-dev libltdl-dev libunistring-dev libgc-dev libffi-dev pkg-config
wget https://ftp.gnu.org/gnu/guile/guile-2.2.3.tar.gz
tar -xf guile-2.2.3.tar.gz
cd guile-2.2.3
./configure
make -j4
sudo make install
```

Then, build as above.

### Windows (VS2019)
Install [Git](https://git-scm.com/download/win),
choosing settings so that it can be invoked from a Windows _Command Prompt_
(the defaults should be fine).

Install [VS2019](https://visualstudio.microsoft.com/vs/) (Community Edition),
configured for "Desktop development with C++".
You only _need_ MSVC, Windows 10 SDK, and C++ CMake tools for Windows,
so feel free to uncheck other optional packages in the right sidebar,
then run the installation!

Next, install dependencies using `vcpkg`.

(This step touches many files, so you may want to disable the
_Antimalware Service Executable_,
which will otherwise scan every single file and slow things down dramatically:
in "Windows Security → Virus & threat protection settings",
uncheck "Real-time protection".)

In a Windows _Command Prompt_:
```
git.exe clone https://github.com/libfive/libfive
cd libfive
git clone https://github.com/Microsoft/vcpkg.git
.\vcpkg\bootstrap-vcpkg.bat
.\vcpkg\vcpkg.exe install --triplet x64-windows eigen3 boost-container boost-bimap boost-interval boost-lockfree boost-functional boost-algorithm boost-math libpng qt5-base python3
```
Go get some coffee or something - this will take a while.

Once this is done installing,
you're ready to actually build `libfive` and Studio!
```
mkdir build
cd build
"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\Common7\IDE\CommonExtensions\Microsoft\CMake\CMake\bin\cmake.exe" -DCMAKE_TOOLCHAIN_FILE="..\vcpkg\scripts\buildsystems\vcpkg.cmake" -DVCPKG_TARGET_TRIPLET="x64-windows" -G"Visual Studio 16 2019" ..
"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\Common7\IDE\CommonExtensions\Microsoft\CMake\CMake\bin\cmake.exe" --build . --config Release --target Studio -- -v:n -m:8
.\studio\Release\Studio.exe
```

At this point, you can also double-click on `Studio.exe` to launch it,
and create a shortcut to put it on your desktop.

(don't move it out of the `build` directory,
or the precarious house of cards that finds Python will come tumbling down)

When changes are made, you _should_ only need to re-run the build step, i.e.
```
"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\Common7\IDE\CommonExtensions\Microsoft\CMake\CMake\bin\cmake.exe" --build . --config Release --target Studio -- -v:n -m:8
```
