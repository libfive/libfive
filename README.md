# libfive
*Infrastructure for solid modeling.*  
[Homepage](https://libfive.com) | [API Examples](https://libfive.com/examples) | [Downloads](https://libfive.com/download)

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
- High level [Rust bindings](https://crates.io/crates/libfive)
- Unpublished [Stanza](http://lbstanza.org/) bindings (email for details)

### Viewers
- [Inspekt3D](https://gitlab.com/kavalogic-inc/inspekt3d): Lightweight pure-Guile viewer
- [PyFive3D](https://gitlab.com/kavalogic-inc/pyfive3d): Lightweight pure-Python viewer (work in progress)
- [C5H12 (Pentane)](https://gitlab.com/kavalogic-inc/C5H12): Lightweight C viewer

### Research
- [Massively Parallel Rendering of Complex Closed-Form Implicit Surfaces](https://www.mattkeeter.com/research/mpr/):
  a technical paper extending `libfive` to render on the GPU
  ([reference implementation](https://github.com/mkeeter/mpr/))

## Community
For `libfive`-specific discussions, consider opening a topic in the
[Github Discussions tab](https://github.com/libfive/libfive/discussions).

There's also a `libfive` subforum in the [SDF User Group Discourse](https://sdfug.discourse.group/),
which is a good place for general discussion of modeling with signed distance
fields.

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
`libfive` and Studio are compatible with macOS, Linux, and Windows.

### Dependencies
#### libfive
- [`cmake 1.65 or later`](https://cmake.org/)
- [`pkg-config`](https://www.freedesktop.org/wiki/Software/pkg-config/)
- [Eigen 3.3.x](http://eigen.tuxfamily.org/index.php?title=Main_Page)
- [`libpng`](http://www.libpng.org/pub/png/libpng.html)
- [Boost 1.65 or later](https://www.boost.org)

#### Guile bindings (optional, macOS and Linux only)
- [Guile 2.2.1 or later](https://www.gnu.org/software/guile/)

#### Python bindings (optional)
- [Python 3.7 or later](https://www.python.org/)

#### Studio (optional, requires Guile or Python bindings)
- [Qt 5.12 or later](https://www.qt.io)

When `cmake` is first run,
it will check for all dependencies and print details of what
will be build, e.g.

```
Checking dependencies:
  libfive:              ✓
  Guile bindings:       ✓
  Python bindings:      ✓
  Studio:               ✓   (Python + Guile)
```

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
`libfive` should build out of the box on the latest Ubuntu LTS
(currently 20.04).  If you find that's not the case, please open an issue!

Start by installing dependencies through the package manager:
```
sudo apt-get install cmake pkg-config libeigen3-dev libpng-dev libboost-all-dev guile-3.0-dev qtbase5-dev python3
```
Omit `guile-3.0-dev` and/or `qtbase5-dev` if you do not want Guile bindings and/or Studio to be built too.

Building is similar as on Mac: clone the repository, then run something like
```
mkdir build
cd build
cmake ..
make -j4
```

Once building is complete, run Studio with `./studio/Studio`.

Running `sudo make install` will install components to system-wide destinations,
e.g. `/usr/local/bin/Studio` for the main executable.
This will let you invoke `Studio` from anywhere in the system,
rather than just the `build` directory.
If you are using this workflow,
`sudo make install` must be run after changes to the repository
to update the system-wide installation of the executable and libraries.
[`Studio.desktop`](https://github.com/libfive/libfive/blob/master/studio/deploy/linux/Studio.desktop)
may be used to put a shortcut on your desktop.

If you don't want the Python bindings installed under `/usr/lib`, you
can specify the install directory using the `cmake` variable
`PYTHON_SITE_PACKAGES_DIR`, e.g.
```
cmake -DPYTHON_SITE_PACKAGES_DIR=/usr/local/lib/python3.9/dist-packages .
```

Ubuntu releases before 20.04 are not officially supported;
if you insist,
there are hints [here](https://github.com/libfive/libfive/blob/b4e0e0bbf8c740a313754062a205a98ac336a19c/README.md#before-2004)
and a discussion of Python linking issues [here](https://github.com/libfive/libfive/issues/408).

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
