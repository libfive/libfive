# The `libfive` standard library
## Overview
In order to do useful design, we need a standard library of shapes,
CSG operations, transforms, and so on.
In the past, each language-level binding had to implement this on their own.
Now, `libfive` itself includes such a standard library,
using the _lingua franca_ of software: **C**.

## `libfive_std.h` style
[`libfive_std.h`](libfive_std.h) is both a C header and a structured document
parsed by a [helper script](parse.py).

It is used to generate bindings using the C FFI of the chosen language.

As C does not include niceties like default arguments,
the function declarations in the header follow a particular style.

For example:
```
LIBFIVE_STDLIB circle(
    // A 2D circle with the given radius and optional center
    tfloat r, tvec2 center__0);
```

`LIBFIVE_STDLIB` indicates that this is a `libfive` standard library function,
for ease of parsing.  All standard library functions return a `libfive_tree`.

The comment before function arguments is a docstring.

The `tfloat` type is philosophically a `float`, but in practice,
is a `libfive_tree`; this is to allow functions to be parameterized by
free variables.

The suffix on the name of `tvec2 center__0` indicates that
this is an optional argument and should default to 0 when not used.

## C and C++ bindings
The standard library is implemented in [`stdlib_impl.cpp`](stdlib_impl.cpp),
using C++ types (e.g. `libfive::Tree`) to avoid tedious manual memory
management.

However, FFI callers will be using `C` types (e.g. `libfive_tree`).

[`gen_c.py`](gen_c.py) generates two glue files:
- [`stdlib_impl.hpp`](stdlib_impl.hpp) is a header using C++ types
- [`stdlib.cpp`](stdlib.cpp) is a source file which call from C into C++
