# The `libfive` standard library
`libfive` includes a standard library of shapes, CSG operations,
transforms, and so on.

To avoid needing to rewrite this library in every programming language,
it uses the _lingua franca_ of software: **C**.

The headers in this folder are meant to be parsed by a [helper script](parse.py)
and used to generate bindings using the C FFI of the chosen language.

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
