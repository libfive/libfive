# The `libfive` standard library
**This is half-baked and not yet deployed**

`libfive` includes a standard library of shapes, CSG operations,
transforms, and so on.

To avoid needing to rewrite this library in every programming language,
it uses the _lingua franca_ of software: **C**.

The headers in this folder are meant to be parsed by a helper script
and used to generate bindings using the C FFI of the chosen language.
As C does not include niceties like default arguments,
the function declarations in the header follow a particular style.

For example:
```
LIBFIVE_STDLIB circle__0(float r, vec2 center__0)
```

`LIBFIVE_STDLIB` indicates that this is a `libfive` standard library function,
for ease of parsing.  All standard library functions return a `libfive_tree`.

`circle__0` indicates that this is the first version of the function,
for backwards compatibility as the API changes.  This is optional,
and defaults to 0 if not present.

The argument `vec2 center__0` indicates that
this is an optional argument and should default to 0 when not used.
