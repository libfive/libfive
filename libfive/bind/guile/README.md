These are the [Guile Scheme](https://www.gnu.org/software/guile/)
bindings to `libfive`, including the core C API and standard library

These bindings are implemented as pure `.scm` files,
both hand-written (everything in `libfive` except `stdlib`)
and auto-generated (`libfive/stdlib/*`).

The auto-generated files are created by [`gen_scm.py`](../../stdlib/gen_scm.py),
and are committed to the repo for simplicity.
