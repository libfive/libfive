# ggspec - lightweight unit testing library for Guile

Copyright (c) 2014 Yawar Amin

GitHub, Reddit, Twitter: yawaramin

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Introduction

ggspec is a _very_ lightweight unit testing framework for Guile.
Currently I am targeting Guile 1.8. I may port it to other Schemes in
future; in principle it should be fairly simple because it doesn't use
very many Guile-specific features.

ggspec is self-testing. In fact, it was bootstrapped by the tests--the
tests were written _first,_ before there was a unit testing framework to
run them; and _then_ the framework was written around the tests.

## Installation

Assuming you have cloned the `ggspec` repository to a directory also
named `ggspec`, create a symbolic link to the `ggspec` directory inside
any directory in your `$GUILE_LOAD_PATH` (Windows: `%GUILE_LOAD_PATH%`).
Then, link or copy the `ggspec` script to any directory in your `$PATH`
(Windows: `%PATH%`).

E.g., if you have `~/guile` in your Guile load path, `~/bin` in your
path, and have cloned the repo into `~/code/ggspec`, then run:

```
cd ~/guile
ln -s ~/code/ggspec
cd ~/bin
ln -s ~/code/ggspec/ggspec
```

## Verifying the framework itself

As I described earlier, the `ggspec` framework is tested using itself.
You can run the tests:

```
cd ~/code/ggspec
./ggspec # Or if you copied/linked the script to a dir in your path,
         # just ggspec.
```

## Contributing

See the file `CONTRIBUTING.md`.

## Minimal complete example

```
$ guile
guile> (use-modules (ggspec lib))
guile> (suite "Hello ggspec" end)
  Suite: Hello ggspec

(0 0 0)
```

The last line above shows the number of passing tests, failing tests,
and skipped tests. Since we didn't write _any_ tests here, all three are
zero.

## Introduction

See the `lib.scm` file for a detailed reference. A brief introduction to
how it works:

  - The `suite` function is presented as the way to organise everything.
    This includes tests, any options you want to pass in to those tests,
    any setup variables you want to define before running each test in
    the suite, and any teardowns you want to do after running each test.

  - Inside the `suite` function, a group of tests is organised together
    using the `tests` function. Each test is created using the `test`
    macro.

  - After the tests come the options, which are organised together using
    the `options` function. Each option is created using the `option`
    function.

  - After the options come the setups, which are organised together
    using the `setups` function. Each setup (which is a symbol name
    'bound' to an expression) is created using the `setup` macro. Each
    setup will be re-evaluated before each test is run. So generally you
    don't want to put expensive computations in these.

  - After the setups come the teardowns, which are organised together
    using the `teardowns` function. Each teardown (which is just a set
    of expressions) is created using the `teardown` macro. Each teardown
    will be re-evaluated _after_ each test is run to ensure a clean
    'environment' for the next test run.

Options and setups are not required, but since setups come after
options, you have to specify options if you want to specify setups. You
can specify empty (or missing) options, setups, and teardowns, in which
case the `suite` function doesn't try to do anything with them.

Normally you will put all your test source code files in a subdirectory
of your project directory called `spec`. This lets the `ggspec` test
runner find and run all of them.

## Tutorial

A short tutorial (in the context of Test-Driven Development):

Suppose we want to develop a function `sqr` that should return the
square of its argument (a number). In this example I will show the
function and its test suite together, but usually you would have them in
separate files. For this example you can follow along by making the
changes in your editor and re-loading the source code file in your REPL:

```
guile> (load "test.scm")
```

First you write a failing test:

```scheme
(use-modules (ggspec lib))

(suite "The sqr function"
  (tests
    (test "Should square 1 correctly"
      ;; The 'e' argument below represents (and contains) all the
      ;; state that we're passing in to the test. All setups, options,
      ;; etc.
      e
      (assert-equal 1 (sqr 1)))))
```

This suite will fail because the `sqr` function does not exist:

```
  Suite: The sqr function
ERROR: Unbound variable: sqr
```

Now implement it to make the test pass:

```scheme
(define (sqr x) 1)
```

The test will now pass because we have 'cheated' to make it so:

```
  Suite: The sqr function
    [PASS]
```

Note that the output is minimal when a test passes. Now we extend the
suite to another 'example':

```scheme
(use-modules (ggspec lib))

(suite "The sqr function"
  (tests
    (test "Should square 1 correctly"
      e
      (assert-equal 1 (sqr 1)))
    (test "Should square 2 correctly"
      e
      (assert-equal 4 (sqr 2)))))
```

This will now fail, again, because the `sqr` function is hard-coded to
always return `1`:

```
  Suite: The sqr function
    [PASS]
    [FAIL] Should square 2 correctly
      Expected: 4
           Got: 1
```

Now we fix that:

```scheme
(define (sqr x) (* x x))
```

This time the test will pass because `sqr` handles the general case:

```
  Suite: The sqr function
    [PASS]
    [PASS]
```

