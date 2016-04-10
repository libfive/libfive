#!
ggspec lib - lightweight unit testing library for Guile

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
!#
(define-module (ggspec lib)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-16)
  #:use-module (ice-9 match)
  #:export
    (
    assert-between
    assert-equal
    assert-gt
    assert-gte
    assert-lt
    assert-lte
    assert-near
    assert-not-equal
    assert-true
    assert-false
    assert-all
    end
    error?
    if-let
    suite
    suite-add-option
    suite-passed
    suite-failed
    suite-skipped
    options
    option
    println
    results-add
    run-file
    setups
    setup
    tests
    test
    teardowns
    teardown
    text-verbose
    output-none
    output-normal
    output-tap
    stub
    kwalist
    ))

(define kolour-red "\x1b[31m")
(define kolour-green "\x1b[32m")
(define kolour-normal "\x1b[0m")

(define-syntax if-let
  (syntax-rules ()
    ((_ name val then-exp else-exp)
      (let ((name val)) (if name then-exp else-exp)))
    ((_ name val then-exp)
      (if-let name val then-exp #f))))

#!
Stub out any variable in any module (including the current module) with
a new value (which could be a function), evaluate some expressions,
restore the old variable value, and return the result value from the
last of the evaluated expressions.

Arguments
  mod-name: '(name-component ...): the module name. Optional. If
  omitted, assumed to be the current module. E.g. '(ggspec lib).

    name-component: one of the components of the module name.

  var-name: symbol: the name of the variable to stub out. E.g. 'map.

  val: any (including function): the new value to give to the
  stubbed-out variable.

  expr ...: any number of expressions to evaluate in the context of the
  new value.

Returns
  The return value from the last of the evaluated 'expr's.
!#
(define-syntax stub
  (syntax-rules ()
    ((_ mod-name var-name val expr ...)
      (let*
        ((mod (resolve-module mod-name #:ensure #f))
        (val-old (module-ref mod var-name)))

        (dynamic-wind
          (lambda () (module-set! mod var-name val))
          (lambda () expr ...)
          (lambda () (module-set! mod var-name val-old)))))
    ((_ var-name val thunk)
      (stub (current-module) var-name val thunk))))

(define end '())

(define (println . args) (for-each display args) (newline))

(define (assert-equal expected got)
  "Checks that the expected value is equal to the received value.

  Arguments
    expected: any: the expected value.

    got: any: the received value.

  Returns:
    (list test-status expected #f got): a list made up of the status
    of the assertion and diagnostic details.

      test-status: boolean: #t if the assert succeeded, #f otherwise.

      expected: any: the 'expected' value that was passed in to the
      function.

      #f: this is a flag that indicates that the 'expected' value is
      understood to be the actual expected value (see below).

      got: any: the 'got' value that was passed in to the function."
  (list (equal? expected got) expected #f got))

(define (assert-not-equal not-expected got)
  "Like assert-equal, but checks that the specified value is not equal
  to the received value.

  Arguments:
    not-expected: any: the value not expected.

    got: any: the received value.

  Returns
    Like 'assert-equal', but the third item in the list, the flag, is
    set to #t to indicate that the 'expected' value being passed back
    was actually _not_ expected."
  (list (not (equal? not-expected got)) not-expected #t got))

(define (assert-lt got expected)
  "Checks that the first argument is less than the second."
  (list (< got expected) (list "less than " expected) #f got))

(define (assert-gt got expected)
  "Checks that the first argument is greater than the second."
  (list (> got expected) (list "greater than " expected) #f got))

(define (assert-lte got expected)
  "Checks that the first argument is less than or equal to the second."
  (list
    (<= got expected)
    (list "less than or equal to " expected)
    #f
    got))

(define (assert-gte got expected)
  "Checks that the first argument is greater than or equal to the
  second."
  (list
    (>= got expected)
    (list "greater than or equal to " expected)
    #f
    got))

(define (assert-between got lower upper)
  "Checks that the first argument is between the second and third.

  Arguments
    got: number: what we want to check.

    lower: number: the lower bound (inclusive).

    upper: number: the upper bound (inclusive).

  Returns
    Like 'assert-equal', but the first element of the result list is #t
    if got is between lower and upper inclusive, and #f otherwise."
  (assert-all (assert-gte got lower) (assert-lte got upper)))

#!
Checks that the first argument is near the second, optionally specifying
how near with the third argument. This is useful for comparing
floating-point numbers. E.g., if you do:

  (assert-equal 3.3 (* 3 1.1))

... the assertion will fail because of floating-point arithmetic. But if
you do:

  (assert-near 3.3 (* 3 1.1))

... it will succeed.

Arguments
  got: number: what we want to check.

  expected: number: the number that 'got' should be close to in value.

  within: number: the maximum distance we want to tolerate between 'got'
  and 'expected'. Optional. If omitted, assumed to be 0.01.

Returns
  Like 'assert-equal', but the first element of the result list is #t if
  'got' is near 'expected', with the default or custom-set tolerance, or
  #f otherwise.
!#
(define assert-near
  (case-lambda
    ((got expected within)
      (assert-between got (- expected within) (+ expected within)))
    ((got expected)
      (assert-near got expected 0.01))))

(define (assert-true x) (assert-equal 'true (if x 'true 'false)))
(define (assert-false x) (assert-equal 'false (if x 'true 'false)))

(define (assert-all . exprs)
  "Asserts all of the given assertions (see above). In other words,
  composes a set of assertions together into a single 'super-assertion'.

  Arguments
    exprs: a variable number of assertions created with one of the above
    assertion functions.

  Returns
    A failure result from the first assertion that fails, if any; or a
    success result."
  (let loop
    ((exprs exprs))

    (if (null? exprs)
      (list #t #t #f #t)
      (let*
        ((first-expr (car exprs))
        (test-status (car first-expr))
        (expected (cadr first-expr))
        (flag (caddr first-expr))
        (got (cadddr first-expr)))

        (if (not test-status)
          (list #f expected flag got)
          (loop (cdr exprs)))))))

(define-syntax error?
  (syntax-rules ()
    ((_ expr) (catch #t (lambda () expr #f) (lambda _ #t)))))

(define (results-add r1 r2)
  "Returns a new results list created by adding together the individual
  passes, fails and skips from the two passed-in lists.

  Arguments
    r1: (list num-passes num-fails num-skips): the first results list.
    r2: (list num-passes num-fails num-skips): the second results list.

  Returns
    (list num-passes num-fails num-skips)"
  (list
    (+ (suite-passed r1) (suite-passed r2))
    (+ (suite-failed r1) (suite-failed r2))
    (+ (suite-skipped r1) (suite-skipped r2))))

#!
Declares a test suite.

Arguments
  desc: string: description of the suite.

  tsts: (tests tst ...): a collection of tests to run in this suite.

    tst: (test desc expr opts)

      desc: string: description of the test.
      expr: the body of the test.
      opts: same as suite options, see immediately below.

  opts: (options opt ...): a collection of options to pass in to the
  suite. Optional. Default is no options to pass in to the suite.

    opt: (option opt-name opt-val)

      opt-name: symbol: the name of the option.
      opt-val: any: the value being given to the option.

  sups: (setups sup ...): a collection of setup names and values to pass
  into each test. Optional. Default is no setups.

    sup: (setup sup-name expr ...)

      sup-name: symbol

      expr ...: the expressions to evaluate. The last of these will be
      the value assigned to the setup name. Will be re-evaluated each
      time a test is run.

  tdowns: (teardowns tdown ...): a collection of teardowns to run after
  running each test. Optional. Default is no teardowns.

    tdown: (teardown e body ...)

      e: the 'environment' that was set up by the setup functions.
      Required even if no setups were done.

      body: ... the body expressions of the teardown.

Side Effects
  Outputs descriptive and diagnostic messages using the given runtime
  message ('output-cb') function.

Returns
  (list num-passes num-fails num-skips)

    num-passes: number: the number of passed tests.
    num-fails: number: the number of failed tests.
    num-skips number: the number of skipped tests.
!#
(define suite
  (case-lambda
    ((desc tsts opts sups tdowns)
      (define num-tests (length tsts))
      (define output-cb
        (if-let v (assoc-ref opts 'output-cb) v output-normal))
      (define colour? (if-let v (assoc-ref opts 'colour) v))
      (define skip? (if-let v (assoc-ref opts 'skip) v))
      (define tally? (if-let v (assoc-ref opts 'tally) v))
      (define (run-test-safely tst tdowns env)
        (define retval
          (catch
            #t
            (lambda () (tst env))
            (lambda (key . params)
              (list #f "No error" #f (cons key (cons ": " params))))))

        (for-each (lambda (td) (td env)) tdowns)
        retval)

      (output-cb #:suite-desc desc)
      (if skip?
        ;; Skip all tests in this suite.
        (begin
          (for-each
            (lambda (tst)
              (output-cb #:test-desc (car tst) #:test-status 'skip))
            tsts)
          (if tally?
            (output-cb
              #:tally-passed 0
              #:tally-failed 0
              #:tally-skipped num-tests))
          (output-cb #:suite-status 'complete)
          (list 0 0 num-tests))
        (begin
          (let*
            ((suite-bindings
              (list
                #!
                These assertion functions are now deprecated in favour
                of the plain, pure assertion functions above.
                !#
                (cons
                  'assert-equal
                  (lambda (expected got)
                    (assert-equal expected got)))
                (cons
                  'assert-not-equal
                  (lambda (expected got)
                    (assert-not-equal expected got)))
                (cons
                  'assert-true
                  (lambda (x) (assert-true x)))
                (cons
                  'assert-false
                  (lambda (x) (assert-false x)))))
            (intermediate-results
              (map
                (lambda (tst)
                  (define test-desc (car tst))
                  (define test-bindings
                    (append
                      suite-bindings
                      (map
                        (lambda (sup) (cons (car sup) ((cdr sup))))
                        (or sups end))))
                  (define (env name) (assoc-ref test-bindings name))

                  (let*
                    ;; Run the test's function and all teardowns:
                    ((result (run-test-safely (caddr tst) tdowns env))
                    ;; Extract parts of each result:
                    (test-status (car result)) ; #t or #f
                    (expected (cadr result))
                    (flag (caddr result))
                    (got (cadddr result)))

                    ;; Output diagnostics:
                    (output-cb
                      #:colour colour?
                      #:test-desc test-desc
                      #:test-status (if test-status 'pass 'fail)
                      #:got got
                      (if flag #:not-expected #:expected) expected)
                    test-status))
                (or
                  (filter
                    (lambda (tst)
                      ;; If a 'skip option is given in a test ...
                      (if-let skip? (assoc-ref (cadr tst) 'skip)
                        #!
                        Skip this test if the 'skip option has a value
                        #t
                        !#
                        (begin
                          (output-cb
                            #:test-desc (car tst)
                            #:test-status 'skip)
                          (not skip?))
                        #!
                        If a 'skip option is not given, don't skip this
                        test
                        !#
                        #t))
                    tsts)
                  end)))
            (num-passes
              (length (filter identity intermediate-results)))
            (num-fails
              (length
                (filter
                  (lambda (result) (not result))
                  intermediate-results)))
            (num-skips (- num-tests num-passes num-fails)))

            (if tally?
              (output-cb
                #:tally-passed num-passes
                #:tally-failed num-fails
                #:tally-skipped num-skips))
            (output-cb #:suite-status 'complete)
            (list
              num-passes
              num-fails
              num-skips)))))
    ((desc tsts) (suite desc tsts end end end))
    ((desc tsts opts) (suite desc tsts opts end end))
    ((desc tsts opts sups) (suite desc tsts opts sups end))))

(define (suite-passed s) (car s))
(define (suite-failed s) (cadr s))
(define (suite-skipped s) (caddr s))

(define options list)
(define option cons)
(define setups list)

#!
Declares a setup symbol-binding.

Arguments
  'sym: symbol: a symbol by which to refer to the bound value.

  expr ...: any: a variable number of expressions, the last of which will be
  bound to the symbol above. This value may later be accessed from any test in
  the same suite by calling the test's 'environment' (usually e) with the
  symbol. E.g., (e 'sym).
!#
(define-syntax setup
  (syntax-rules ()
    ((_ sym expr ...)
      (cons sym (lambda () expr ...)))))

(define tests list)

#!
Declares a test.

Arguments
  desc: string: a description of the test.

  env: an 'environment' (an alist of names and bindings) that is passed
  in to the test. Two types of names are defined:

    1. Names always automatically defined by ggspec before running the
    test: 'assert-equal, 'assert-not-equal, etc. These are defined by
    ggspec automatically because they depend on the suite options like
    where to send test runtime messages to.

    2. Names defined by the test writer by setting up names and
    corresponding values in the suite setup section. The corresponding
    values are evaluated anew each time a test is run, which is why in
    the setup section you have to wrap each value up inside a thunk.

  expr: a value returned by one of the above assertion functions. An
  expression that makes up the body of the test. This will be wrapped
  inside a function and the function will be passed in the 'environment'
  env from above.

  opts: same type as in 'suite', above. Optional (default is no
  options).

Returns
  (list desc opts func): a three-member list of the test description,
  the options passed in to the test, and the unevaluated function making
  up the body of the test.
!#
(define-syntax test
  (syntax-rules ()
    ((_ desc env expr opts)
      (list desc opts (lambda (env) expr)))
    ((_ desc env expr)
      (test desc env expr end))))

(define teardowns list)

#!
Declares a teardown. Every teardown will be run after each test, and
each teardown will be passed the 'environment' that was defined for that
test.

Arguments
  env: same as in 'test', above.

  expr ...: any number of expressions.

Returns
  A function which takes an 'environment' and carries out any number of
  actions.
!#
(define-syntax teardown
  (syntax-rules ()
    ((_ env expr ...) (lambda (env) expr ...))))

(define (kwalist arglist)
  "Turn a list of keyword arguments into an alist of symbols and
  values.

  Arguments
    arglist: (list #:kw1 arg1 ...)

  Returns
    (list (cons sym1 arg1) ...)"
  (cond
    ((null? arglist) end)
    ((= 1 (length arglist)) (error "Keyword argument error"))
    (#t
      (cons
        (cons
          (keyword->symbol (car arglist))
          (cadr arglist))
        (kwalist (cddr arglist))))))

(define (text-verbose . kwargs)
  (define kws (kwalist kwargs))
  (define (when-then-print sym msg)
    (if (assoc sym kws)
      (println msg (assoc-ref kws sym))))

  (when-then-print 'suite-desc "  Suite: ")
  (when-then-print 'test-desc "    Test: ")
  (when-then-print 'expected "      Expected: ")
  (when-then-print 'not-expected "      Not expected: ")
  (when-then-print 'got "      Got: ")
  (when-then-print 'test-status "      Assert ")
  (when-then-print 'test-status "    Test "))

#!
Varieties of calls to the 'output-cb' function(s)

#:suite-desc desc

#:test-desc desc #:test-status 'skip

#:colour (#t OR #f)
#:test-desc test-desc
#:test-status ('pass OR 'fail OR 'skip)
#:got got
(#:not-expected OR #:expected) expected

#:final-tally (#t OR #f)
#:tally-passed num-passes
#:tally-failed num-fails
#:tally-skipped num-skips

#:tally-passed num-passes
#:tally-failed num-fails
#:tally-skipped num-skips

#:suite-status 'complete
!#
(define (output-normal . kwargs)
  (define kws (kwalist kwargs))
  ;; We'll use this helper function to access the keyword arguments.
  (define (kw sym) (assoc-ref kws sym))

  (define colour-red
    (if-let v (kw 'colour) kolour-red ""))
  (define colour-green
    (if (equal? colour-red "") "" kolour-green))
  (define colour-normal
    (if (equal? colour-red "") "" kolour-normal))

  (if-let suite-desc (kw 'suite-desc) (println "  Suite: " suite-desc))
  (if-let suite-status (kw 'suite-status) (newline))
  (if-let final-tally? (kw 'final-tally)
    (if-let passed (kw 'tally-passed)
      (if-let failed (kw 'tally-failed)
        (if-let skipped (kw 'tally-skipped)
          (println
            "Total " (+ passed failed skipped)
            ", passed " passed
            ", failed " failed
            ", skipped " skipped
            ".")))))

  (if-let test-status (kw 'test-status)
    (if-let test-desc (kw 'test-desc)
      (cond
        ((equal? test-status 'pass)
          (println "    " colour-green "[PASS]" colour-normal))
        ((equal? test-status 'skip)
          (println "    [SKIP] " test-desc))
        (#t ; The test failed:
          (if-let got (kw 'got)
            (begin
              (println
                "    "
                colour-red
                "[FAIL] "
                colour-normal
                test-desc)
              (if-let expected (kw 'expected)
                (begin
                  (println "      Expected: '" expected "'")
                  (println "           Got: '" got "'"))
                (if-let not-expected (kw 'not-expected)
                  (begin
                    (println "      Expected: not '" not-expected "'")
                    (println "           Got: '" got "'")))))
            (println "      Test failed: details unavailable")))))))

(define (output-tap . kwargs)
  "Output suite and test results in TAP format.

  Arguments
    See varieties of keyword arguments above.

  Side Effects
    Outputs to the current standard output port test results in (a
    subset of) TAP format. Does not output the 'plan' (the total number
    of tests run)--that job should be done by the 'ggspec' script."
  (define kws (kwalist kwargs))
  ;; We'll use this helper function to access the keyword arguments.
  (define (kw sym) (assoc-ref kws sym))

  (define colour-red
    (if-let v (kw 'colour) kolour-red ""))
  (define colour-green
    (if (equal? colour-red "") "" kolour-green))
  (define colour-normal
    (if (equal? colour-red "") "" kolour-normal))

  (if-let suite-desc (kw 'suite-desc)
    (println "# Suite: " suite-desc))
  (if-let passed (kw 'tally-passed)
    (if-let failed (kw 'tally-failed)
      (if-let skipped (kw 'tally-skipped)
        (println "1.." (+ passed failed skipped)))))

  (if-let test-status (kw 'test-status)
    (if-let test-desc (kw 'test-desc)
      (cond
        ((equal? test-status 'pass)
          (println colour-green "ok" colour-normal " - " test-desc))
        ((equal? test-status 'skip)
          (println
            colour-green
            "ok"
            colour-normal
            " - "
            test-desc
            " # SKIP"))
        (#t ; The test failed:
          (if-let got (kw 'got)
            (begin
              (println
                colour-red
                "not ok"
                colour-normal
                " - "
                test-desc)
              (if-let expected (kw 'expected)
                (begin
                  (println "# Expected: '" expected "'")
                  (println "#      Got: '" got "'"))
                (if-let not-expected (kw 'not-expected)
                  (begin
                    (println "# Expected: not '" not-expected "'")
                    (println "#      Got: '" got "'")))))
            (println "# Test failed: details unavailable")))))))

(define (output-none . _) #f)

(define (suite-add-option opt s)
  "Add an option to the read, unevaluated form of a suite.

  Arguments
    opt: '(option 'k v)

      'k: symbol: name of the option.
      v: any: value of the option.

    s: form: a read, unevaluated 'suite' form:

      (suite desc tsts opts sups tdowns)

      opts, sups, and tdowns may be missing.

  Returns
    A read, unevaluated suite with the option added."
  (define (opts-add-opt os o)
    (if
      (member
        (cadr o)
        (map (lambda (opt) (cadr opt)) (cdr os)))
      os
      (cons 'options (cons o (cdr os)))))

  #!
  Below, variable names have the following meanings:

  d: suite description
  ts: a list of tests
  os: a list of options
  ss: a list of setups
  tds: a list of teardowns
  !#
  (match s
    (('suite d ts) (list 'suite d ts (list 'options opt)))
    (('suite d ts os) (list 'suite d ts (opts-add-opt os opt)))
    (('suite d ts os ss) (list 'suite d ts (opts-add-opt os opt) ss))
    (('suite d ts os ss tds)
      (list 'suite d ts (opts-add-opt os opt) ss tds))))

(define (run-file fname opts)
  "Run all test suites found in the given file, passing in all given
  options, and aggregate and return the number of passed and failed
  tests.

  Arguments
    fname: string: the name of the file to look in for test suites.

    opts: (list opt ...)

      opt: '(option 'k v)

        'k: symbol: the name of the option to pass to all suites in the
        file.

        v: any: the value of the option.

  Returns
    (list num-passes num-fails): same as in the suite function."
  (call-with-input-file
    fname
    (lambda (f)
      (let loop
        ((form (read f))
        (final-result (list 0 0 0)))

        (cond
          ((eof-object? form) final-result)
          ((equal? (car form) 'suite)
            ;; Add options to the current suite and run it.
            (let
              ((results
                (eval
                  (fold suite-add-option form opts)
                  (current-module))))

              (loop
                (read f)
                (results-add final-result results))))
          (#t
            ;; This is some form other than a suite definition.
            (begin
              (eval form (current-module))
              ;; Go on to the next form, with results unchanged.
              (loop (read f) final-result))))))))

