/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <libguile.h>
#include <boost/algorithm/string/predicate.hpp>

#include "catch.hpp"

#include "libfive-guile.h"

////////////////////////////////////////////////////////////////////////////////

static SCM eval_inner(void* body) {
    std::string input = *(std::string*)body;
    return scm_simple_format(SCM_BOOL_F, scm_from_locale_string("~A"),
            scm_list_1(scm_c_eval_string(input.c_str())));
}

static SCM handler(void*, SCM key, SCM args)
{
    return scm_simple_format(SCM_BOOL_F, scm_from_locale_string("~A: ~A"),
           scm_list_2(key, scm_simple_format(
                SCM_BOOL_F, scm_cadr(args), scm_caddr(args))));
}

static std::string eval(std::string input) {
    static bool initialized = false;
    if (!initialized)
    {
        scm_init_guile();
        scm_init_libfive_modules();
        scm_c_use_module("libfive kernel");
        scm_c_use_module("libfive vec");
        scm_c_use_module("libfive csg");
        scm_c_use_module("libfive shapes");
        scm_c_use_module("libfive util");
        scm_c_use_module("libfive sandbox");
        initialized = true;
    }

    auto str = scm_to_locale_string(
            scm_internal_catch(SCM_BOOL_T, eval_inner, &input, handler, NULL));
    std::string out(str);
    free(str);

    return out;
}

////////////////////////////////////////////////////////////////////////////////

TEST_CASE("make-shape")
{
    SECTION("Valid")
    {
        auto result = eval("(make-shape 'var-x)");
        CAPTURE(result);
        REQUIRE(boost::algorithm::starts_with(result, "#<<shape> "));
    }

    SECTION("Invalid opcode")
    {
        auto result = eval("(make-shape 'lol)");
        CAPTURE(result);
        REQUIRE(boost::algorithm::starts_with(result, "wrong-type-arg:"));
    }

    SECTION("Invalid argument count")
    {
        auto a = eval("(make-shape 'var-x 12)");
        CAPTURE(a);
        REQUIRE(boost::algorithm::starts_with(a, "wrong-number-of-args:"));

        auto b = eval("(make-shape 'add 12)");
        CAPTURE(b);
        REQUIRE(boost::algorithm::starts_with(b, "wrong-number-of-args:"));
    }

    SECTION("Invalid argument type")
    {
        auto a = eval("(make-shape 'sin \"hi\")");
        CAPTURE(a);
        REQUIRE(boost::algorithm::starts_with(a, "wrong-type-arg:"));
    }
}

TEST_CASE("make-var")
{
    auto result = eval("(make-var)");
    CAPTURE(result);
    REQUIRE(boost::algorithm::starts_with(result, "#<<shape> "));
}

TEST_CASE("var?")
{
    SECTION("#t")
    {
        auto result = eval("(var? (make-var))");
        CAPTURE(result);
        REQUIRE(result == "#t");
    }

    SECTION("#f")
    {
        auto result = eval("(var? (make-shape 'var-x))");
        CAPTURE(result);
        REQUIRE(result == "#f");
    }
}

TEST_CASE("Guile overloads")
{
    SECTION("min")
    {
        auto result = eval("(min (make-shape 'var-x) 1 2 3)");
        CAPTURE(result);
        REQUIRE(boost::algorithm::starts_with(result, "#<<shape> "));

        auto err = eval("(min)");
        CAPTURE(err);
        REQUIRE(boost::algorithm::starts_with(err, "goops-error"));

        auto num = eval("(min 1 2 3)");
        REQUIRE(num == "1");
    }
}

TEST_CASE("#[vector notation]")
{
    auto result = eval("#[  1  2.1  ]");
    REQUIRE(result == "#[1 2.1]");
}

TEST_CASE("shape-eval")
{
    auto result = eval(
            "(shape-eval (lambda-shape (x y z) (* x (/ 1 z))) #[0 0 0])");
    REQUIRE(result == "+nan.0");
}

TEST_CASE("eval-sandboxed")
{
    SECTION("Single line")
    {
        auto result = eval("(eval-sandboxed \"(+ 1 2 3)\")");
        REQUIRE(result == "((valid 6))");
    }

    SECTION("Multi-line")
    {
        auto result = eval("(eval-sandboxed \"(define-shape (f x y z) (+ x y))(+ f 12)\")");
        CAPTURE(result);
        REQUIRE(boost::algorithm::starts_with(result,
            "((valid #<unspecified>) (valid #<<shape>"));
    }

    SECTION("Error on single line")
    {
        auto result = eval("(eval-sandboxed \"(+ 1 2 'omg)\")");
        CAPTURE(result);
        REQUIRE(boost::algorithm::starts_with(result,
            "((error (0 . 0)"));
    }

    SECTION("Parsing variables")
    {
        auto a = eval("(eval-sandboxed \"#123\")");
        CAPTURE(a);
        REQUIRE(boost::algorithm::starts_with(a, "((valid #<<shape>"));
    }
}

TEST_CASE("libfive-guile CSG")
{
    auto result = eval(R"(
        (difference
            (box #[0 0 0] #[10 10 10])
            (box #[5 5 5] #[11 11 11])
        ) )");

    CAPTURE(result);
    REQUIRE(boost::algorithm::starts_with(result, "#<<shape> "));
}
