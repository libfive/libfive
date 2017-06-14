#include <libguile.h>
#include <boost/algorithm/string/predicate.hpp>
#include "catch.hpp"

#include "guile.h"

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
    scm_init_guile();
    scm_init_ao_kernel_module();
    scm_c_use_module("ao kernel");

    auto str = scm_to_locale_string(
            scm_internal_catch(SCM_BOOL_T, eval_inner, &input, handler, NULL));
    std::string out(str);
    free(str);

    return out;
}

////////////////////////////////////////////////////////////////////////////////

TEST_CASE("make-tree")
{
    SECTION("Valid")
    {
        auto result = eval("(make-tree 'x)");
        CAPTURE(result);
        REQUIRE(boost::algorithm::starts_with(result, "#<tree "));
    }

    SECTION("Invalid opcode")
    {
        auto result = eval("(make-tree 'lol)");
        CAPTURE(result);
        REQUIRE(boost::algorithm::starts_with(result, "wrong-type-arg:"));
    }

    SECTION("Invalid argument count")
    {
        auto a = eval("(make-tree 'x 12)");
        CAPTURE(a);
        REQUIRE(boost::algorithm::starts_with(a, "wrong-number-of-args:"));

        auto b = eval("(make-tree 'add 12)");
        CAPTURE(b);
        REQUIRE(boost::algorithm::starts_with(b, "wrong-number-of-args:"));
    }

    SECTION("Invalid argument type")
    {
        auto a = eval("(make-tree 'sin \"hi\")");
        CAPTURE(a);
        REQUIRE(boost::algorithm::starts_with(a, "wrong-type-arg:"));
    }
}

TEST_CASE("Guile overloads")
{
    SECTION("min")
    {
        auto result = eval("(min (make-tree 'x) 1 2 3)");
        CAPTURE(result);
        REQUIRE(boost::algorithm::starts_with(result, "#<tree "));

        auto err = eval("(min)");
        CAPTURE(err);
        REQUIRE(boost::algorithm::starts_with(err, "wrong-number-of-args"));
    }
}
