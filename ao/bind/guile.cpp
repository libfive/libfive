#include <libguile.h>

#include <cstdlib>
#include <cassert>

#include "lib.h"

void del_tree(void* t)      { ao_tree_delete((ao_tree)t); }

SCM scm_wrap_tree = NULL;
SCM scm_unwrap_tree = NULL;
SCM scm_tree_p = NULL;

bool scm_is_tree(SCM t) { return scm_is_true(scm_call_1(scm_tree_p, t)); }

SCM scm_number_to_tree(SCM n)
{
    SCM_ASSERT_TYPE(scm_is_number(n), n, 0, "scm_number_to_tree", "number");
    auto ptr = scm_from_pointer(ao_tree_const(scm_to_double(n)), del_tree);
    return scm_call_1(scm_wrap_tree, ptr);
}

SCM scm_tree(SCM op, SCM a, SCM b)
{
    SCM_ASSERT_TYPE(scm_is_symbol(op), op, 0, "scm_tree", "symbol");

    auto str = scm_to_locale_string(scm_symbol_to_string(op));
    auto opcode = ao_opcode_enum(str);
    free(str);

    SCM_ASSERT_TYPE(opcode != -1, op, 0, "scm_tree", "opcode");
    auto args = ao_opcode_args(opcode);

    if (args >= 1)
    {
        SCM_ASSERT_TYPE(scm_is_number(a) || scm_is_tree(a),
                        a, 1, "scm_tree", "number or tree");
        if (scm_is_number(a)) { a = scm_number_to_tree(a); }
    }
    if (args >= 2)
    {
        SCM_ASSERT_TYPE(scm_is_number(b) || scm_is_tree(b),
                        b, 2, "scm_tree", "number or tree");
        if (scm_is_number(b)) { b = scm_number_to_tree(b); }
    }

    ao_tree out = nullptr;
    switch (args)
    {
        case 0: out = ao_tree_nonary(opcode); break;
        case 1: out = ao_tree_unary(opcode,
            (ao_tree)scm_to_pointer(scm_call_1(scm_unwrap_tree, a))); break;
        case 2: out = ao_tree_binary(opcode,
            (ao_tree)scm_to_pointer(scm_call_1(scm_unwrap_tree, a)),
            (ao_tree)scm_to_pointer(scm_call_1(scm_unwrap_tree, b))); break;
        default: assert(false);
    }

    auto ptr = scm_from_pointer(out, del_tree);
    return scm_call_1(scm_wrap_tree, ptr);
}

void init_ao(void*)
{
    scm_c_eval_string(R"(
(use-modules (system foreign))

(define-wrapped-pointer-type
    tree tree? wrap-tree unwrap-tree
    (lambda (o p)
        (format p "#<tree 0x~x>"
        (pointer-address (unwrap-tree o))))) )");

    scm_tree_p = scm_c_eval_string("tree?");
    scm_wrap_tree = scm_c_eval_string("wrap-tree");
    scm_unwrap_tree = scm_c_eval_string("unwrap-tree");

    scm_c_define_gsubr("make-tree", 1, 2, 0, (void*)scm_tree);
    scm_c_define_gsubr("number->tree", 1, 0, 0, (void*)scm_number_to_tree);
    scm_c_export("make-tree", "number->tree", "tree?", "tree", "wrap-tree", "unwrap-tree");
}

extern "C"
{

void scm_init_ao_kernel_module()
{
    scm_c_define_module("ao kernel", init_ao, NULL);
}

}   // extern C
