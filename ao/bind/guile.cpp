#include <libguile.h>

#include <cstdlib>
#include <cassert>

#include "lib.h"
#include "guile.h"

void del_tree(void* t)      { ao_tree_delete((ao_tree)t); }

SCM scm_wrap_tree_ = NULL;
SCM scm_unwrap_tree_= NULL;
SCM scm_tree_p_= NULL;

SCM scm_wrap_tree(SCM ptr)      { return scm_call_1(scm_wrap_tree_, ptr); }
SCM scm_unwrap_tree(SCM ptr)    { return scm_call_1(scm_unwrap_tree_, ptr); }
SCM scm_tree_p(SCM ptr)         { return scm_call_1(scm_tree_p_, ptr); }

bool scm_is_tree(SCM t) { return scm_is_true(scm_tree_p(t)); }

SCM scm_number_to_tree(SCM n)
{
    SCM_ASSERT_TYPE(scm_is_number(n), n, 0, "scm_number_to_tree", "number");
    auto ptr = scm_from_pointer(ao_tree_const(scm_to_double(n)), del_tree);
    return scm_wrap_tree(ptr);
}

SCM scm_tree_equal_p(SCM a, SCM b)
{
    SCM_ASSERT_TYPE(scm_is_tree(a), a, 0, "scm_tree_equal_p", "tree");
    SCM_ASSERT_TYPE(scm_is_tree(b), b, 1, "scm_tree_equal_p", "tree");

    return ao_tree_eq((ao_tree)scm_to_pointer(scm_unwrap_tree(a)),
                      (ao_tree)scm_to_pointer(scm_unwrap_tree(b)))
        ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM scm_tree(SCM op, SCM a, SCM b)
{
    SCM_ASSERT_TYPE(scm_is_symbol(op), op, 0, "scm_tree", "symbol");

    auto str = scm_to_locale_string(scm_symbol_to_string(op));
    auto opcode = ao_opcode_enum(str);
    free(str);

    SCM_ASSERT_TYPE(opcode != -1, op, 0, "scm_tree", "opcode");
    auto args = ao_opcode_args(opcode);

    if (args == 0)
    {
        if (a != SCM_UNDEFINED || b != SCM_UNDEFINED)
        {
            scm_wrong_num_args(scm_from_locale_string("scm_tree"));
            return SCM_UNDEFINED;
        }
    }
    else if (args == 1)
    {
        SCM_ASSERT_TYPE(scm_is_number(a) || scm_is_tree(a),
                        a, 1, "scm_tree", "number or tree");
        if (b != SCM_UNDEFINED)
        {
            scm_wrong_num_args(scm_from_locale_string("scm_tree"));
            return SCM_UNDEFINED;
        }
        if (scm_is_number(a)) { a = scm_number_to_tree(a); }
    }
    else if (args == 2)
    {
        if (a == SCM_UNDEFINED || b == SCM_UNDEFINED)
        {
            scm_wrong_num_args(scm_from_locale_string("scm_tree"));
            return SCM_UNDEFINED;
        }
        SCM_ASSERT_TYPE(scm_is_number(a) || scm_is_tree(a),
                        a, 1, "scm_tree", "number or tree");
        SCM_ASSERT_TYPE(scm_is_number(b) || scm_is_tree(b),
                        b, 2, "scm_tree", "number or tree");
        if (scm_is_number(a)) { a = scm_number_to_tree(a); }
        if (scm_is_number(b)) { b = scm_number_to_tree(b); }
    }

    ao_tree out = nullptr;
    switch (args)
    {
        case 0: out = ao_tree_nonary(opcode); break;
        case 1: out = ao_tree_unary(opcode,
            (ao_tree)scm_to_pointer(scm_unwrap_tree(a))); break;
        case 2: out = ao_tree_binary(opcode,
            (ao_tree)scm_to_pointer(scm_unwrap_tree(a)),
            (ao_tree)scm_to_pointer(scm_unwrap_tree(b))); break;
        default: assert(false);
    }

    auto ptr = scm_from_pointer(out, del_tree);
    return scm_wrap_tree(ptr);
}

SCM scm_tree_eval(SCM t, SCM x, SCM y, SCM z)
{
    SCM_ASSERT_TYPE(scm_is_tree(t), t, 0, "scm_tree_eval", "tree");

    SCM_ASSERT_TYPE(scm_is_number(x), x, 1, "scm_tree_eval", "number");
    SCM_ASSERT_TYPE(scm_is_number(y), y, 2, "scm_tree_eval", "number");
    SCM_ASSERT_TYPE(scm_is_number(z), z, 3, "scm_tree_eval", "number");

    float x_ = scm_to_double(x);
    float y_ = scm_to_double(y);
    float z_ = scm_to_double(z);

    auto r = ao_tree_eval_f((ao_tree)scm_to_pointer(scm_unwrap_tree(t)),
                            {x_, y_, z_});

    return scm_from_double(r);
}

SCM scm_tree_to_mesh(SCM t, SCM f, SCM res, SCM region)
{
    SCM_ASSERT_TYPE(scm_is_tree(t), t, 0, "scm_tree_to_mesh", "tree");
    SCM_ASSERT_TYPE(scm_is_string(f), f, 1, "scm_tree_to_mesh", "string");
    SCM_ASSERT_TYPE(scm_is_number(res), res, 2, "scm_tree_to_mesh", "number");
    SCM_ASSERT_TYPE(scm_is_true(scm_list_p(region)) &&
                    scm_to_int(scm_length(region)) == 3, region, 3,
                    "scm_tree_to_mesh", "three-element list");

    float rs[6];
    for (int i=0; i < 3; ++i)
    {
        SCM_ASSERT_TYPE(scm_is_pair(scm_car(region)) &&
                scm_is_number(scm_caar(region)) &&
                scm_is_number(scm_cdar(region)),
                scm_car(region), 3,
                "scm_tree_to_mesh", "pair of numbers");
        rs[i*2 + 0] = scm_to_double(scm_caar(region));
        rs[i*2 + 1] = scm_to_double(scm_cdar(region));
        region = scm_cdr(region);
    }


    auto filename = scm_to_locale_string(f);
    auto out = ao_tree_save_mesh((ao_tree)scm_to_pointer(scm_unwrap_tree(t)),
            {{rs[0], rs[1]}, {rs[2], rs[3]}, {rs[4], rs[5]}},
            scm_to_double(res), filename);
    free(filename);

    return out ? SCM_BOOL_T : SCM_BOOL_F;
}

void init_ao(void*)
{
    scm_c_eval_string(R"(
(use-modules (system foreign))

(define-wrapped-pointer-type
    tree tree? wrap-tree unwrap-tree
    (lambda (o p)
        (format p "#<tree 0x~x>"
        (pointer-address (unwrap-tree o)))))
)");

    scm_tree_p_ = scm_c_eval_string("tree?");
    scm_wrap_tree_ = scm_c_eval_string("wrap-tree");
    scm_unwrap_tree_ = scm_c_eval_string("unwrap-tree");

    scm_c_define_gsubr("make-tree", 1, 2, 0, (void*)scm_tree);
    scm_c_define_gsubr("number->tree", 1, 0, 0, (void*)scm_number_to_tree);
    scm_c_define_gsubr("tree-equal?", 2, 0, 0, (void*)scm_tree_equal_p);
    scm_c_define_gsubr("tree-eval", 4, 0, 0, (void*)scm_tree_eval);
    scm_c_define_gsubr("tree->mesh", 4, 0, 0, (void*)scm_tree_to_mesh);

    // Overload all of the arithmetic operations with tree-based substitutes!
    scm_c_eval_string(R"(
(use-modules (srfi srfi-1) (ice-9 receive))

(define (make-commutative func sym default)
    (lambda (. args)
        (if (any tree? args)
            (fold (lambda (e p) (make-tree sym e p)) default args)
            (apply func args))))

(define (make-semicommutative func sym inverse)
    (lambda (. args)
        (format #t "args: ~A len ~A\n" args (> (length args) 1))
        (if (any tree? args)
            (if (> (length args) 1)
                (make-tree sym (car args) (apply inverse (cdr args)))
                (make-tree sym (inverse) (car args)))
            (apply func args))))

(define (make-unary func sym)
    (lambda (a) (if (number? a) (func a) (make-tree sym a))))

(define-syntax-rule (overload-commutative func sym default)
    (define func (make-commutative func sym default)))

(define-syntax-rule (overload-semicommutative func sym inverse)
    (define func (make-semicommutative func sym inverse)))

(define-syntax-rule (overload-unary func sym)
    (define func (make-unary func sym)))

(overload-commutative + 'add 0)
(overload-commutative * 'mul 1)
(overload-commutative min 'min #nil)
(overload-commutative max 'max #nil)

(overload-semicommutative - 'sub +)
(overload-semicommutative / 'div *)

(overload-unary sqrt 'sqrt)
(overload-unary abs 'abs)
(overload-unary sin 'sin)
(overload-unary cos 'cos)
(overload-unary tan 'tan)
(overload-unary asin 'asin)
(overload-unary acos 'acos)
(overload-unary exp 'exp)

(define (square f)
    (if (number? f) (* f f) (make-tree 'square f)))

(define $atan atan)
(define* (atan a #:optional b)
    (if b
        (if (every number? (list a b))
            ($atan a b)
            (make-tree 'atan2 a b))
        (if (number? a) ($atan a) (make-tree 'atan a))))

(define $expt expt)
(define (expt a b)
    (if (tree? a)
        (begin
            (when (not (rational? b))
                (scm-error 'wrong-type-arg #f
                    "RHS of exponentiation must be rational, not ~A"
                    (list b) #f))
            (make-tree 'nth-root (make-tree 'pow a (numerator b))
                                 (denominator b)))
        ($expt a b)))

(define $modulo modulo)
(define (modulo a b)
    (if (every number? (list a b))
        ($modulo a b)
        (make-tree 'mod a b)))

(export! + * min max - /
    sqrt abs sin cos tan asin acos exp square atan expt mod)
)");

    scm_c_export(
            "tree?", "tree", "wrap-tree", "unwrap-tree",
            "make-tree", "number->tree", "tree-equal?", "tree-eval",
            "tree->mesh",
            NULL);
}

void scm_init_ao_kernel_module()
{
    scm_c_define_module("ao kernel", init_ao, NULL);
}
