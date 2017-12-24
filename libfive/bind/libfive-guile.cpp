/*
Guile bindings to the libfive CAD kernel
Copyright (C) 2017  Matt Keeter

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#include <cstdlib>
#include <cassert>

#include "libfive-guile.h"
#include "libfive.h"

////////////////////////////////////////////////////////////////////////////////

void del_tree(void* t)
{
    ao_tree_delete((ao_tree)t);
}

// Raw Scheme functions
SCM scm_wrap_tree_ = NULL;
SCM scm_unwrap_tree_= NULL;
SCM scm_tree_p_= NULL;

SCM scm_vec3_ = NULL;

// Scheme-flavored bindings
SCM scm_wrap_tree(SCM ptr)      { return scm_call_1(scm_wrap_tree_, ptr); }
SCM scm_unwrap_tree(SCM ptr)    { return scm_call_1(scm_unwrap_tree_, ptr); }
SCM scm_tree_p(SCM ptr)         { return scm_call_1(scm_tree_p_, ptr); }

// C-flavored bindings
bool scm_is_tree(SCM t) { return scm_is_true(scm_tree_p(t)); }
ao_tree scm_to_tree(SCM t)
{
    return (ao_tree)scm_to_pointer(scm_unwrap_tree(t));
}

SCM scm_from_tree(ao_tree t)
{
    return scm_wrap_tree(scm_from_pointer(t, del_tree));
}

SCM scm_vec3(float x, float y, float z)
{
    return scm_call_3(scm_vec3_, scm_from_double(x),
                                 scm_from_double(y),
                                 scm_from_double(z));
}


////////////////////////////////////////////////////////////////////////////////

SCM scm_number_to_tree(SCM n)
{
    SCM_ASSERT_TYPE(scm_is_number(n), n, 0, "scm_number_to_tree", "number");
    return scm_from_tree(ao_tree_const(scm_to_double(n)));
}

SCM scm_tree_equal_p(SCM a, SCM b)
{
    SCM_ASSERT_TYPE(scm_is_tree(a), a, 0, "scm_tree_equal_p", "tree");
    SCM_ASSERT_TYPE(scm_is_tree(b), b, 1, "scm_tree_equal_p", "tree");

    return ao_tree_eq(scm_to_tree(a), scm_to_tree(b))
        ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM scm_var()
{
    return scm_from_tree(ao_tree_var());
}

SCM scm_var_p(SCM a)
{
    SCM_ASSERT_TYPE(scm_is_tree(a), a, 0, "scm_var_p", "tree");
    return ao_tree_is_var(scm_to_tree(a)) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM scm_tree_id(SCM a)
{
    SCM_ASSERT_TYPE(scm_is_tree(a), a, 0, "scm_tree_id", "tree");
    return scm_from_uintptr_t((uintptr_t)ao_tree_id(scm_to_tree(a)));
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
        case 0: out = ao_tree_nonary(opcode);                   break;
        case 1: out = ao_tree_unary(opcode, scm_to_tree(a));    break;
        case 2: out = ao_tree_binary(opcode, scm_to_tree(a),
                                             scm_to_tree(b));   break;
        default: assert(false);
    }

    return scm_from_tree(out);
}

SCM scm_tree_eval_f(SCM t, SCM x, SCM y, SCM z)
{
    SCM_ASSERT_TYPE(scm_is_tree(t), t, 0, "scm_tree_eval_f", "tree");

    SCM_ASSERT_TYPE(scm_is_number(x) || scm_is_tree(t),
                    x, 1, "scm_tree_eval_f", "number");
    SCM_ASSERT_TYPE(scm_is_number(y) || scm_is_tree(t),
                    y, 2, "scm_tree_eval_f", "number");
    SCM_ASSERT_TYPE(scm_is_number(z) || scm_is_tree(t),
                    z, 3, "scm_tree_eval_f", "number");

    auto x_ = scm_is_number(x) ? ao_tree_const(scm_to_double(x))
                               : scm_to_tree(x);
    auto y_ = scm_is_number(y) ? ao_tree_const(scm_to_double(y))
                               : scm_to_tree(y);
    auto z_ = scm_is_number(z) ? ao_tree_const(scm_to_double(z))
                               : scm_to_tree(z);

    auto out = ao_tree_remap(scm_to_tree(t), x_, y_, z_);

    bool is_const = false;
    auto val = ao_tree_get_const(out, &is_const);

    return is_const ? scm_from_double(val) : scm_from_tree(out);
}

SCM scm_tree_eval_d(SCM t, SCM x, SCM y, SCM z)
{
    SCM_ASSERT_TYPE(scm_is_tree(t), t, 0, "scm_tree_eval_d", "tree");

    SCM_ASSERT_TYPE(scm_is_number(x), x, 1, "scm_tree_eval_d", "number");
    SCM_ASSERT_TYPE(scm_is_number(y), y, 2, "scm_tree_eval_d", "number");
    SCM_ASSERT_TYPE(scm_is_number(z), z, 3, "scm_tree_eval_d", "number");

    float x_ = scm_to_double(x);
    float y_ = scm_to_double(y);
    float z_ = scm_to_double(z);

    auto out = ao_tree_eval_d(scm_to_tree(t), {x_, y_, z_});

    return scm_vec3(out.x, out.y, out.z);
}

SCM scm_tree_eval_i(SCM t, SCM xmin, SCM xmax,
                           SCM ymin, SCM ymax,
                           SCM zmin, SCM zmax)
{
    SCM_ASSERT_TYPE(scm_is_tree(t), t, 0, "scm_tree_eval_f", "tree");

    SCM_ASSERT_TYPE(scm_is_number(xmin), xmin, 1, "scm_tree_eval_r", "number");
    SCM_ASSERT_TYPE(scm_is_number(xmax), xmax, 2, "scm_tree_eval_r", "number");
    SCM_ASSERT_TYPE(scm_is_number(ymin), ymin, 3, "scm_tree_eval_r", "number");
    SCM_ASSERT_TYPE(scm_is_number(ymax), ymax, 4, "scm_tree_eval_r", "number");
    SCM_ASSERT_TYPE(scm_is_number(zmin), zmin, 5, "scm_tree_eval_r", "number");
    SCM_ASSERT_TYPE(scm_is_number(zmax), zmax, 6, "scm_tree_eval_r", "number");

    float xmin_ = scm_to_double(xmin);
    float ymin_ = scm_to_double(ymin);
    float zmin_ = scm_to_double(zmin);

    float xmax_ = scm_to_double(xmax);
    float ymax_ = scm_to_double(ymax);
    float zmax_ = scm_to_double(zmax);

    ao_region3 r = {{xmin_, xmax_}, {ymin_, ymax_}, {zmin_, zmax_}};
    auto i = ao_tree_eval_r(scm_to_tree(t), r);

    return scm_cons(scm_from_double(i.lower), scm_from_double(i.upper));
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
    auto out = ao_tree_save_mesh(scm_to_tree(t),
            {{rs[0], rs[1]}, {rs[2], rs[3]}, {rs[4], rs[5]}},
            scm_to_double(res), filename);
    free(filename);

    return out ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM scm_shape_bounds(SCM t)
{
    SCM_ASSERT_TYPE(scm_is_tree(t), t, 0, "scm_tree_to_mesh", "tree");
    auto b = ao_tree_bounds(scm_to_tree(t));
    return scm_cons(scm_vec3(b.X.lower, b.Y.lower, b.Z.lower),
                    scm_vec3(b.X.upper, b.Y.upper, b.Z.upper));
}

void init_ao_kernel(void*)
{
    scm_c_eval_string(R"(
(use-modules (system foreign) (oop goops) (ao vec))

(define-wrapped-pointer-type
    tree-ptr tree-ptr? wrap-tree-ptr unwrap-tree-ptr
    (lambda (o p)
        (format p "#<tree-ptr 0x~x>"
        (pointer-address (unwrap-tree-ptr o)))))

(define-class <tree> (<number>) (ptr #:init-value #f #:init-keyword #:ptr))
(define (tree? t) (is-a? t <tree>))
(define (wrap-tree t) (make <tree> #:ptr (wrap-tree-ptr t)))
(define (unwrap-tree t) (unwrap-tree-ptr (slot-ref t 'ptr)))
)");

    // Extract tree wrapping and unwrapping from local environment
    scm_tree_p_ = scm_c_eval_string("tree?");
    scm_wrap_tree_ = scm_c_eval_string("wrap-tree");
    scm_unwrap_tree_ = scm_c_eval_string("unwrap-tree");

    // Extract vec3 constructor from local environment
    scm_vec3_ = scm_c_eval_string("vec3");

    // Inject all of our compiled functions into the module environment
    scm_c_define_gsubr("make-tree", 1, 2, 0, (void*)scm_tree);
    scm_c_define_gsubr("make-var", 0, 0, 0, (void*)scm_var);
    scm_c_define_gsubr("var?", 1, 0, 0, (void*)scm_var_p);
    scm_c_define_gsubr("tree-id", 1, 0, 0, (void*)scm_tree_id);
    scm_c_define_gsubr("number->tree", 1, 0, 0, (void*)scm_number_to_tree);
    scm_c_define_gsubr("tree-equal?", 2, 0, 0, (void*)scm_tree_equal_p);
    scm_c_define_gsubr("tree-eval-f", 4, 0, 0, (void*)scm_tree_eval_f);
    scm_c_define_gsubr("tree-eval-i", 7, 0, 0, (void*)scm_tree_eval_i);
    scm_c_define_gsubr("tree-eval-d", 4, 0, 0, (void*)scm_tree_eval_d);
    scm_c_define_gsubr("tree->mesh", 4, 0, 0, (void*)scm_tree_to_mesh);
    scm_c_define_gsubr("shape-bounds", 1, 0, 0, (void*)scm_shape_bounds);

    // Overload all of arithmetic operations with tree-based methods,
    // then add a handful of other useful functions to the module.
    scm_c_eval_string(R"(
(define (ensure-tree t)
  (cond ((tree? t) t)
        ((number? t) (number->tree t))
        (else (scm-error 'wrong-type-arg #f
               "Wrong argument ~A for ensure-tree" (list t) t))))

(define-macro (overload func sym) `(begin
  (define-method (,func (a <number>) (b <tree>)) (,func (ensure-tree a) b))
  (define-method (,func (a <tree>) (b <number>)) (,func a (ensure-tree b)))
  (define-method (,func (a <tree>) (b <tree>)) (make-tree ,sym a b)))
)

(overload + 'add)
(overload * 'mul)
(overload min 'min)
(overload max 'max)
(overload - 'sub)
(overload / 'div)
(overload atan 'atan2)
(overload modulo 'mod)

(define-method (+ (a <tree>)) a)
(define-method (* (a <tree>)) a)
(define-method (min (a <tree>)) a)
(define-method (max (a <tree>)) a)

(define-method (expt (a <tree>) (b <fraction>))
  (when (not (rational? b))
    (scm-error 'wrong-type-arg #f
        "RHS of exponentiation must be rational, not ~A"
        (list b) #f))
  (make-tree 'nth-root (make-tree 'pow a (numerator b))
                       (denominator b)))

(define-method (/ (a <tree>)) (make-tree 'recip a))
(define-method (- (a <tree>)) (make-tree 'neg a))
(define-method (sqrt (a <tree>)) (make-tree 'sqrt a))
(define-method (sin (a <tree>)) (make-tree 'sin a))
(define-method (cos (a <tree>)) (make-tree 'cos a))
(define-method (tan (a <tree>)) (make-tree 'tan a))
(define-method (asin (a <tree>)) (make-tree 'asin a))
(define-method (acos (a <tree>)) (make-tree 'acos a))
(define-method (exp (a <tree>)) (make-tree 'exp a))
(define-method (abs (a <tree>)) (make-tree 'abs a))
(define-method (atan (a <tree>)) (make-tree 'atan a))
(define-method (log (a <tree>)) (make-tree 'log a))
(define-method (constant (a <tree>)) (make-tree 'const-var a))
(define (square f) (if (tree? f) (make-tree 'square f) (* f f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method (tree-eval (a <tree>) (pt <vec3>))
    (tree-eval-f a (.x pt) (.y pt) (.z pt)))

(define-method (tree-eval (a <tree>) (lower <vec3>) (upper <vec3>))
    (tree-eval-i a (.x lower) (.x upper)
                   (.y lower) (.y upper)
                   (.z lower) (.z upper)))

(define-method (tree-derivs (a <tree>) (pt <vec3>))
    (tree-eval-d a (.x pt) (.y pt) (.z pt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (lambda-shape vars ...)
  (ensure-tree
    ((lambda vars ...) (make-tree 'var-x)
                       (make-tree 'var-y)
                       (make-tree 'var-z))))

(define-syntax-rule (define-shape (name . vars) body ...)
  (define name (lambda-shape vars body ...)))

(define-syntax-rule (remap-shape (tree . vars) x y z)
  (tree-eval tree #[(lambda-shape vars x)
                    (lambda-shape vars y)
                    (lambda-shape vars z)]))

(define-syntax sequence_
  (syntax-rules ()
   ((sequence_ a) a)
   ((sequence_ a (t . args) . rest)
      (sequence_ (t a . args) . rest))
))

(define-syntax-rule (v->list a)
  (call-with-values (lambda () a) (lambda (. args) args)))

(define-syntax vs->list
  (syntax-rules ()
    ((vs->list a) (list (v->list a)))
    ((vs->list a . rest) (cons (v->list a) (vs->list . rest)))))

(define-syntax-rule (values->list . vs)
    (apply append (vs->list . vs)))

(define-syntax-rule (values-from . vs)
  (apply values (values->list . vs)))

(define-syntax-rule (sequence a . args)
 (apply values (map (lambda (q) (sequence_ q . args))
                    (apply append (vs->list a)))))

;; These are "safe" bindings that can be used in the sandbox
(define ao-bindings '(square constant lambda-shape define-shape remap-shape
                      shape-bounds tree-eval tree-derivs sequence values-from
                      values->list
                      ao-bindings))
(eval (cons 'export ao-bindings) (interaction-environment))
 )");

    scm_c_export(
            "tree?", "tree", "wrap-tree", "unwrap-tree",
            "make-tree", "make-var", "var?", "tree-id", "number->tree",
            "tree-equal?", "tree-eval", "tree->mesh",
            NULL);
}

// This functions include keywords that are replaced at compile-time
// by the contents of scripts in separate .scm files
void init_ao_csg(void*)
{
    scm_c_eval_string(R"(AO_GUILE_CSG)");
}

void init_ao_transforms(void*)
{
    scm_c_eval_string(R"(AO_GUILE_TRANSFORMS)");
}

void init_ao_text(void*)
{
    scm_c_eval_string(R"(AO_GUILE_TEXT)");
}

void init_ao_shapes(void*)
{
    scm_c_eval_string(R"(AO_GUILE_SHAPES)");
}

void init_ao_vec(void*)
{
    scm_c_eval_string(R"(AO_GUILE_VEC)");
}

void init_ao_sandbox(void*)
{
    scm_c_eval_string(R"(AO_GUILE_SANDBOX)");
}

void scm_init_ao_modules()
{
    // Listed in order of dependencies
    scm_c_define_module("ao vec", init_ao_vec, NULL);
    scm_c_define_module("ao kernel", init_ao_kernel, NULL);
    scm_c_define_module("ao csg", init_ao_csg, NULL);
    scm_c_define_module("ao transforms", init_ao_transforms, NULL);
    scm_c_define_module("ao shapes", init_ao_shapes, NULL);
    scm_c_define_module("ao text", init_ao_text, NULL);
    scm_c_define_module("ao sandbox", init_ao_sandbox, NULL);
}
