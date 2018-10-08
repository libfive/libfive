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
#include <clocale>

#include "libfive-guile.h"
#include "libfive.h"

////////////////////////////////////////////////////////////////////////////////

struct scm_shape {
    libfive_tree tree;
    SCM meta;
};

void del_shape(void* s)
{
    libfive_tree_delete(((scm_shape*)s)->tree);
    scm_gc_unprotect_object(((scm_shape*)s)->meta);
    delete (scm_shape*)s;
}

// Raw Scheme functions
SCM scm_wrap_shape_ = NULL;
SCM scm_unwrap_shape_= NULL;
SCM scm_shape_p_= NULL;

SCM scm_vec3_ = NULL;

// Scheme-flavored bindings
SCM scm_wrap_shape(SCM ptr)      { return scm_call_1(scm_wrap_shape_, ptr); }
SCM scm_unwrap_shape(SCM ptr)    { return scm_call_1(scm_unwrap_shape_, ptr); }
SCM scm_shape_p(SCM ptr)         { return scm_call_1(scm_shape_p_, ptr); }

// C-flavored bindings
bool scm_is_shape(SCM t) { return scm_is_true(scm_shape_p(t)); }
libfive_tree scm_get_tree(SCM t)
{
    return ((scm_shape*)scm_to_pointer(scm_unwrap_shape(t)))->tree;
}

SCM scm_from_tree(libfive_tree t)
{
    static SCM hash_new =  scm_c_eval_string("make-hash-table");

    auto s = new scm_shape;
    s->tree = t;
    s->meta = scm_call_0(hash_new);
    scm_gc_protect_object(s->meta);

    return scm_wrap_shape(scm_from_pointer(s, del_shape));
}

SCM scm_vec3(float x, float y, float z)
{
    return scm_call_3(scm_vec3_, scm_from_double(x),
                                 scm_from_double(y),
                                 scm_from_double(z));
}


////////////////////////////////////////////////////////////////////////////////

SCM scm_number_to_shape(SCM n)
{
    SCM_ASSERT_TYPE(scm_is_number(n), n, 0, "scm_number_to_shape", "number");
    return scm_from_tree(libfive_tree_const(scm_to_double(n)));
}

SCM scm_shape_equal_p(SCM a, SCM b)
{
    SCM_ASSERT_TYPE(scm_is_shape(a), a, 0, "scm_shape_equal_p", "shape");
    SCM_ASSERT_TYPE(scm_is_shape(b), b, 1, "scm_shape_equal_p", "shape");

    return libfive_tree_eq(scm_get_tree(a), scm_get_tree(b))
        ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM scm_var()
{
    return scm_from_tree(libfive_tree_var());
}

SCM scm_var_p(SCM a)
{
    SCM_ASSERT_TYPE(scm_is_shape(a), a, 0, "scm_var_p", "shape");
    return libfive_tree_is_var(scm_get_tree(a)) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM scm_shape_constant_vars(SCM a)
{
    SCM_ASSERT_TYPE(scm_is_shape(a), a, 0, "scm_shape_constant_vars", "shape");
    return scm_from_tree(libfive_tree_constant_vars(scm_get_tree(a)));
}

SCM scm_shape_tree_id(SCM a)
{
    SCM_ASSERT_TYPE(scm_is_shape(a), a, 0, "scm_shape_tree_id", "shape");
    return scm_from_uintptr_t((uintptr_t)libfive_tree_id(scm_get_tree(a)));
}

SCM scm_shape(SCM op, SCM a, SCM b)
{
    SCM_ASSERT_TYPE(scm_is_symbol(op), op, 0, "scm_shape", "symbol");
    static const auto scm_shape_str = scm_from_locale_string("scm_shape");

    auto str = scm_to_locale_string(scm_symbol_to_string(op));
    auto opcode = libfive_opcode_enum(str);
    free(str);

    SCM_ASSERT_TYPE(opcode != -1, op, 0, "scm_shape", "opcode");
    auto args = libfive_opcode_args(opcode);

    if (args == 0)
    {
        if (a != SCM_UNDEFINED || b != SCM_UNDEFINED)
        {
            scm_wrong_num_args(scm_shape_str);
            return SCM_UNDEFINED;
        }
    }
    else if (args == 1)
    {
        SCM_ASSERT_TYPE(scm_is_number(a) || scm_is_shape(a),
                        a, 1, "scm_shape", "number or shape");
        if (b != SCM_UNDEFINED)
        {
            scm_wrong_num_args(scm_shape_str);
            return SCM_UNDEFINED;
        }
        if (scm_is_number(a)) { a = scm_number_to_shape(a); }
    }
    else if (args == 2)
    {
        if (a == SCM_UNDEFINED || b == SCM_UNDEFINED)
        {
            scm_wrong_num_args(scm_shape_str);
            return SCM_UNDEFINED;
        }
        SCM_ASSERT_TYPE(scm_is_number(a) || scm_is_shape(a),
                        a, 1, "scm_shape", "number or shape");
        SCM_ASSERT_TYPE(scm_is_number(b) || scm_is_shape(b),
                        b, 2, "scm_shape", "number or shape");
        if (scm_is_number(a)) { a = scm_number_to_shape(a); }
        if (scm_is_number(b)) { b = scm_number_to_shape(b); }
    }

    libfive_tree out = nullptr;
    switch (args)
    {
        case 0: out = libfive_tree_nonary(opcode);                  break;
        case 1: out = libfive_tree_unary(opcode, scm_get_tree(a));  break;
        case 2: out = libfive_tree_binary(opcode, scm_get_tree(a),
                                          scm_get_tree(b));   break;
        default: assert(false);
    }

    return scm_from_tree(out);
}

SCM scm_shape_eval_f(SCM t, SCM x, SCM y, SCM z)
{
    SCM_ASSERT_TYPE(scm_is_shape(t), t, 0, "scm_shape_eval_f", "shape");

    SCM_ASSERT_TYPE(scm_is_number(x),
                    x, 1, "scm_shape_eval_f", "number");
    SCM_ASSERT_TYPE(scm_is_number(y),
                    y, 2, "scm_shape_eval_f", "number");
    SCM_ASSERT_TYPE(scm_is_number(z),
                    z, 3, "scm_shape_eval_f", "number");

    float x_ = scm_to_double(x);
    float y_ = scm_to_double(y);
    float z_ = scm_to_double(z);

    auto val = libfive_tree_eval_f(scm_get_tree(t), {x_, y_, z_});
    return scm_from_double(val);
}

SCM scm_shape_remap(SCM t, SCM x, SCM y, SCM z)
{
    SCM_ASSERT_TYPE(scm_is_shape(t), t, 0, "scm_shape_eval_f", "shape");

    SCM_ASSERT_TYPE(scm_is_number(x) || scm_is_shape(t),
                    x, 1, "scm_shape_remap", "number");
    SCM_ASSERT_TYPE(scm_is_number(y) || scm_is_shape(t),
                    y, 2, "scm_shape_remap", "number");
    SCM_ASSERT_TYPE(scm_is_number(z) || scm_is_shape(t),
                    z, 3, "scm_shape_remap", "number");

    auto x_ = scm_is_number(x) ? libfive_tree_const(scm_to_double(x))
                               : scm_get_tree(x);
    auto y_ = scm_is_number(y) ? libfive_tree_const(scm_to_double(y))
                               : scm_get_tree(y);
    auto z_ = scm_is_number(z) ? libfive_tree_const(scm_to_double(z))
                               : scm_get_tree(z);

    auto out = libfive_tree_remap(scm_get_tree(t), x_, y_, z_);
    return scm_from_tree(out);
}

SCM scm_shape_eval_d(SCM t, SCM x, SCM y, SCM z)
{
    SCM_ASSERT_TYPE(scm_is_shape(t), t, 0, "scm_shape_eval_d", "shape");

    SCM_ASSERT_TYPE(scm_is_number(x), x, 1, "scm_shape_eval_d", "number");
    SCM_ASSERT_TYPE(scm_is_number(y), y, 2, "scm_shape_eval_d", "number");
    SCM_ASSERT_TYPE(scm_is_number(z), z, 3, "scm_shape_eval_d", "number");

    float x_ = scm_to_double(x);
    float y_ = scm_to_double(y);
    float z_ = scm_to_double(z);

    auto out = libfive_tree_eval_d(scm_get_tree(t), {x_, y_, z_});

    return scm_vec3(out.x, out.y, out.z);
}

SCM scm_shape_eval_i(SCM t, SCM xmin, SCM xmax,
                            SCM ymin, SCM ymax,
                            SCM zmin, SCM zmax)
{
    SCM_ASSERT_TYPE(scm_is_shape(t), t, 0, "scm_shape_eval_f", "shape");

    SCM_ASSERT_TYPE(scm_is_number(xmin), xmin, 1, "scm_shape_eval_r", "number");
    SCM_ASSERT_TYPE(scm_is_number(xmax), xmax, 2, "scm_shape_eval_r", "number");
    SCM_ASSERT_TYPE(scm_is_number(ymin), ymin, 3, "scm_shape_eval_r", "number");
    SCM_ASSERT_TYPE(scm_is_number(ymax), ymax, 4, "scm_shape_eval_r", "number");
    SCM_ASSERT_TYPE(scm_is_number(zmin), zmin, 5, "scm_shape_eval_r", "number");
    SCM_ASSERT_TYPE(scm_is_number(zmax), zmax, 6, "scm_shape_eval_r", "number");

    float xmin_ = scm_to_double(xmin);
    float ymin_ = scm_to_double(ymin);
    float zmin_ = scm_to_double(zmin);

    float xmax_ = scm_to_double(xmax);
    float ymax_ = scm_to_double(ymax);
    float zmax_ = scm_to_double(zmax);

    libfive_region3 r = {{xmin_, xmax_}, {ymin_, ymax_}, {zmin_, zmax_}};
    auto i = libfive_tree_eval_r(scm_get_tree(t), r);

    return scm_cons(scm_from_double(i.lower), scm_from_double(i.upper));
}

SCM scm_shape_to_mesh(SCM t, SCM f, SCM res, SCM region)
{
    SCM_ASSERT_TYPE(scm_is_shape(t), t, 0, "scm_shape_to_mesh", "shape");
    SCM_ASSERT_TYPE(scm_is_string(f), f, 1, "scm_shape_to_mesh", "string");
    SCM_ASSERT_TYPE(scm_is_number(res), res, 2, "scm_shape_to_mesh", "number");
    SCM_ASSERT_TYPE(scm_is_true(scm_list_p(region)) &&
                    scm_to_int(scm_length(region)) == 3, region, 3,
                    "scm_shape_to_mesh", "three-element list");

    float rs[6];
    for (int i=0; i < 3; ++i)
    {
        SCM_ASSERT_TYPE(scm_is_pair(scm_car(region)) &&
                scm_is_number(scm_caar(region)) &&
                scm_is_number(scm_cdar(region)),
                scm_car(region), 3,
                "scm_shape_to_mesh", "pair of numbers");
        rs[i*2 + 0] = scm_to_double(scm_caar(region));
        rs[i*2 + 1] = scm_to_double(scm_cdar(region));
        region = scm_cdr(region);
    }

    auto filename = scm_to_locale_string(f);
    auto out = libfive_tree_save_mesh(scm_get_tree(t),
            {{rs[0], rs[1]}, {rs[2], rs[3]}, {rs[4], rs[5]}},
            scm_to_double(res), filename);
    free(filename);

    return out ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM scm_shape_to_string(SCM t)
{
    SCM_ASSERT_TYPE(scm_is_shape(t), t, 0, "scm_shape_to_string", "shape");
    auto c = libfive_tree_print(scm_get_tree(t));
    auto out = scm_from_locale_string(c);
    free(c);
    return out;
}

SCM scm_shape_get_meta(SCM t)
{
    SCM_ASSERT_TYPE(scm_is_shape(t), t, 0, "scm_shape_to_string", "shape");
    return ((struct scm_shape*)scm_to_pointer(scm_unwrap_shape(t)))->meta;
}

SCM scm_shape_save(SCM t, SCM filename)
{
    SCM_ASSERT_TYPE(scm_is_shape(t), t, 0, "scm_shape_save", "shape");
    SCM_ASSERT_TYPE(scm_is_string(filename), t, 1, "scm_shape_save", "string");
    auto f = scm_to_locale_string(filename);
    auto result = libfive_tree_save(scm_get_tree(t), f);
    free(f);
    return result ? SCM_UNSPECIFIED : SCM_BOOL_F;
}

SCM scm_shape_load(SCM filename)
{
    SCM_ASSERT_TYPE(scm_is_string(filename), filename, 0,
                    "scm_shape_load", "string");
    auto f = scm_to_locale_string(filename);
    auto result = libfive_tree_load(f);
    free(f);
    return result ? scm_from_tree(result) : SCM_BOOL_F;
}

void init_libfive_kernel(void*)
{
    scm_c_eval_string(R"(
(use-modules (system foreign) (oop goops) (libfive vec))

(define-wrapped-pointer-type
    shape-ptr shape-ptr? wrap-shape-ptr unwrap-shape-ptr
    (lambda (o p)
        (format p "#<shape-ptr 0x~x>"
        (pointer-address (unwrap-shape-ptr o)))))

(define-class <shape> (<number>) (ptr #:init-value #f #:init-keyword #:ptr))
(define (shape? t) (is-a? t <shape>))
(define (wrap-shape t) (make <shape> #:ptr (wrap-shape-ptr t)))
(define (unwrap-shape t) (unwrap-shape-ptr (slot-ref t 'ptr)))
)");

    // Extract shape wrapping and unwrapping from local environment
    scm_shape_p_ = scm_c_eval_string("shape?");
    scm_wrap_shape_ = scm_c_eval_string("wrap-shape");
    scm_unwrap_shape_ = scm_c_eval_string("unwrap-shape");

    // Extract vec3 constructor from local environment
    scm_vec3_ = scm_c_eval_string("vec3");

    // Inject all of our compiled functions into the module environment
    scm_c_define_gsubr("make-shape", 1, 2, 0, (void*)scm_shape);
    scm_c_define_gsubr("make-var", 0, 0, 0, (void*)scm_var);
    scm_c_define_gsubr("var?", 1, 0, 0, (void*)scm_var_p);
    scm_c_define_gsubr("constant", 1, 0, 0, (void*)scm_shape_constant_vars);
    scm_c_define_gsubr("shape-tree-id", 1, 0, 0, (void*)scm_shape_tree_id);
    scm_c_define_gsubr("number->shape", 1, 0, 0, (void*)scm_number_to_shape);
    scm_c_define_gsubr("shape-equal?", 2, 0, 0, (void*)scm_shape_equal_p);
    scm_c_define_gsubr("_shape-eval-f", 4, 0, 0, (void*)scm_shape_eval_f);
    scm_c_define_gsubr("_shape-remap", 4, 0, 0, (void*)scm_shape_remap);
    scm_c_define_gsubr("_shape-eval-i", 7, 0, 0, (void*)scm_shape_eval_i);
    scm_c_define_gsubr("_shape-eval-d", 4, 0, 0, (void*)scm_shape_eval_d);
    scm_c_define_gsubr("shape->mesh", 4, 0, 0, (void*)scm_shape_to_mesh);
    scm_c_define_gsubr("shape->string", 1, 0, 0, (void*)scm_shape_to_string);
    scm_c_define_gsubr("shape-meta", 1, 0, 0, (void*)scm_shape_get_meta);
    scm_c_define_gsubr("save-shape", 2, 0, 0, (void*)scm_shape_save);
    scm_c_define_gsubr("load-shape", 1, 0, 0, (void*)scm_shape_load);

    // Overload all of arithmetic operations with shape-based methods,
    // then add a handful of other useful functions to the module.
    scm_c_eval_string(R"(
(define (ensure-shape t)
  (cond ((shape? t) t)
        ((number? t) (number->shape t))
        (else (scm-error 'wrong-type-arg ensure-shape
               "Wrong argument ~A (should be number or shape) " (list t) (list t)))))

(define-macro (overload func sym) `(begin
  (define-method (,func (a <number>) (b <shape>)) (,func (ensure-shape a) b))
  (define-method (,func (a <shape>) (b <number>)) (,func a (ensure-shape b)))
  (define-method (,func (a <shape>) (b <shape>)) (make-shape ,sym a b)))
)

(overload + 'add)
(overload * 'mul)
(overload min 'min)
(overload max 'max)
(overload - 'sub)
(overload / 'div)
(overload atan 'atan2)
(overload modulo 'mod)
(overload nan-fill 'nanfill)

(overload compare 'compare)
(define-method (compare (a <number>) (b <number>))
  (cond ((< a b) -1)
        ((> a b)  1)
        ( else    0)))

(define-method (+ (a <shape>)) a)
(define-method (* (a <shape>)) a)
(define-method (min (a <shape>)) a)
(define-method (max (a <shape>)) a)

(define-method (expt (a <shape>) (b <fraction>))
  (make-shape 'nth-root (make-shape 'pow a (numerator b))
                        (denominator b)))
(define-method (expt (a <shape>) (b <number>))
  (scm-error 'wrong-type-arg 'expt
      "RHS of exponentiation must be rational, not ~A"
      (list b) (list b)))

(define-method (/ (a <shape>)) (make-shape 'recip a))
(define-method (- (a <shape>)) (make-shape 'neg a))
(define-method (sqrt (a <shape>)) (make-shape 'sqrt a))
(define-method (sin (a <shape>)) (make-shape 'sin a))
(define-method (cos (a <shape>)) (make-shape 'cos a))
(define-method (tan (a <shape>)) (make-shape 'tan a))
(define-method (asin (a <shape>)) (make-shape 'asin a))
(define-method (acos (a <shape>)) (make-shape 'acos a))
(define-method (exp (a <shape>)) (make-shape 'exp a))
(define-method (abs (a <shape>)) (make-shape 'abs a))
(define-method (atan (a <shape>)) (make-shape 'atan a))
(define-method (log (a <shape>)) (make-shape 'log a))
(define (square f) (if (shape? f) (make-shape 'square f) (* f f)))

(define-method (nan-fill (a <number>) (b <number>)) (if (nan? a) b a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method (shape-eval (a <shape>) (pt <vec3>))
    (_shape-eval-f a (.x pt) (.y pt) (.z pt)))

(define-method (shape-eval (a <shape>) (lower <vec3>) (upper <vec3>))
    (_shape-eval-i a (.x lower) (.x upper)
                   (.y lower) (.y upper)
                   (.z lower) (.z upper)))

(define-method (shape-derivs (a <shape>) (pt <vec3>))
    (_shape-eval-d a (.x pt) (.y pt) (.z pt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (lambda-shape vars ...)
  (ensure-shape
    ((lambda vars ...) (make-shape 'var-x)
                       (make-shape 'var-y)
                       (make-shape 'var-z))))

(define-syntax-rule (define-shape (name . vars) body ...)
  (define name (lambda-shape vars body ...)))

(define-syntax remap-shape
  (syntax-rules ()
    ((remap-shape (shape . vars) x y z)
        (remap-shape shape vars x y z))
    ((remap-shape shape (. vars) x y z)
        (_shape-remap shape (lambda-shape vars x)
                            (lambda-shape vars y)
                            (lambda-shape vars z)))))

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
(define libfive-bindings '(
    square
    nan-fill
    constant
    compare
    lambda-shape
    define-shape
    remap-shape
    shape-find-bounds
    shape->string
    shape-eval
    shape-derivs
    sequence
    values-from
    values->list
    libfive-bindings))
(eval (cons 'export libfive-bindings) (interaction-environment))
 )");

    scm_c_export(
            "shape?", "<shape>", "wrap-shape", "unwrap-shape",
            "make-shape", "make-var", "var?", "shape-tree-id", "number->shape",
            "shape-equal?", "shape-eval", "shape->mesh", "shape-meta",
            "save-shape", "load-shape",
            NULL);
}

// This functions include keywords that are replaced at compile-time
// by the contents of scripts in separate .scm files
void init_libfive_csg(void*)
{
    scm_c_eval_string(R"(LIBFIVE_GUILE_CSG)");
}

void init_libfive_transforms(void*)
{
    scm_c_eval_string(R"(LIBFIVE_GUILE_TRANSFORMS)");
}

void init_libfive_text(void*)
{
    scm_c_eval_string(R"(LIBFIVE_GUILE_TEXT)");
}

void init_libfive_shapes(void*)
{
    scm_c_eval_string(R"(LIBFIVE_GUILE_SHAPES)");
}

void init_libfive_vec(void*)
{
    scm_c_eval_string(R"(LIBFIVE_GUILE_VEC)");
}

void init_libfive_sandbox(void*)
{
    scm_c_eval_string(R"(LIBFIVE_GUILE_SANDBOX)");
}

void init_libfive_util(void*)
{
    scm_c_eval_string(R"(LIBFIVE_GUILE_UTIL)");
}

void scm_init_libfive_modules()
{
    const auto locale = std::setlocale(LC_CTYPE, NULL);
    std::setlocale(LC_CTYPE, "en_US.UTF-8");

    // Listed in order of dependencies
    scm_c_define_module("libfive vec", init_libfive_vec, NULL);
    scm_c_define_module("libfive util", init_libfive_util, NULL);
    scm_c_define_module("libfive kernel", init_libfive_kernel, NULL);
    scm_c_define_module("libfive csg", init_libfive_csg, NULL);
    scm_c_define_module("libfive transforms", init_libfive_transforms, NULL);
    scm_c_define_module("libfive shapes", init_libfive_shapes, NULL);
    scm_c_define_module("libfive text", init_libfive_text, NULL);
    scm_c_define_module("libfive sandbox", init_libfive_sandbox, NULL);

    std::setlocale(LC_CTYPE, locale);
}
