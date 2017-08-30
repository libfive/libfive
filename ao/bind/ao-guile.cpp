#include <cstdlib>
#include <cassert>

#include "ao-guile.h"
#include "ao/ao.h"

////////////////////////////////////////////////////////////////////////////////

void del_tree(void* t)
{
    auto s = (scm_ao_tree*)t;

    ao_tree_delete(s->t);
    delete [] s->tree_pos;
    delete s;
}

// Raw Scheme functions
SCM scm_wrap_tree_ = NULL;
SCM scm_unwrap_tree_= NULL;
SCM scm_tree_p_= NULL;

// Scheme-flavored bindings
SCM scm_wrap_tree(SCM ptr)      { return scm_call_1(scm_wrap_tree_, ptr); }
SCM scm_unwrap_tree(SCM ptr)    { return scm_call_1(scm_unwrap_tree_, ptr); }
SCM scm_tree_p(SCM ptr)         { return scm_call_1(scm_tree_p_, ptr); }

// C-flavored bindings
bool scm_is_tree(SCM t) { return scm_is_true(scm_tree_p(t)); }
scm_ao_tree* scm_to_tree(SCM t)
{
    return (scm_ao_tree*)scm_to_pointer(scm_unwrap_tree(t));
}

SCM scm_from_tree(ao_tree t)
{
    auto s = new scm_ao_tree;

    s->t = t;
    s->text_pos[0] = -1;
    s->text_pos[1] = -1;
    s->tree_pos = nullptr;
    s->value = 0.0f;

    return scm_wrap_tree(scm_from_pointer(s, del_tree));
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

    return ao_tree_eq(scm_to_tree(a)->t, scm_to_tree(b)->t)
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
        case 0: out = ao_tree_nonary(opcode);                   break;
        case 1: out = ao_tree_unary(opcode, scm_to_tree(a)->t); break;
        case 2: out = ao_tree_binary(opcode, scm_to_tree(a)->t,
                                     scm_to_tree(b)->t);        break;
        default: assert(false);
    }

    return scm_from_tree(out);
}

SCM scm_tree_eval(SCM t, SCM x, SCM y, SCM z)
{
    SCM_ASSERT_TYPE(scm_is_tree(t), t, 0, "scm_tree_eval", "tree");

    SCM_ASSERT_TYPE(scm_is_number(x) || scm_is_tree(t),
                    x, 1, "scm_tree_eval", "number");
    SCM_ASSERT_TYPE(scm_is_number(y) || scm_is_tree(t),
                    y, 2, "scm_tree_eval", "number");
    SCM_ASSERT_TYPE(scm_is_number(z) || scm_is_tree(t),
                    z, 3, "scm_tree_eval", "number");

    auto x_ = scm_is_number(x) ? ao_tree_const(scm_to_double(x))
                               : scm_to_tree(x)->t;
    auto y_ = scm_is_number(y) ? ao_tree_const(scm_to_double(y))
                               : scm_to_tree(y)->t;
    auto z_ = scm_is_number(z) ? ao_tree_const(scm_to_double(z))
                               : scm_to_tree(z)->t;

    auto out = ao_tree_remap(scm_to_tree(t)->t, x_, y_, z_);

    bool is_const = false;
    auto val = ao_tree_get_const(out, &is_const);

    return is_const ? scm_from_double(val) : scm_from_tree(out);
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
    auto out = ao_tree_save_mesh(scm_to_tree(t)->t,
            {{rs[0], rs[1]}, {rs[2], rs[3]}, {rs[4], rs[5]}},
            scm_to_double(res), filename);
    free(filename);

    return out ? SCM_BOOL_T : SCM_BOOL_F;
}

void init_ao_kernel(void*)
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
(use-modules (srfi srfi-1))

(define (make-commutative func sym default)
    (lambda (. args)
        (define (folder default args)
            (fold (lambda (e p) (make-tree sym e p)) default args))
        (cond ((not (any tree? args)) (apply func args))
              ((nil? default)
                (when (= 0 (length args))
                    (scm-error 'wrong-number-of-args #f
                         "Wrong number of arguments to ~A"
                         (list sym) #f))
                (folder (car args) (cdr args)))
                (else (folder default args)))))

(define (make-semicommutative func sym inverse)
    (lambda (. args)
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
(overload-unary sin 'sin)
(overload-unary cos 'cos)
(overload-unary tan 'tan)
(overload-unary asin 'asin)
(overload-unary acos 'acos)
(overload-unary exp 'exp)

(define (square f)
    (if (number? f) (* f f) (make-tree 'square f)))

(define $abs abs)
(define* (abs a)
  (if (number? a) ($abs a) (max a (- a))))

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

(define-syntax-rule (lambda-shape vars ...)
  ((lambda vars ...) (make-tree 'var-x) (make-tree 'var-y) (make-tree 'var-z)))

(define-syntax-rule (define-shape (name . vars) body ...)
  (define name (lambda-shape vars body ...)))

(define-syntax-rule (remap-shape (tree . vars) x y z)
  (tree-eval tree (lambda-shape vars x)
                  (lambda-shape vars y)
                  (lambda-shape vars z)))
(define (.x pt) (if (list? pt) (car pt)   (vector-ref pt 0)))
(define (.y pt) (if (list? pt) (cadr pt)  (vector-ref pt 1)))
(define (.z pt) (if (list? pt) (caddr pt) (vector-ref pt 2)))

;; These are "safe" bindings that can be used in the sandbox
(define ao-bindings '(+ * min max - / .x .y .z
                      sqrt abs sin cos tan asin acos exp square atan expt mod
                      lambda-shape define-shape ao-bindings remap-shape))
(eval (cons 'export! ao-bindings) (interaction-environment))
 )");

    scm_c_export(
            "tree?", "tree", "wrap-tree", "unwrap-tree",
            "make-tree", "number->tree", "tree-equal?", "tree-eval",
            "tree->mesh",
            NULL);
}

void init_ao_csg(void*)
{
    scm_c_eval_string(R"(
(use-modules (ao kernel))

(define-public (union . args)
    "union a [b [c [...]]]
    Returns the union of any number of shapes"
    (apply min args))

(define-public (intersection . args)
    "intersection a [b [c [...]]]
    Returns the intersection of any number of shapes"
    (apply max args))

(define-public (inverse a)
    "inverse a
    Returns a shape that's the inverse of the input shape"
    (- a))

(define-public (difference a . bs)
    "difference a b [c [d [...]]]
    Subtracts any number of shapes from the first argument"
    (intersection a (inverse (apply union bs))))

(define-public (offset s o)
    "offset shape o
    Expand or contract a given shape by an offset"
    (+ s o))

(define-public (clearance a b o)
    "clearance a b o
    Expands shape b by the given offset then subtracts it from shape a"
    (difference a (offset b o)))

(define-public (shell shape o)
    "shell shape o
    Returns a shell of a shape with the given offset"
    (clearance shape shape o))

(define-public (blend a b m)
    "blend a b m
    Blends two shapes by the given amount"
    (union a b (- (+ (sqrt (abs a)) (sqrt (abs b))) m)))

(define-public (morph a b m)
    "morph a b m
    Morphs between two shapes.
    m = 0 produces a, m = 1 produces b"
    (+ (* (a x y z) (- 1 m)) (* (b x y z) m)))
)");
}

void init_ao_transforms(void*)
{
    scm_c_eval_string(R"(
(use-modules (ao kernel))

(define-public (move shape delta)
    "move shape #(dx dy [dz])
    Moves the given shape in 2D or 3D space"
    (remap-shape (shape x y z)
        (- x (.x delta))
        (- y (.y delta))
        (- z (catch #t (lambda ()(.z delta)) (lambda (. _) 0)))))

(define-public (reflect-xy shape)
    "reflect-xy shape
    Moves the given shape across the plane Y=X"
    (remap-shape (shape x y z) y x z))

(define-public (reflect-yz shape)
    "reflect-xy shape
    Moves the given shape across the plane Y=Z"
    (remap-shape (shape x y z) x z y))
)");
}

void init_ao_shapes(void*)
{
    scm_c_eval_string(R"(
(use-modules (ao kernel) (ao csg) (ao transforms))

(define-public (circle center r)
    "circle #(x y) r"
    (move (lambda-shape (x y z) (- (sqrt (+ (square x) (square y))) r))
        center))

(define-public (sphere center r)
    "sphere #(x y z) r"
    (move (lambda-shape (x y z) (- (sqrt (+ (square x) (square y) (square z))) r))
        center))

(define-public (rectangle a b)
    "rectangle '(xmin ymin) '(xmax ymax)
    Constructs a rectangle from its corners"
    (when (< (.x b) (.x a)) (error "xmax must be >= xmin"))
    (when (< (.y b) (.y a)) (error "ymax must be >= ymin"))
    (lambda-shape (x y z)
        (max (- (.x a) x) (- x (.x b)) (- (.y a) y) (- y (.y b)))))

(define-public (box a b)
    "box #(xmin ymin zmin) #(xmax ymax zmax)\\"
    (extrude-z (rectangle a b) (.z a) (.z b)))

(define-public (extrude-z shape zmin zmax)
    "extrude-z shape zmin zmax
    Extrudes a 2D shape between za and zb"
    (when (< zmax zmin) (error "zmax must be >= zmin"))
    (max shape (lambda-shape (x y z) (max (- zmin z) (- z zmax)))))

(define-public (cylinder center r zmin zmax)
    "cylinder '(x0 y0) r zmin zmax
    A cylinder (oriented along the Z axis) "
    (extrude-z (circle center r) zmin zmax))
)");
}

void scm_init_ao_kernel_module()
{
    scm_c_define_module("ao kernel", init_ao_kernel, NULL);
    scm_c_define_module("ao csg", init_ao_csg, NULL);
    scm_c_define_module("ao transforms", init_ao_transforms, NULL);
    scm_c_define_module("ao shapes", init_ao_shapes, NULL);
}
