#include <iostream>
#include <string>

#include "ao/bind/scm.hpp"

#include "ao/core/store.hpp"
#include "ao/core/tree.hpp"
#include "ao/core/opcode.hpp"
#include "ao/core/token.hpp"

#include "ao/ui/window.hpp"

////////////////////////////////////////////////////////////////////////////////

static Opcode symbol_to_opcode(SCM sym)
{
    char* str_ptr = scm_to_locale_string(scm_symbol_to_string(sym));
    std::string str(str_ptr);
    free(str_ptr);

    if (str == "add")           return OP_ADD;
    else if (str == "mul")      return OP_MUL;
    else if (str == "min")      return OP_MIN;
    else if (str == "max")      return OP_MAX;
    else if (str == "sub")      return OP_SUB;
    else if (str == "div")      return OP_DIV;
    else if (str == "sqrt")     return OP_SQRT;
    else if (str == "neg")      return OP_NEG;
    else                        return INVALID;
}

////////////////////////////////////////////////////////////////////////////////

template <class T>
std::string class_name();

template <>
std::string class_name<Tree>() { return "Tree"; }

template <>
std::string class_name<Token>() { return "Token"; }

template <>
std::string class_name<Store>() { return "Store"; }

template <class T>
static SCM tag_ptr(T* p, void (*finalizer)(void*)=NULL)
{
    return scm_cons(scm_string_to_symbol(
                        scm_from_locale_string(class_name<T>().c_str())),
                    scm_from_pointer(p, finalizer));
}

template <class T>
static T* untag_ptr(SCM ptr)
{
    char* str_ptr = scm_to_locale_string(scm_symbol_to_string(scm_car(ptr)));
    std::string str(str_ptr);
    free(str_ptr);

    if (str != class_name<T>())
    {
        scm_wrong_type_arg("untag_ptr", 1, ptr);
    }

    return static_cast<T*>(scm_to_pointer(scm_cdr(ptr)));
}

////////////////////////////////////////////////////////////////////////////////

void store_delete(void* ptr)
{
    delete static_cast<Store*>(ptr);
}

SCM store_new()
{
    return tag_ptr(new Store, store_delete);
}

////////////////////////////////////////////////////////////////////////////////

SCM token_x(SCM s)
{
    return tag_ptr(untag_ptr<Store>(s)->X());
}

SCM token_y(SCM s)
{
    return tag_ptr(untag_ptr<Store>(s)->Y());
}

SCM token_z(SCM s)
{
    return tag_ptr(untag_ptr<Store>(s)->Z());
}

////////////////////////////////////////////////////////////////////////////////

SCM token_const(SCM store, SCM v)
{
    auto s = untag_ptr<Store>(store);
    double value = scm_to_double(v);
    return tag_ptr(s->constant(value));
}

SCM token_op(SCM store, SCM op_sym, SCM args)
{
    auto s = untag_ptr<Store>(store);
    Opcode op = symbol_to_opcode(op_sym);

    if (op == INVALID)
    {
        scm_wrong_type_arg("make_op_token", 2, op_sym);
        return NULL;
    }

    const int arg_count = Token::args(op);
    if (arg_count != scm_to_int(scm_length(args)))
    {
        scm_misc_error("make_op_token", "Invalid argument count", SCM_EOL);
    }

    Token* a = (arg_count < 1) ? NULL :
        untag_ptr<Token>(scm_car(args));
    Token* b = (arg_count < 2) ? NULL :
        untag_ptr<Token>(scm_cadr(args));

    return tag_ptr(s->operation(op, a, b));
}

////////////////////////////////////////////////////////////////////////////////

void tree_delete(void* ptr)
{
    delete static_cast<Tree*>(ptr);
}

SCM tree_new(SCM store, SCM root)
{
    auto s = untag_ptr<Store>(store);
    auto r = untag_ptr<Token>(root);
    return tag_ptr(new Tree(s, r), tree_delete);
}

SCM tree_eval_double(SCM tree, SCM x, SCM y, SCM z)
{
    auto t = untag_ptr<Tree>(tree);
    double out = t->eval(scm_to_double(x),
                         scm_to_double(y),
                         scm_to_double(z));
    return scm_from_double(out);
}

SCM tree_eval_interval(SCM tree, SCM x, SCM y, SCM z)
{
    auto t = untag_ptr<Tree>(tree);
    Interval X(scm_to_double(scm_car(x)), scm_to_double(scm_cdr(x)));
    Interval Y(scm_to_double(scm_car(y)), scm_to_double(scm_cdr(y)));
    Interval Z(scm_to_double(scm_car(z)), scm_to_double(scm_cdr(z)));

    Interval out = t->eval(X, Y, Z);
    return scm_cons(scm_from_double(out.lower()),
                    scm_from_double(out.upper()));
}

////////////////////////////////////////////////////////////////////////////////

SCM gl_window(SCM tree)
{
    auto win = Window();
    win.addShape(untag_ptr<Tree>(tree));
    win.run();
    return SCM_ELISP_NIL;
}

////////////////////////////////////////////////////////////////////////////////

void libao_init()
{
    scm_c_define_gsubr("make-store", 0, 0, 0, (void*)store_new);
    scm_c_define_gsubr("make-tree", 2, 0, 0, (void*)tree_new);
    scm_c_define_gsubr("tree-eval-double", 4, 0, 0, (void*)tree_eval_double);
    scm_c_define_gsubr("tree-eval-interval", 4, 0, 0, (void*)tree_eval_interval);

    scm_c_define_gsubr("token-x", 1, 0, 0, (void*)token_x);
    scm_c_define_gsubr("token-y", 1, 0, 0, (void*)token_y);
    scm_c_define_gsubr("token-z", 1, 0, 0, (void*)token_z);

    scm_c_define_gsubr("token-const", 2, 0, 0, (void*)token_const);
    scm_c_define_gsubr("token-op", 3, 0, 0, (void*)token_op);

    scm_c_define_gsubr("gl-window", 1, 0, 0, (void*)gl_window);
}
