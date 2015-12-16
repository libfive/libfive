#include <iostream>
#include <string>

#include "ao/bind/scm.hpp"
#include "ao/core/store.hpp"
#include "ao/core/tree.hpp"
#include "ao/core/opcode.hpp"
#include "ao/core/token.hpp"

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

void store_delete(void* ptr)
{
    delete static_cast<Store*>(ptr);
}

SCM store_new()
{
    Store* s = new Store;
    return scm_from_pointer(s, store_delete);
}

////////////////////////////////////////////////////////////////////////////////

SCM token_x(SCM s)
{
    return scm_from_pointer(static_cast<Store*>(scm_to_pointer(s))->X(), NULL);
}

SCM token_y(SCM s)
{
    return scm_from_pointer(static_cast<Store*>(scm_to_pointer(s))->Y(), NULL);
}

SCM token_z(SCM s)
{
    return scm_from_pointer(static_cast<Store*>(scm_to_pointer(s))->Z(), NULL);
}

////////////////////////////////////////////////////////////////////////////////

SCM token_const(SCM store, SCM v)
{
    Store* s = static_cast<Store*>(scm_to_pointer(store));
    double value = scm_to_double(v);
    return scm_from_pointer(s->constant(value), NULL);
}

SCM token_op(SCM store, SCM op_sym, SCM args)
{
    Store* s = static_cast<Store*>(scm_to_pointer(store));
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
        static_cast<Token*>(scm_to_pointer(scm_car(args)));
    Token* b = (arg_count < 2) ? NULL :
        static_cast<Token*>(scm_to_pointer(scm_cadr(args)));

    return scm_from_pointer(s->operation(op, a, b), NULL);
}

////////////////////////////////////////////////////////////////////////////////

void tree_delete(void* ptr)
{
    delete static_cast<Tree*>(ptr);
}

SCM tree_new(SCM store, SCM root)
{
    Store* s = static_cast<Store*>(scm_to_pointer(store));
    Token* r = static_cast<Token*>(scm_to_pointer(root));
    return scm_from_pointer(new Tree(s, r), tree_delete);
}

SCM tree_eval_double(SCM tree, SCM x, SCM y, SCM z)
{
    Tree* t = static_cast<Tree*>(scm_to_pointer(tree));
    double out = t->eval(scm_to_double(x),
                         scm_to_double(y),
                         scm_to_double(z));
    return scm_from_double(out);
}

SCM tree_eval_doubles(SCM tree, SCM x, SCM y, SCM z)
{
    Tree* t = static_cast<Tree*>(scm_to_pointer(tree));

    std::vector<double> xs;
    std::vector<double> ys;
    std::vector<double> zs;

    int nx = scm_to_int(scm_length(x));
    int ny = scm_to_int(scm_length(y));
    int nz = scm_to_int(scm_length(z));

    if (nx != ny || ny != nz)
    {
        scm_misc_error("tree_eval_doubles",
                       "Arrays must be of the same size", SCM_EOL);
        return SCM_ELISP_NIL;
    }

    auto unpack = [](int n, SCM d, std::vector<double>* ds)
    {
        ds->reserve(n);
        while (!scm_is_null(d))
        {
            ds->push_back(scm_to_double(scm_car(d)));
            d = scm_cdr(d);
        }
    };

    unpack(nx, x, &xs);
    unpack(ny, y, &ys);
    unpack(nz, z, &zs);

    std::vector<double> result = t->eval(xs, ys, zs);

    // Pack the results into a Scheme list
    SCM out = SCM_ELISP_NIL;
    for (auto itr = result.crbegin(); itr != result.crend(); ++itr)
    {
        out = scm_cons(scm_from_double(*itr), out);
    }
    return out;
}

SCM tree_eval_interval(SCM tree, SCM x, SCM y, SCM z)
{
    Tree* t = static_cast<Tree*>(scm_to_pointer(tree));
    Interval X(scm_to_double(scm_car(x)), scm_to_double(scm_cdr(x)));
    Interval Y(scm_to_double(scm_car(y)), scm_to_double(scm_cdr(y)));
    Interval Z(scm_to_double(scm_car(z)), scm_to_double(scm_cdr(z)));

    Interval out = t->eval(X, Y, Z);
    return scm_cons(scm_from_double(out.lower()),
                    scm_from_double(out.upper()));
}

////////////////////////////////////////////////////////////////////////////////

void libao_init()
{
    scm_c_define_gsubr("make-store", 0, 0, 0, (void*)store_new);
    scm_c_define_gsubr("make-tree", 2, 0, 0, (void*)tree_new);
    scm_c_define_gsubr("tree-eval-double", 4, 0, 0, (void*)tree_eval_double);
    scm_c_define_gsubr("tree-eval-doubles", 4, 0, 0, (void*)tree_eval_doubles);
    scm_c_define_gsubr("tree-eval-interval", 4, 0, 0, (void*)tree_eval_interval);

    scm_c_define_gsubr("token-x", 1, 0, 0, (void*)token_x);
    scm_c_define_gsubr("token-y", 1, 0, 0, (void*)token_y);
    scm_c_define_gsubr("token-z", 1, 0, 0, (void*)token_z);

    scm_c_define_gsubr("token-const", 2, 0, 0, (void*)token_const);
    scm_c_define_gsubr("token-op", 3, 0, 0, (void*)token_op);
}
