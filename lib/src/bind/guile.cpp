#include <iostream>
#include <string>

#include <boost/algorithm/string.hpp>
#include <libguile.h>

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

static void store_delete(void* ptr)
{
    delete static_cast<Store*>(ptr);
}

static SCM store_new()
{
    return tag_ptr(new Store, store_delete);
}

////////////////////////////////////////////////////////////////////////////////

static SCM token_x(SCM s)
{
    return tag_ptr(untag_ptr<Store>(s)->X());
}

static SCM token_y(SCM s)
{
    return tag_ptr(untag_ptr<Store>(s)->Y());
}

static SCM token_z(SCM s)
{
    return tag_ptr(untag_ptr<Store>(s)->Z());
}

////////////////////////////////////////////////////////////////////////////////

static SCM token_const(SCM store, SCM v)
{
    auto s = untag_ptr<Store>(store);
    double value = scm_to_double(v);
    return tag_ptr(s->constant(value));
}

static SCM token_op(SCM store, SCM op_sym, SCM args)
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

static void tree_delete(void* ptr)
{
    delete static_cast<Tree*>(ptr);
}

static SCM tree_new(SCM store, SCM root)
{
    auto s = untag_ptr<Store>(store);
    auto r = untag_ptr<Token>(root);
    return tag_ptr(new Tree(s, r), tree_delete);
}

static SCM tree_eval_double(SCM tree, SCM x, SCM y, SCM z)
{
    auto t = untag_ptr<Tree>(tree);
    double out = t->eval(scm_to_double(x),
                         scm_to_double(y),
                         scm_to_double(z));
    return scm_from_double(out);
}

static SCM tree_eval_interval(SCM tree, SCM x, SCM y, SCM z)
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

static SCM show_tree(SCM tree)
{
    Window::instance()->addTree(untag_ptr<Tree>(tree));
    return SCM_ELISP_NIL;
}

////////////////////////////////////////////////////////////////////////////////

static void populate_module(void* data)
{
    (void)data;

    auto define = [](std::string name, int argc, void* func){
        SCM f = scm_c_make_gsubr(name.c_str(), argc, 0, 0, func);
        scm_c_define(name.c_str(), f);
        scm_c_export(name.c_str(), 0); };

    define("make-store", 0, (void*)store_new);

    define("make-tree", 2, (void*)tree_new);
    define("tree-eval-double", 4, (void*)tree_eval_double);
    define("tree-eval-interval", 4, (void*)tree_eval_interval);

    define("token-x", 1, (void*)token_x);
    define("token-y", 1, (void*)token_y);
    define("token-z", 1, (void*)token_z);

    define("token-const", 2, (void*)token_const);
    define("token-op", 3, (void*)token_op);

    define("show-tree", 1, (void*)show_tree);
}

static void guile_hotpatch()
{
    SCM v = scm_c_private_lookup("system repl common", "*version*");

    char* str_ptr = scm_to_locale_string(scm_variable_ref(v));
    std::string str(str_ptr);
    free(str_ptr);

    // Patch ambiguity in the Guile startup message
    boost::replace_all(str, "This program", "Guile");

    str = R"(         .8.           ,o888888o.
        .888.       . 8888     `88.
       :88888.     ,8 8888       `8b
      . `88888.    88 8888        `8b
     .8. `88888.   88 8888         88
    .8`8. `88888.  88 8888         88
   .8' `8. `88888. 88 8888        ,8P
  .8'   `8. `88888.`8 8888       ,8P
 .888888888. `88888.` 8888     ,88'
.8'       `8. `88888.  `8888888P'
       (c) 2015 Matt Keeter

REPL is provided by )" + str;

    scm_variable_set_x(v, scm_from_locale_string(str.c_str()));
}

static void* guile_init(void* data)
{
    (void)data;

    scm_c_define_module("ao lib", populate_module, nullptr);
    scm_primitive_load(scm_from_locale_string("ao/startup.scm"));

    guile_hotpatch();

    return nullptr;
}

int main(int argc, char* argv[])
{
    // Initialize OpenGL and trigger an initial draw action
    Window::instance()->draw();

    // Start a Guile REPL running in a secondary thread
    auto repl = std::async(std::launch::async, [=](){
        scm_with_guile(&guile_init, NULL);
        scm_shell(argc, argv); });

    while (1)
    {
        Window::instance()->poll();
        std::future_status status = repl.wait_for(std::chrono::seconds(0));
        if (status == std::future_status::ready)
        {
            break;
        }
    }
    delete Window::instance();

    return 0;
}
