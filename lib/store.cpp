#include <libguile.h>
#include "store.h"
#include "token.h"

static std::string symbol_to_string(SCM v)
{
    SCM str = scm_symbol_to_string(v);
    char* str_ptr = scm_to_locale_string(str);
    std::string out(str_ptr);
    free(str_ptr);

    return out;
}

static Opcode symbol_to_opcode(SCM sym)
{
    const std::string str = symbol_to_string(sym);

    if (str == "add")           return OP_ADD;
    else if (str == "mul")      return OP_MUL;
    else if (str == "min")      return OP_MIN;
    else if (str == "max")      return OP_MAX;
    else if (str == "sub")      return OP_SUB;
    else if (str == "div")      return OP_DIV;
    else if (str == "sqrt")     return OP_SQRT;
    else                        return INVALID;
}

static int arg_count(Opcode op)
{
    if (op < NO_ARGUMENTS)          return 0;
    else if (op < ONE_ARGUMENT)     return 1;
    else if (op < TWO_ARGUMENTS)    return 2;
    return -1;
}

void delete_store(void* ptr)
{
    delete static_cast<Store*>(ptr);
}

SCM make_store()
{
    Store* s = new Store;
    return scm_from_pointer(s, delete_store);
}

SCM make_var_token(SCM store, SCM v)
{
    std::string var = symbol_to_string(v);

    Store* s = static_cast<Store*>(scm_to_pointer(store));
    if (s->vars.find(var) == s->vars.end())
    {
        s->vars[var] = new VarToken(var);
    }

    return scm_from_pointer(s->vars[var], NULL);
}

SCM make_num_token(SCM store, SCM v)
{
    double value = scm_to_double(v);

    Store* s = static_cast<Store*>(scm_to_pointer(store));
    if (s->constants.find(value) == s->constants.end())
    {
        s->constants[value] = new ConstToken(value);
    }

    return scm_from_pointer(s->constants[value], NULL);
}

SCM make_op_token(SCM store, SCM op_sym, SCM args)
{
    Opcode op = symbol_to_opcode(op_sym);
    if (op == INVALID)
    {
        scm_wrong_type_arg("make_op_token", 2, op_sym);
        return NULL;
    }

    Store* s = static_cast<Store*>(scm_to_pointer(store));
    int arg_length = scm_to_int(scm_length(args));
    if (arg_count(op) != arg_length)
    {
        scm_misc_error("make_op_token", "Invalid argument count", NULL);
    }

    std::pair<Token*, Token*> arg_pair;
    arg_pair.first  = (arg_length > 0) ?
        static_cast<Token*>(scm_to_pointer(scm_car(args))) : NULL;
    arg_pair.second = (arg_length > 1) ?
        static_cast<Token*>(scm_to_pointer(scm_cadr(args))) : NULL;

    if (s->ops[op].find(arg_pair) == s->ops[op].end())
    {
        s->ops[op][arg_pair] = new OpToken(op, arg_pair.first, arg_pair.second);
    }
    return scm_from_pointer(s->ops[op][arg_pair], NULL);
}
