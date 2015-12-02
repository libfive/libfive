#pragma once

#include <map>
#include <utility>
#include <string>

#include <libguile.h>
#include "opcodes.h"

////////////////////////////////////////////////////////////////////////////////

struct Token;

////////////////////////////////////////////////////////////////////////////////

struct Store
{
    std::map<double, Token*> constants;
    std::map<Opcode, std::map<std::pair<Token*, Token*>, Token*>> ops;
    std::map<std::string, Token*> vars;
};

////////////////////////////////////////////////////////////////////////////////

extern "C" {
    void delete_store(void* ptr);
    SCM make_store();
    SCM describe_store(SCM store);
    SCM make_var_token(SCM store, SCM v);
    SCM make_num_token(SCM store, SCM n);
    SCM make_op_token(SCM store, SCM op, SCM args);
};
