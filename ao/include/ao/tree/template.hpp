#pragma once

#include <map>
#include "ao/tree/tree.hpp"

namespace Kernel {

struct Template
{
    Template(Tree t) : tree(t) { /* Nothing to do here */ }

    Tree tree;

    std::string name;
    std::string doc;

    std::map<Tree::Id, std::string> var_names;
    std::map<Tree::Id, std::string> var_docs;
};

}   // namespace Kernel
