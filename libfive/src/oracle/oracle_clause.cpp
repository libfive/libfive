#include <iostream>
#include <cassert>

#include "libfive/oracle/oracle_clause.hpp"
#include "libfive/oracle/transformed_oracle_clause.hpp"
#include "libfive/oracle/oracle.hpp"

namespace libfive {

std::unique_ptr<const OracleClause> OracleClause::deserialize(
        const std::string& name, Deserializer& in)
{
    auto itr = installed().find(name);
    if (itr == installed().end())
    {
        std::cerr << "OracleClause::deserialize: no installed \""
                  << name << "\"\n"
                  << "  You may need to call OracleClause::install."
                  << std::endl;
        return nullptr;
    }
    else
    {
        return (*itr).second.second(in);
    }
}

bool OracleClause::serialize(const std::string& name,
        const OracleClause* clause, Serializer& out)
{
    auto itr = installed().find(name);
    if (itr == installed().end())
    {
        std::cerr << "OracleClause::serialize: no installed \""
                  << name << "\"\n"
                  << "  You may need to call OracleClause::install."
                  << std::endl;
        return false;
    }
    else
    {
        return (*itr).second.first(clause, out);
    }
}

std::unique_ptr<const OracleClause> OracleClause::remap(
            Tree self, Tree X_, Tree Y_, Tree Z_) const
{
    assert(self->op == Opcode::ORACLE);
    assert(self->oracle.get() == this);

    return std::unique_ptr<const OracleClause>(
        new TransformedOracleClause(self, X_, Y_, Z_));
}

unsigned OracleClause::rank() const
{
    unsigned out = 0;
    for (auto dependency : evaluationDependencies()) 
    {
        out = std::max(out, dependency->rank + 1);
    }
    return out;
}

std::unique_ptr<Oracle>
OracleClause::getOracle(std::unordered_map<Tree::Id, Clause::Id> map) const
{
    return getOracle();
}

std::unique_ptr<Oracle> OracleClause::getOracle() const
{
    return getOracle({});
}

}   // namespace libfive
