#include <iostream>
#include <cassert>

#include "libfive/tree/oracle_clause.hpp"
#include "libfive/tree/transformed_oracle_clause.hpp"

namespace Kernel {

std::map<std::string, std::pair<OracleClause::Serializer,
                                       OracleClause::Deserializer>>
    OracleClause::installed;

std::unique_ptr<const OracleClause> OracleClause::deserialize(
        const std::string& name, const uint8_t*& pos, const uint8_t* end,
        std::map<uint32_t, Tree>& ts)
{
    auto itr = installed.find(name);
    if (itr == installed.end())
    {
        std::cerr << "OracleClause::deserialize: no installed \""
                  << name << "\"\n"
                  << "  You may need to call OracleClause::install."
                  << std::endl;
        return nullptr;
    }
    else
    {
        return (*itr).second.second(pos, end, ts);
    }
}

bool OracleClause::serialize(const std::string& name,
        const OracleClause* clause, std::vector<uint8_t>& data,
         std::map<Tree::Id, uint32_t>& ids)
{
    auto itr = installed.find(name);
    if (itr == installed.end())
    {
        std::cerr << "OracleClause::serialize: no installed \""
                  << name << "\"\n"
                  << "  You may need to call OracleClause::install."
                  << std::endl;
        return false;
    }
    else
    {
        return (*itr).second.first(clause, data, ids);
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

}   // namespace Kernel
