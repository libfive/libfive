#include <iostream>

#include "libfive/tree/oracle_clause.hpp"

namespace Kernel {

std::map<std::string, std::pair<OracleClause::Serializer,
                                       OracleClause::Deserializer>>
    OracleClause::installed;

std::unique_ptr<const OracleClause> OracleClause::deserialize(
        const std::string& name, const uint8_t*& pos, const uint8_t* end)
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
        return (*itr).second.second(pos, end);
    }
}

bool OracleClause::serialize(const std::string& name,
        const OracleClause* clause, std::vector<uint8_t>& data)
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
        return (*itr).second.first(clause, data);
    }
}

}   // namespace Kernel
