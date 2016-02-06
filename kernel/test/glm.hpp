#include <catch/catch.hpp>
#include <glm/gtx/string_cast.hpp>
#include <glm/vec3.hpp>

/*  Overload StringMaker to properly print glm::vec3  */
namespace Catch {
    template<> struct StringMaker<glm::vec3> {
        static std::string convert(glm::vec3 const& value) {
            return glm::to_string(value);
        }
    };
}
