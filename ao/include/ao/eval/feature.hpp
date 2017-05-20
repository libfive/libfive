#pragma once

#include <list>
#include <map>
#include <glm/vec3.hpp>

#include "ao/eval/clause.hpp"

namespace Kernel
{

class Feature
{
public:
    struct Choice
    {
        const Clause::Id id;
        const int choice;
    };

    /*
     *  Checks to see whether a particular epsilon is compatible with
     *  all of the other epsilons in the system.
     *  This is a slow (worst-case O(n^3)) operation, but it should be called
     *  rarely and so doesn't need to be optimized yet.
     */
    bool isCompatible(glm::vec3 e) const;

    /*
     *  If incompatible, does nothing and returns false
     *  Otherwise, pushes to the front of the choice list and returns true
     */
    bool push(glm::vec3 e, Choice c={0, 0});

    /*
     *  Accessor method for the choice list
     */
    const std::list<Choice>& getChoices() const { return choices; }

    /*
     *  Top-level derivative (set manually)
     */
    glm::vec3 deriv;

    /*
     *  Inserts a choice without any checking
     */
    void push_raw(Choice c, glm::vec3 v);

    /*
     *  Returns the epsilon associated with a particular choice
     */
    glm::vec3 getEpsilon(Clause::Id i) const { return _epsilons.at(i); }

protected:
    typedef enum { NOT_PLANAR, PLANAR_FAIL, PLANAR_SUCCESS } PlanarResult;
    PlanarResult checkPlanar(glm::vec3 v) const;

    /*  Per-clause decisions  */
    std::list<Choice> choices;

    /*  Deduplicated list of epsilons  */
    std::list<glm::vec3> epsilons;

    /*  Per-clause epsilons  */
    std::map<Clause::Id, glm::vec3> _epsilons;
};

/*  Defining operator< lets us store Choices in std::set, etc */
bool operator<(const Feature::Choice& a, const Feature::Choice& b);

}   // namespace Kernel
