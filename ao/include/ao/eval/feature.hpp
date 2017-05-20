#pragma once

#include <list>
#include <map>

#include <Eigen/Eigen>

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
    bool isCompatible(Eigen::Vector3d e) const;

    /*
     *  If incompatible, does nothing and returns false
     *  Otherwise, pushes to the front of the choice list and returns true
     */
    bool push(Eigen::Vector3d e, Choice c={0, 0});

    /*
     *  Accessor method for the choice list
     */
    const std::list<Choice>& getChoices() const { return choices; }

    /*
     *  Top-level derivative (set manually)
     */
    Eigen::Vector3d deriv;

    /*
     *  Inserts a choice without any checking
     */
    void push_raw(Choice c, Eigen::Vector3d v);

    /*
     *  Returns the epsilon associated with a particular choice
     */
    Eigen::Vector3d getEpsilon(Clause::Id i) const { return _epsilons.at(i); }

protected:
    typedef enum { NOT_PLANAR, PLANAR_FAIL, PLANAR_SUCCESS } PlanarResult;
    PlanarResult checkPlanar(Eigen::Vector3d v) const;

    /*  Per-clause decisions  */
    std::list<Choice> choices;

    /*  Deduplicated list of epsilons  */
    std::list<Eigen::Vector3d> epsilons;

    /*  Per-clause epsilons  */
    std::map<Clause::Id, Eigen::Vector3d> _epsilons;
};

/*  Defining operator< lets us store Choices in std::set, etc */
bool operator<(const Feature::Choice& a, const Feature::Choice& b);

}   // namespace Kernel
