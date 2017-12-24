/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once

#include <list>
#include <map>
#include <set>

#include <Eigen/Eigen>

#include "libfive/eval/clause.hpp"

namespace Kernel
{

class Feature
{
public:
    struct Choice
    {
        const Clause::Id id;
        const int choice;
        bool operator<(const Choice& other) { return id < other.id; }
    };

    /*
     *  Checks to see whether a particular epsilon is compatible with
     *  all of the other epsilons in the system.
     *  This is a slow (worst-case O(n^3)) operation, but it should be called
     *  rarely and so doesn't need to be optimized yet.
     */
    bool isCompatible(const Eigen::Vector3d& e) const;

    /*
     *  If incompatible, does nothing and returns false
     *  Otherwise, pushes to the front of the choice list and returns true
     */
    bool push(const Eigen::Vector3d& e, Choice c={0, 0});

    /*
     *  Accessor method for the choice list
     */
    const std::set<Choice>& getChoices() const { return choices; }

    /*
     *  Top-level derivative (set manually)
     */
    Eigen::Vector3d deriv;

    /*
     *  Inserts a choice without any checking
     */
    void pushRaw(Choice c, const Eigen::Vector3d& v);

    /*
     *  Inserts a choice without an associated direction
     *  This is useful to collapse cases like min(a, a)
     */
    void pushChoice(Choice c);

    /*
     *  Returns the epsilon associated with a particular choice
     */
    Eigen::Vector3d getEpsilon(Clause::Id i) const { return _epsilons.at(i); }

    /*
     *  Checks to see whether the given clause has an epsilon
     */
    bool hasEpsilon(Clause::Id i) const
        { return _epsilons.find(i) != _epsilons.end(); }

protected:
    /*
     *  Versions of isCompatible and push when e is known to be normalized
     */
    bool isCompatibleNorm(const Eigen::Vector3d& e) const;
    bool pushNorm(const Eigen::Vector3d& e, Choice choice);

    typedef enum { NOT_PLANAR, PLANAR_FAIL, PLANAR_SUCCESS } PlanarResult;
    PlanarResult checkPlanar(const Eigen::Vector3d& v) const;

    /*  Per-clause decisions  */
    std::set<Choice> choices;

    /*  Deduplicated list of epsilons  */
    std::list<Eigen::Vector3d> epsilons;

    /*  Per-clause epsilons  */
    std::map<Clause::Id, Eigen::Vector3d> _epsilons;
};

/*  Defining operator< lets us store Choices in std::set, etc */
bool operator<(const Feature::Choice& a, const Feature::Choice& b);

}   // namespace Kernel
