/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <map>
#include <memory>
#include <mutex>

#include "libfive/tree/tree.hpp"

namespace libfive {

/*
 *  A Cache stores values in a deduplicated math expression
 */
class Cache
{
    /*  Helper typedef to avoid writing this over and over again  */
    typedef std::shared_ptr<Tree::Tree_> Node;

    /*  Handle to safely access cache  */
    class Handle
    {
    public:
        Handle(Cache& c) : lock(*mut()), c(c) {}
        Cache* operator->() const { return &c; }
    protected:
        std::unique_lock<std::recursive_mutex> lock;
        Cache& c;
    };

public:
    /*
     *  Returns a safe (locking) handle to the global Cache
     */
    static Handle instance() {
        return Handle(*singleton());
    }

    /*
     *  Deletes all singleton objects, allowing the user to ensure that
     *  nothing is leaked to the operating system (e.g. to avoid false
     *  positives in leak detection software).  Fails (returning -1)
     *  if the cache is not empty; returns 0 if successful.  Caution must
     *  be used: If instance() or shutdown() is called after or during a 
     *  successful shutdown, or if shutdown() is called while a Handle that 
     *  was created on the same thread still exists, the behavior is undefined.
     */
    static int shutdown();

    Node constant(float v);
    Node operation(Opcode::Opcode op, Node lhs=nullptr, Node rhs=nullptr,
                   bool simplify=true);

    Node X() { return operation(Opcode::VAR_X); }
    Node Y() { return operation(Opcode::VAR_Y); }
    Node Z() { return operation(Opcode::VAR_Z); }

    Node var();

    /*
     *  Called when the last Tree_ is destroyed
     */
    void del(float v);
    void del(Opcode::Opcode op, Node lhs=nullptr, Node rhs=nullptr);

    /*
     *  Returns the given node as an affine sum-of-multiplications
     *
     *  This is a building block for automatic collapsing of affine
     *  expressions, exposed here primarily for unit testing.
     */
    std::map<Node, float> asAffine(Node n);

    /*
     *  Converts a sum-of-multiplications into an affine tree.
     */
    Node fromAffine(const std::map<Node, float>& ns);

protected:
    static std::recursive_mutex* mut() {
      static auto m = new std::recursive_mutex;
      return m;
    }

    static Cache* singleton() {
        static Cache* c = new Cache();
        return c;
    }

    /*
     *  Cache constructor is private so outsiders must use instance()
     */
    Cache() {}

    /*
     *  Checks whether the operation is an identity operation
     *  If so returns an appropriately simplified tree
     *  i.e. (X + 0) will return X
     */
    Node checkIdentity(Opcode::Opcode op, Node a, Node b);

    /*
     *  If the opcode is commutative, consider tweaking tree structure
     *  to keep it as balanced as possible.
     */
    Node checkCommutative(Opcode::Opcode op, Node a, Node b);

    /*
     *  Checks whether a `op` b can be replaced by a simpler affine
     *  form, e.g. (2*x + y) - (4*y) --> 2*x - 3*y
     */
    Node checkAffine(Opcode::Opcode op, Node a, Node b);

    /*
     *  A Key uniquely identifies an operation Node, so that we can
     *  deduplicate based on opcode  and arguments
     */
    typedef std::tuple<Opcode::Opcode,  /* opcode */
                       Tree::Id,        /* lhs */
                       Tree::Id         /* rhs */ > Key;
    std::map<Key, std::weak_ptr<Tree::Tree_>> ops;

    /*  Constants in the tree are uniquely identified by their value  */
    std::map<float, std::weak_ptr<Tree::Tree_>> constants;

    /*  nan cannot be stored in the usual map, so the nan constant lives here */
    std::weak_ptr<Tree::Tree_> nan_constant;

    /*  Oracles do not need to use the cache to be deduplicated, since they
     *  are created from unique_ptr's, and therefore are already impossible
     *  to duplicate.  */
};

}   // namespace libfive
