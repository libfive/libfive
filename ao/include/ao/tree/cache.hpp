#pragma once

#include <map>
#include <memory>
#include <mutex>

#include "ao/tree/tree.hpp"

namespace Kernel {

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
        Handle() : lock(mut), cache(&_instance) {}
        Cache* operator->() const { return cache; }
    protected:
        std::unique_lock<std::recursive_mutex> lock;
        Cache* cache;
    };

public:
    /*
     *  Returns a safe (locking) handle to the global Cache
     */
    static Handle instance() { return Handle(); }

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
     *  Serialize to a byte array
     */
    std::vector<uint8_t> serialize(Node root, const std::string& name="") const;

protected:
    /*
     *  Cache constructor is private so outsiders must use instance()
     */
    Cache() {}

    /*
     *  Serialize a string, wrapping in quotes and escaping with backslash
     */
    static void serialize(const std::string& s, std::vector<uint8_t>& out);

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
     *  A Key uniquely identifies an operation Node, so that we can
     *  deduplicate based on opcode  and arguments
     */
    typedef std::tuple<Opcode::Opcode,  /* opcode */
                       Tree::Id,        /* lhs */
                       Tree::Id         /* rhs */ > Key;
    std::map<Key, std::weak_ptr<Tree::Tree_>> ops;

    /*  Constants in the tree are uniquely identified by their value  */
    std::map<float, std::weak_ptr<Tree::Tree_>> constants;

    static std::recursive_mutex mut;
    static Cache _instance;
};

}   // namespace Kernel
