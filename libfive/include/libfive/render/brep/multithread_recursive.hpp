/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <tbb/task.h>
#include <tbb/enumerable_thread_specific.h>

namespace libfive {


// Calls Callable on Divisible, recursing over its children.  

// Divisible should be either a (possibly const) class or a pointer to one; in
// either case, the class should have a method getChildren(), returning an 
// iterable container of Divisibles with a size() method.

// If Out is not void, PreCallable should have the signature 
// Out(Divisible, Out* par, unsigned childNo, bool& recurse, Local&); 
// par will be the parent of the current node, or nullptr if the current node 
// is the root, and childNo will be which number child of that parent is
// currently being handled (or undefined if the current node is the root).  
// "recurse" must be set by PreCallable to instruct this algorithm whether to
// continue recursing or to make the current node into a leaf.  While 
// PreCallable may modify the Out that is passed in, it must do so atomically
// if at all.
// If Out is void, the signature should instead be
// void(Divisible, bool& recurse, Local&).  In either case, the call must be
// thread-safe with respect to any captured variables (i.e. only writing to 
// atomics, if at all), and its return value is also the return value of this
// function.

// PostCallable should have the signature void(Divisible, Out& child, Local&)
// if Out is not void, or void(Divisible, Local&) if it is.  PreCallable will be
// called before recursing to children, while PostCallable will be called
// afterward; there is no guarantee that the two will be called on the same 
// thread (or with the same Local&).  If "recurse" is set to false by 
// PreCallable, or if the algorithm was cancelled via tbb, PostCallable will 
// not be called.

// The final two function arguments (parent and childNo) should not be passed in
// except via recursive calling.

template <class Divisible, class Out, class Local,
    class PreCallable, class PostCallable>
Out multithreadRecursive(
        Divisible root, tbb::enumerable_thread_specific<Local>& locals,
        const PreCallable& pre, const PostCallable& post,
        Out* parent = nullptr, unsigned childNo = -1) {

    struct Task : tbb::task {
        Divisible root;
        tbb::enumerable_thread_specific<Local>& locals;
        const PreCallable& pre;
        const PostCallable& post;
        Out* parent;
        unsigned childNo;

        Task(Divisible root, tbb::enumerable_thread_specific<Local>& locals,
            const PreCallable& pre, const PostCallable& post,
            Out* parent = nullptr, unsigned childNo = -1)
            : root(root), locals(locals), pre(pre), post(post),
            parent(parent), childNo(childNo) {}

            Task* execute() override {
            multithreadRecursive(
                root, locals, pre, post, parent, childNo);
            return nullptr;
        }
    };

    bool toRecurse;
    auto getChildren = [&root]() {
        if constexpr (std::is_pointer_v<Divisible>) {
            return root->getChildren();
        }
        else {
            return root.getChildren();
        }
    };

    if constexpr (std::is_same_v<Out, void>) {
        pre(root, toRecurse, locals.local());
        if (toRecurse) {
            auto waiter = new(tbb::task::allocate_root()) tbb::empty_task;
            auto children = getChildren();
            waiter->set_ref_count(children.size() + 1);
            tbb::task_list list;
            for (auto& child : children) {
                // We don't need to set parent and childNo, since when Out is
                // void they are not used.
                auto task = new(waiter->allocate_child())
                    Task(child, locals, pre, post);
                list.push_back(*task);
            }
            waiter->spawn_and_wait_for_all(list);
            if (!tbb::task::self().is_cancelled()) {
              post(std::move(root), locals.local());
            }
        }
    }
    else {
        auto out = pre(root, parent, childNo, toRecurse, locals.local());
        if (toRecurse) {
            auto waiter = new(tbb::task::allocate_root()) tbb::empty_task;
            auto children = getChildren();
            waiter->set_ref_count(children.size() + 1);
            tbb::task_list list;
            auto i = 0;
            for (auto& child : children) {
                auto task = new(waiter->allocate_child())
                    Task(child, locals, pre, post, &out, i++);
                list.push_back(*task);
            }
            assert(i == children.size());
            waiter->spawn_and_wait_for_all(list);
            if (!tbb::task::self().is_cancelled()) {
              post(std::move(root), out, locals.local());
            }
        }
        return out;
    }
}

}   // namespace libfive
