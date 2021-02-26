/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/eval/evaluator.hpp"

namespace libfive {

bool Evaluator::intervalAndCheck(const Eigen::Vector3f& lower,
                                 const Eigen::Vector3f& upper,
                                 size_t count,
                                 const Tape& tape) 
{
  assert(!lower.array().isNaN().any()); // A region's bounds should
  assert(!upper.array().isNaN().any()); // never be NaN.

  i[deck->X] = { lower.x(), upper.x() };
  i[deck->Y] = { lower.y(), upper.y() };
  i[deck->Z] = { lower.z(), upper.z() };
  maybe_nan[deck->X] = false;
  maybe_nan[deck->Y] = false;
  maybe_nan[deck->Z] = false;

  setCount(count);

  for (auto idx = 0; idx < count_actual; ++idx) 
  {
    std::array<Clause::Id, 3> coords{ deck->X, deck->Y, deck->Z };
    for (auto coord : coords) {
      if (std::isnan(v(coord, idx)) ||
          v(coord, idx) < i[coord].lower() ||
          v(coord, idx) > i[coord].upper()) 
      {
        assert(false);
        return false;
      }
    }
  }

  for (auto& o : deck->oracles)
  {
    o->set(lower, upper);
  }

  deck->bindOracles(tape);
  bool failed;
  for (auto itr = tape.rbegin(); itr != tape.rend(); ++itr) {
    (*this)(itr->op, itr->id, itr->a, itr->b, failed);
    if (failed) {
      break;
    }
  }
  deck->unbindOracles();
  return !failed;
}

////////////////////////////////////////////////////////////////////////////////

void Evaluator::operator()(Opcode::Opcode op, Clause::Id id,
                           Clause::Id a_, Clause::Id b_, bool& failed)
{
  IntervalEvaluator::operator()(op, id, a_, b_);
  ArrayEvaluator::operator()(op, id, a_, b_);

  for (auto idx = 0; idx < count_actual; ++idx) 
  {
    if ((std::isnan(v(id, idx)) && !maybe_nan[id]) ||
        v(id, idx) < i[id].lower() ||
        v(id, idx) > i[id].upper())
    {
      assert(false);
      // Other test code can be added here as needed.  It's
      // a great place for a breakpoint as well.
      failed = true;
    }
  }
}

}   // namespace libfive
