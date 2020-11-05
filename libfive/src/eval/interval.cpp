/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/eval/interval.hpp"

namespace libfive {
    thread_local std::optional<boost::numeric::interval_lib::save_state<
        Interval::BaseRound>> Interval::savedRounding = std::nullopt;
}
