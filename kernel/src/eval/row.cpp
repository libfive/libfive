/*
 *  Copyright (C) 2016 Matthew Keeter
 *
 *  This file is part of Ao.
 *
 *  Ao is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <cassert>

#include "ao/kernel/eval/row.hpp"
#include "ao/kernel/eval/clause.hpp"

void Row::push()
{
    disabled.push(0);

    size_t index=0;
    while (index < active)
    {
        if ((*this)[index]->checkDisabled())
        {
            disable(index);
        }
        else
        {
            index++;
        }
    }
}

void Row::pop()
{
    // Walk through the most recently disabled atoms, clearing their
    // DISABLED flag to indicate that they should resume normal operation
    for (size_t i=0; i < disabled.top(); ++i)
    {
        (*this)[active++]->enable();
    }
    disabled.pop();
}

void Row::disable(size_t i)
{
    assert(i < active);
    assert(disabled.size() > 0);

    // Mark this Clause as disabled
    (*this)[i]->disable();

    // Swap the atom to the back of the list, decrementing active
    // Since we only evaluate from 0 to active, this disables the atom
    std::swap((*this)[i], (*this)[--active]);
    disabled.top()++;
}

void Row::setSize()
{
    active = size();
}
