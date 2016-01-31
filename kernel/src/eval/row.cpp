#include <cassert>

#include "ao/eval/row.hpp"
#include "ao/eval/clause.hpp"

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
