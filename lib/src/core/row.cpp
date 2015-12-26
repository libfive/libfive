#include "ao/core/row.hpp"
#include "ao/core/atom.hpp"

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
    active += disabled.top();
    disabled.pop();
}

void Row::disable(size_t i)
{
    assert(i < active);
    assert(disabled.size() > 0);

    // Put a sane value in the entire result array
    (*this)[i]->result.fill((*this)[i]->result.get<Interval>(0).lower());

    // Swap the atom to the back of the list, decrementing active
    // Since we only evaluate from 0 to active, this disables the atom
    std::swap((*this)[i], (*this)[--active]);
    disabled.top()++;
}

void Row::setSize()
{
    active = size();
}
