#include <stack>
#include <vector>

class Clause;

/*
 *  The Row subclass stores a row of clauses (of a particular weight)
 *  It's not particularly useful outside of an Evaluator
 */
class Row : public std::vector<Clause*>
{
public:
    /*
     *  Push a new disabled count onto the stack
     */
    void push();

    /*
     *  Re-enable nodes that were disabled since the last push
     */
    void pop();

    /*
     *  Disables the i'th clauses in the list by swapping it to the back
     *  and decrementing the active count by 1
     *
     *  Also increments the disabled count by one; must be called after at
     *  least one push() so that there's something on the disabled stack
     */
    void disable(size_t i);

    /*
     *  Record the row's current size as its active node count
     */
    void setSize();

protected:
    /*  active is the number of clauses to evaluate in each pass  */
    size_t active;

    /*  disabled stores the number of clauses disabled in each call to  *
     *  Row::disable.  It's a stack with values that are popped off     *
     *  with Row::pop                                                   */
    std::stack<size_t> disabled;

    friend class Evaluator;
    friend class Accelerator;
};
