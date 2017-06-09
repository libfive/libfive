#pragma once

#include <QObject>

#include "ao/tree/tree.hpp"

class Shape : public QObject
{
    Q_OBJECT
public:
    Shape(Kernel::Tree t) : tree(t) { /* Nothing to do here */ }
    void draw(const QMatrix4x4& M);
protected:
    Kernel::Tree tree;
};
