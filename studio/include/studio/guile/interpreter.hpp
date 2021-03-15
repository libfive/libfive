/*
Studio: a simple GUI for the libfive CAD kernel
Copyright (C) 2021  Matt Keeter

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#pragma once
#include <libguile.h>

#include "studio/interpreter.hpp"

namespace Studio {
namespace Guile {

class Interpreter: public ::Studio::Interpreter {
public:
    Interpreter();
    ~Interpreter();

    void init() override;

    QString defaultScript() override;
    QString extension() override { return ".scm"; }

public slots:
    void eval(QString s) override;

protected:
    /*  Lots of miscellaneous Scheme objects, constructed once
     *  during init() so that we don't need to build them over
     *  and over again at runtime */
    SCM scm_eval_sandboxed;
    SCM scm_shape_to_ptr;
    SCM scm_is_shape;

    SCM scm_port_eof_p;
    SCM scm_valid_sym;
    SCM scm_syntax_error_sym;
    SCM scm_numerical_overflow_sym;

    SCM scm_syntax_error_fmt;
    SCM scm_numerical_overflow_fmt;
    SCM scm_other_error_fmt;
    SCM scm_result_fmt;
    SCM scm_in_function_fmt;

    /*  Unprotects all of the SCM objects above.  This function must be called
     *  from a thread in Guile mode, e.g. using scm_with_guile */
    void unprotectGuileVars();
    static void* unprotectGuileVars(void* t);
};
}   // namespace Guile
}   // namespace Studio
