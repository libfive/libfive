'''
Python bindings to the libfive CAD kernel
Copyright (C) 2021  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
'''

import ast

def run(s):
    ''' Evaluates a string, clause-by-clause.

        Returns a list of values from each expression in the string, or
        raises an error if something went wrong.
    '''
    parsed = ast.parse(s)
    # TODO: hotpatch var(...) nodes
    gs = {}
    out = []
    for p in parsed.body:
        if isinstance(p, ast.Expr):
            exp = ast.Expression(p.value)
            f = compile(exp, '<file>', 'eval')
            r = eval(f, gs)
            if r is not None:
                out.append(r)
        else:
            mod = ast.Module([p])
            mod.type_ignores = []
            f = compile(mod, '<file>', 'exec')
            exec(f, gs)
    return out

