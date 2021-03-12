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
        Returns a list of (okay, error-or-value) tuples
    '''
    try:
        parsed = ast.parse(s)
    except SyntaxError as e:
        return [(False, e)]
    # TODO: hotpatch var(...) nodes
    gs = {}
    out = []
    for p in parsed.body:
        if isinstance(p, ast.Expr):
            exp = ast.Expression(p.value)
            f = compile(exp, '<file>', 'eval')
            try:
                r = eval(f, gs)
                if r is not None:
                    out.append((True, r))
            except Exception as e:
                out.append((False, e))
        else:
            mod = ast.Module([p])
            mod.type_ignores = []
            f = compile(mod, '<file>', 'exec')
            try:
                exec(f, gs)
            except Exception as e:
                out.append((False, e))
    return out

