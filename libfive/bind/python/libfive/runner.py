'''
Python bindings to the libfive CAD kernel
Copyright (C) 2021  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
'''

import ast
import numbers

class VarTransformer(ast.NodeTransformer):
    def __init__(self):
        self._i = 0

    def visit_Call(self, node):
        if isinstance(node.func, ast.Name) and node.func.id == 'var':
            # TODO: inject these errors into the script, so that they end
            # up producing a reasonable backtrace.
            if len(node.args) != 1 or node.keywords:
                raise RuntimeError("var() must take 1 argument")
            elif not isinstance(node.args[0], ast.Constant):
                raise RuntimeError("var() must take a constant argument")
            elif not isinstance(node.args[0].value, numbers.Number):
                raise RuntimeError("var() argument must be a number")

            # We'll be injecting more arguments into the var() call, but don't
            # want to mess up tagged lines and columns, so we'll use a set of
            # dummy fields that indicate that the new arguments have size 0.
            dummy = {
                'lineno': node.end_lineno,
                'end_lineno': node.end_lineno,
                'col_offset': node.end_col_offset,
                'end_col_offset': node.end_col_offset,
            }

            # Each var gets a tag explaining where to find it in the text,
            # and is deduplicated based on order in the AST.
            node.args.append(ast.Constant(value=(
                node.lineno, node.end_lineno,
                node.col_offset, node.end_col_offset), **dummy))
            self._i += 1
        return node

def run(s, **env):
    ''' Evaluates a string, clause-by-clause.

        Returns a list of values from each expression in the string, or
        raises an error if something went wrong.
    '''
    parsed = ast.parse(s)
    tagged = VarTransformer().generic_visit(parsed)
    gs = {**env}
    out = []
    for p in tagged.body:
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
