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

    def check_var(self, node):
        if len(node.args) != 1 or node.keywords:
            return "var() must take 1 argument"
        if isinstance(node.args[0], ast.Constant):
            if not isinstance(node.args[0].value, numbers.Number):
                return "var() argument must be a number"
            else:
                pass # Numerical constant
        elif isinstance(node.args[0], ast.UnaryOp):
            if not isinstance(node.args[0].op, ast.USub):
                return "var() argument must be a constant"
            elif not isinstance(node.args[0].operand, ast.Constant):
                return "var() argument must be a constant"
            elif not isinstance(node.args[0].operand.value, numbers.Number):
                return "var() argument must be a number"
            else:
                pass # Unary subtraction
        else:
            return "var() argument must be a constant"

    def visit_Call(self, node):
        if isinstance(node.func, ast.Name) and node.func.id == 'var':
            # We'll be injecting more arguments into the var() call, but don't
            # want to mess up tagged lines and columns, so we'll use a set of
            # dummy fields that indicate that the new arguments have size 0.
            dummy = {
                'lineno': node.end_lineno,
                'end_lineno': node.end_lineno,
                'col_offset': node.end_col_offset,
                'end_col_offset': node.end_col_offset,
            }

            # Patch the call to be var('error string') if the AST is invalid.
            # This is checked by the var function and re-raised as a
            # RuntimeError, rather than raising it here (which would produce
            # a confusing traceback).
            err = self.check_var(node)
            if err:
                node.args[0] = ast.Constant(value=err, **dummy)
            else:
                # Each var gets a tag explaining where to find it in the text,
                # and is deduplicated based on order in the AST.
                node.args.append(ast.Constant(value=(
                    node.lineno, node.end_lineno,
                    node.col_offset + 4, node.end_col_offset), **dummy))
            self._i += 1
        return self.generic_visit(node) # Recurse, e.g. to patch sphere(var(1))

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
        else:
            mod = ast.Module([p])
            mod.type_ignores = []
            f = compile(mod, '<file>', 'exec')
            r = exec(f, gs)
        out.append(r)
    return out
