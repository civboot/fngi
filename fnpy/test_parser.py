from .parser import *

def unwrap(ast: ASTNode) -> List[any]:
    """Takes a single node and "unwrapps" it into a list of comparable data types
    that we can easily write assertions for.
    """
    out = []
    if isinstance(ast, Number):
        return [ast.value]
    elif isinstance(ast, PrimaryBytes):
        return [b'{}::{}'.format(ast.variant.variant.name, ast.value)]
    elif isinstance(ast, LabelStmt):
        if ast.label:
            out.append(b'#' + ast.label)
        out.extend(unwrap(ast.stmt))
        return out


def unroll(asts: List[ASTNode]) -> List[ASTNode]:
    out = []
    for a in asts:
        out.extend(unwrap(a))
    return out
    
def testParseNumbers():
    ll = FakeLexemeLL(b'42; 33; 009')
    stmts = parseFile(ll)
    assert [42, 33, 9] == unroll(stmts)
