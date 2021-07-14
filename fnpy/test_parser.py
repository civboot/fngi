from .parser import *

def testParseNumbers():
    ll = FakeLexemeLL(b'42; 33; 009')
    stmts = parseFile(ll)
    assert [42, 33, 9] == unrollAST(stmts)
