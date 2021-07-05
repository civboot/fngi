import re
import parsimonious
from parsimonious.grammar import Grammar

def readf(path):
    with open(path) as f:
        return f.read()

grammarTxt = readf('grammar/full_grammar.peg')

RAW_REGEX = re.compile(r'"[^"]*"')

def testRawString():
    source = r'''"foo bar 'baz' \nbob"'''
    assert source == RAW_REGEX.match(source).group(0)

def getGrammar():
    return Grammar(grammarTxt)

def testFullGrammar():
    grammar = getGrammar()

def testParseFull():
    txt = readf('grammar/simple.fn')
    grammar = getGrammar()
    grammar.parse(txt)

def testTry():
    g = Grammar(readf('grammar/try_grammar.peg'))
    g.parse(readf('grammar/try.fn'))

