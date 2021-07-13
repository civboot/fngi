#########################################
# Parser
#
# With what is in types.py we should have enough to compile the stage0
# language.
#
# First we are going to build the parser, which is fairly self-explanatory. We
# parse and emit lexemes (sometimes also called tokens).

import copy
import dataclasses
import enum
import os
import io
import pprint

from typing import List

## Lexemes
class LexemeVariant(enum.Enum):
    # TODO: use solid integer values
    EOF = enum.auto()
    INVALID = enum.auto()

    # Values
    LINE_COMMENT = enum.auto() # //
    BLOCK_COMMENT = enum.auto() # /* ... */
    NUMBER = enum.auto()
    IDEN = enum.auto()
    ESC_STR = enum.auto() # \"
    RAW_STR = enum.auto() # "

    # Symbol Operators (Grammar Order)
    MACRO1 = enum.auto() # !
    MACRO2 = enum.auto() # !!
    MINUS = enum.auto() # -
    REF = enum.auto() # &
    DEREF = enum.auto() # @
    CALL = enum.auto() # $
    DIVIDE = enum.auto() # /
    MULTIPLY = enum.auto() # *
    PLUS = enum.auto() # +
    GE = enum.auto() # >=
    GT = enum.auto() # >
    LE = enum.auto() # <=
    LT = enum.auto() # <
    NE = enum.auto() # !=
    EQ = enum.auto() # ==
    SET = enum.auto() # :=

    SEMICOLON = enum.auto() # ;
    COLON = enum.auto() # :
    EQUAL = enum.auto() # =
    BLOCK_OPEN = enum.auto() # (
    BLOCK_CLOSE = enum.auto() # )
    DATA_OPEN = enum.auto() # {
    DATA_CLOSE = enum.auto() # }
    TYPE_OPEN = enum.auto() # [
    TYPE_CLOSE = enum.auto() # ]

    # Type operators
    ARROW = enum.auto() # ->
    DOT = enum.auto() # .
    LABEL = enum.auto() # #

    # Keywords
    FN = enum.auto()
    DO = enum.auto()
    STRUCT = enum.auto()
    ENUM = enum.auto()
    IMPL = enum.auto()
    STK = enum.auto()
    ARR = enum.auto()

    IF = enum.auto()
    ELIF = enum.auto()
    ELDO = enum.auto()
    SWITCH = enum.auto()
    CASE = enum.auto()
    WHILE = enum.auto()

    NOT = enum.auto()
    AND = enum.auto()
    OR = enum.auto()
    BITNOT = enum.auto()
    BITOR = enum.auto()
    BITXOR = enum.auto()
    BITAND = enum.auto()

    CONT = enum.auto()
    BREAK = enum.auto()
    RETURN = enum.auto()




@dataclasses.dataclass
class Lexeme:
    variant: LexemeVariant
    text: str = None


EOF = Lexeme(LexemeVariant.EOF)

MACRO1 = Lexeme(LexemeVariant.MACRO1)
MACRO2 = Lexeme(LexemeVariant.MACRO2)
MINUS = Lexeme(LexemeVariant.MINUS)
REF = Lexeme(LexemeVariant.REF)
DEREF = Lexeme(LexemeVariant.DEREF)
CALL = Lexeme(LexemeVariant.CALL)
DIVIDE = Lexeme(LexemeVariant.DIVIDE)
MULTIPLY = Lexeme(LexemeVariant.MULTIPLY)
PLUS = Lexeme(LexemeVariant.PLUS)
GE = Lexeme(LexemeVariant.GE)
GT = Lexeme(LexemeVariant.GT)
LE = Lexeme(LexemeVariant.LE)
LT = Lexeme(LexemeVariant.LT)
NE = Lexeme(LexemeVariant.NE)
EQ = Lexeme(LexemeVariant.EQ)
SET = Lexeme(LexemeVariant.SET)

SEMICOLON = Lexeme(LexemeVariant.SEMICOLON)
COLON = Lexeme(LexemeVariant.COLON)
EQUAL = Lexeme(LexemeVariant.EQUAL)
BLOCK_OPEN = Lexeme(LexemeVariant.BLOCK_OPEN)
BLOCK_CLOSE = Lexeme(LexemeVariant.BLOCK_CLOSE)
DATA_OPEN = Lexeme(LexemeVariant.DATA_OPEN)
DATA_CLOSE = Lexeme(LexemeVariant.DATA_CLOSE)
TYPE_OPEN = Lexeme(LexemeVariant.TYPE_OPEN)
TYPE_CLOSE = Lexeme(LexemeVariant.TYPE_CLOSE)

ARROW = Lexeme(LexemeVariant.ARROW)
DOT = Lexeme(LexemeVariant.DOT)
LABEL = Lexeme(LexemeVariant.LABEL)

FN = Lexeme(LexemeVariant.FN)
DO = Lexeme(LexemeVariant.DO)
STRUCT = Lexeme(LexemeVariant.STRUCT)
ENUM = Lexeme(LexemeVariant.ENUM)
IMPL = Lexeme(LexemeVariant.IMPL)
STK = Lexeme(LexemeVariant.STK)
ARR = Lexeme(LexemeVariant.ARR)

IF = Lexeme(LexemeVariant.IF)
ELIF = Lexeme(LexemeVariant.ELIF)
ELDO = Lexeme(LexemeVariant.ELDO)
SWITCH = Lexeme(LexemeVariant.SWITCH)
CASE = Lexeme(LexemeVariant.CASE)
WHILE = Lexeme(LexemeVariant.WHILE)

NOT = Lexeme(LexemeVariant.NOT)
AND = Lexeme(LexemeVariant.AND)
OR = Lexeme(LexemeVariant.OR)
BITNOT = Lexeme(LexemeVariant.BITNOT)
BITOR = Lexeme(LexemeVariant.BITOR)
BITXOR = Lexeme(LexemeVariant.BITXOR)
BITAND = Lexeme(LexemeVariant.BITAND)

CONT = Lexeme(LexemeVariant.CONT)
BREAK = Lexeme(LexemeVariant.BREAK)
RETURN = Lexeme(LexemeVariant.RETURN)

################################
# Scanner
# The scanner reads into a 32 byte buffer and provides nextByte() and
# backByte() methods.
#
# The scanner only allows going back by 1 byte before progressing again.
# Any more may throw an error. The scanner also tracks the filePos, line, and
# col.
#
# Obviously there are python classes that can do this for us, but we
# want something that we can emulate in the forth implementation.

class Scanner(object):
    BUF_SIZE = 32
    LAST_COL_SENTINEL = 0xFFFFFFFF

    def __init__(self, fo: io.TextIOWrapper):
        self.fo = fo
        self.buffer = bytearray(self.BUF_SIZE)
        self.bufMax = 0
        self.bufIndex = 0
        self.filePos = 0
        self.lastCol = self.LAST_COL_SENTINEL
        self.col = 0
        self.line = 0

    def nextByte(self) -> int:
        if self.bufIndex == self.bufMax:
            if self.bufMax != 0:
                # Move the last character to the start to allow backByte()
                self.buffer[0] = self.buffer[self.bufMax -1]
                self.bufIndex = 1
            else:
                self.bufIndex = 0 # no byte to go back to

            # try to fill the buffer
            readBytes = self.fo.read(self.BUF_SIZE - 1)
            self.bufMax = self.bufIndex + len(readBytes)
            self.buffer[self.bufIndex:self.bufMax] = readBytes

        if self.bufIndex == self.bufMax:
            return 0 # EOF

        self.lastCol = self.col

        b = self.buffer[self.bufIndex]
        self.bufIndex += 1
        self.filePos += 1
        if b == ord('\n'):
            self.line += 1
            self.col = 0
        else:
            self.col += 1

        return b

    def backByte(self):
        if self.bufIndex == 0:
            raise IndexError("Cannot go back at start.")
        if self.lastCol == self.LAST_COL_SENTINEL:
            raise IndexError("Cannot go back > 1 byte")
        self.bufIndex -= 1
        self.filePos -= 1
        self.col = self.lastCol
        self.lastCol = self.LAST_COL_SENTINEL
        if self.buffer[self.bufIndex] == ord('\n'):
            self.line -= 1

################################
# Lexer
# The lexer returns a stream of Lexemes, which must be stored by another data
# structure. Complex tokens like numbers, full identifiers and strings are
# handled directly.

def isWhitespace(c: int) -> bool:
    if c == 0: return False
    return c <= ord(' ')

def isSymbol(c: int) -> bool:
    return (ord('!') <= c <= ord('/') or
            ord(':') <= c <= ord('@') or
            ord('[') <= c <= ord('`') or
            ord('{') <= c <= ord('~'))

def isNumber(c: int) -> bool:
    return ord('0') <= c <= ord('9')

def isHex(c: int) -> bool:
    return (
        isNumber(c)
        or (ord('A') <= c <= ord('F'))
        or (ord('a') <= c <= ord('f')))

def isNameChar(c: int) -> bool:
    return not isWhitespace(c) and not isSymbol(c)


class Lexer(object):
    def __init__(self, fo: io.TextIOWrapper):
        fo.seek(0)
        self.scanner = Scanner(fo)

        self.fo = fo
        self.fileSize = os.stat(fo.fileno()).st_size
        self.tokenIndex = 0

    def nextLexeme(self) -> Lexeme:
        sc = self.scanner
        c = sc.nextByte()
        while isWhitespace(c):
            if c == b'': break
            c = sc.nextByte()

        if c == 0:
            return EOF

        elif c == ord('/'):
            c = sc.nextByte()
            if c == ord('/'): # LINE_COMMENT
                out = bytearray(b'/')
                while True:
                    if c == 0: break
                    if c == ord('\n'):
                        sc.backByte()
                        break
                    out.append(c)
                    c = sc.nextByte()
                return Lexeme(LexemeVariant.LINE_COMMENT, out)
            elif c == ord('*'): # BLOCK_COMMENT
                out = bytearray(b'/*')
                lastWasStar = False
                while True:
                    c = sc.nextByte()
                    out.append(c)
                    if c == '*':
                        lastWasStar = True
                    elif c == 0 or (lastWasStar and c == '/'):
                        return Lexeme(LexemeVariant.BLOCK_COMMENT, out)
                    else:
                        lastWasStar = False

            else:
                return DIVIDE

        elif c == ord('"'):
            out = bytearray()
            c = sc.nextByte()
            while True:
                if c == 0:
                    return Lexeme(LexemeVariant.INVALID, out)
                out.append(c)
                if c == ord('"'):
                    break
                c = sc.nextByte()
            return Lexeme(LexemeVariant.RAW_STR, out)

        elif c == ord('!'):
            c = sc.nextByte()
            if c == ord('='):
                return NE
            elif c == ord('!'):
                return MACRO2
            sc.backByte()
            return MACRO1

        elif c == ord('-'):
            c = sc.nextByte()
            if c == ord('>'):
                return ARROW
            return MINUS

        elif c == ord('>'):
            c = sc.nextByte()
            if c == ord('='):
                return GE
            sc.backByte()
            return GT

        elif c == ord('<'):
            c = sc.nextByte()
            if c == ord('='):
                return LE
            sc.backByte()
            return LT

        elif c == ord(':'):
            c = sc.nextByte()
            if c == ord('='):
                return SET
            sc.backByte()
            return COLON

        elif c == ord('\\'): # Escape, must be \"
            out = bytearray([c])
            c = sc.nextByte()
            out.append(c)
            if c != ord('"'):
                return Lexeme(LexemeVariant.INVALID, out)

            while True:
                c = sc.nextByte()
                out.append(c)
                if c == ord('\\'):
                    c = sc.nextByte()
                    out.append(c)
                    if c == ord('"'):
                        break

            return Lexeme(LexemeVariant.ESC_STR, out)

        elif ord('0') <= c <= ord('9'):
            out = bytearray([c])
            c = sc.nextByte()

            numIsHex = False
            if c == ord('x'):
                numIsHex = True
                out.append(c)
                c = sc.nextByte()

            if numIsHex and isHex(c):
                out.append(c)
            elif isNumber(c):
                out.append(c)
            elif isNameChar(c):
                sc.backByte()
                out.extend(b' followed by nameChar')
                return Lexeme(LexemeVariant.INVALID, out)
            else:
                sc.backByte()
                return Lexeme(LexemeVariant.NUMBER, out)

        elif c == ord('&'): return REF
        elif c == ord('@'): return DEREF
        elif c == ord('$'): return CALL
        elif c == ord('+'): return PLUS
        elif c == ord('*'): return MULTIPLY

        elif c == ord('.'): return DOT
        elif c == ord('#'): return LABEL
        elif c == ord(';'): return SEMICOLON
        elif c == ord('='): return EQUAL
        elif c == ord('('): return BLOCK_OPEN
        elif c == ord(')'): return BLOCK_CLOSE
        elif c == ord('{'): return DATA_OPEN
        elif c == ord('}'): return DATA_CLOSE
        elif c == ord('['): return TYPE_OPEN
        elif c == ord(']'): return TYPE_CLOSE
        elif isSymbol(c): return Lexeme(LexemeVariant.INVALID, bytearray([c]))

        name = bytearray([c])
        while True:
            c = sc.nextByte()
            if not isNameChar(c):
                sc.backByte()
                break
            name.append(c)

        if name == b'fn': return FN
        if name == b'do': return DO
        if name == b'struct': return STRUCT
        if name == b'enum': return ENUM
        if name == b'impl': return IMPL
        if name == b'stk': return STK
        if name == b'arr': return ARR

        if name == b'if': return IF
        if name == b'elif': return ELIF
        if name == b'eldo': return ELDO
        if name == b'switch': return SWITCH
        if name == b'case': return CASE
        if name == b'while': return WHILE

        if name == b'not': return NOT
        if name == b'and': return AND
        if name == b'or': return OR
        if name == b'bitnot': return BITNOT
        if name == b'bitor': return BITOR
        if name == b'bitand': return BITAND

        if name == b'cont': return CONT
        if name == b'break': return BREAK
        if name == b'return': return RETURN

        return Lexeme(LexemeVariant.IDEN, name)

    def getAllLexemes(self) -> List[Lexeme]:
        """Used in testing."""
        out = []
        while True:
            lexeme = self.nextLexeme()
            out.append((self.scanner.line, lexeme))
            if lexeme is EOF:
                return out

    def getAllVariants(self) -> List[LexemeVariant]:
        return [t.variant for t in self.getAllLexemes()]

def lineVariants(line: int, variants: List[LexemeVariant]):
    return [(line, v) for v in variants]

def testLexer():
    lv = LexemeVariant
    ts = Lexer(open("grammar/hello_world.fn", 'rb', buffering=0))
    result = ts.getAllLexemes()
    resultVariants = [l[1].variant for l in result]
    resultLineVariants = [(l[0], l[1].variant) for l in result]
    expectLineVariants = (
        [(0, lv.LINE_COMMENT)]
        # say: Str = "Hello " + "World";
        + lineVariants(1, [
            lv.IDEN, lv.COLON, lv.IDEN, lv.EQUAL, lv.RAW_STR,
            lv.PLUS, lv.RAW_STR, lv.SEMICOLON])

        # fn helloWorld: [] -> [] do (
        + lineVariants(3, [
            lv.FN, lv.IDEN, lv.COLON, lv.TYPE_OPEN, lv.TYPE_CLOSE,
            lv.ARROW, lv.TYPE_OPEN, lv.TYPE_CLOSE, lv.DO, lv.BLOCK_OPEN])

        # println! say
        + lineVariants(4, [lv.IDEN, lv.MACRO1, lv.IDEN])

        # );
        + lineVariants(5, [lv.BLOCK_CLOSE, lv.SEMICOLON])
        + [(6, lv.EOF)]
    )

    pprint.pprint(result)
    assert lv.INVALID not in resultVariants
    assert expectLineVariants == resultLineVariants


################################
# Lexeme Node List
# For the parser to be implemented it must be able to:
# - Retrieve lexeme nodes that contain metadata (line, col, etc)
# - Do lookahead of at least 1 lexeme, probably more (we will see)
# - Advance the root (dropping nodes) up until a specific node.

@dataclasses.dataclass
class LexemeNode:
    lexeme: Lexeme
    line: int
    col: int
    next: "LexemeNode" = None


class LexemeLL(object):
    """Lexeme linked list."""
    def __init__(self, lexer: Lexer):
        self.lexer = lexer
        self.root = None
        self.last = None
        self.gotEof = False

    def pop(self) -> LexemeNode:
        """Get the next lexeme node and advance the root."""
        if self.root is None:
            self._populateNext()
        out = self.root
        self.root = out.next
        if self.root is None:
            assert self.last == out
            self.last = None
        return out

    def advance(self, node):
        """Advance the root until node."""
        while self.root is not node:
            if self.root is None:
                raise IndexError(node)
            self.pop()

    def peek(self, node = None, index: int = 0) -> LexemeNode:
        """Peek at the Lexeme Node at index relative to node

        If node is None, it is relative to root.
        """
        if node is None:
            if self.root is None and not self.gotEof:
                self._populateNext()
            node = self.root
        prev = node
        ln = prev
        while index > 0:
            ln = prev.next
            if ln is None and not self.gotEof:
                self._populateNext()
                continue
            if ln is None: return None
            prev = ln
            index = index - 1
        return ln

    def _populateNext(self):
        if self.gotEof:
            raise IndexError("Already to EOF")
        lexeme = self.lexer.nextLexeme()
        if lexeme is EOF:
            self.gotEof = True

        node = LexemeNode(
            lexeme,
            self.lexer.scanner.line,
            self.lexer.scanner.col)

        if self.end is None:
            assert self.root is None
            self.root = node
            self.last = node
        else:
            self.last.next = node
            self.last = node
################################
# AST Nodes

class ASTNode(object):
    pass


@dataclasses.dataclass
class Number(ASTNode):
    value: int


@dataclasses.dataclass
class RawStr(ASTNode):
    value: bytes


@dataclasses.dataclass
class Iden(ASTNode):
    value: bytes


@dataclasses.dataclass
class Label(ASTNode):
    value: bytes


################################
# Parser
# The parser implements the syntax defined in grammar/grammar.peg, which is
# structured (ordered) such that it can be transparently implemented by a
# recursive-descent style parser.
#
# The parser consumes the result of the lexer and converts it into AST nodes.

@dataclasses.dataclass
class ParseError:
    msg: str
    line: int
    col: int

    @classmethod
    def new(cls, msg: any, ln: LexemeNode):
        return cls(str(msg), ln.line, ln.col)

PARSE_ERROR: ParseError = None

def updateError(msg, ln: LexemeNode):
    PARSE_ERROR = ParseError.new(msg, ln)

def parseNumber(ll: LexemeLL) -> Number:
    ln = ll.pop()
    assert ln.lexeme.variant is LexemeVariant.NUMBER
    numTxt = ln.lexeme.text
    try:
        if len(numTxt) >= 2 and numTxt[1] == 'x':
            value = int(numTxt, 16)
        else:
            value = int(numTxt)
    except ValueError as e:
        return updateError(e, ln)
    return Number(value)


# TODO parseEscStr(ll: LexmeLL) -> EscStr:
def parseRawStr(ll: LexmeLL) -> RawStr:
    ln = ll.pop()
    assert ln.lexeme.variant is LexemeVariant.RAW_STR
    return RawStr(ln.lexeme.text[1:-1])


@dataclass.dataclass
class ParsePtr:
    ptr = None


# Mutable globals that will be defined later. Callers will use i.e.
# parseStmt.ptr(ll) to call them.
parseStmt = ParsePtr()
parseExpr = ParsePtr()
parseTy = ParsePtr()
parseName = ParsePtr()
parseMacro2 = ParsePtr()


def parseLabel(ll: LexemeLL) -> Label:
    assert ll.pop().lexeme is LABEL
    ln = ll.pop()

    if ln.lexeme.variant is not LexemeVariant.IDEN:
        return updateError("Expected IDEN after '#' for label", ln)

    return Label(ln.text)
