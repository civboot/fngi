#########################################
# Parser
#
# With what is in types.py we should have enough to compile the stage0
# language.
#
# First we are going to build the parser, which is fairly self-explanatory. We
# parse and emit lexemes (sometimes also called tokens).

import dataclasses
import enum
import os
import io

from typing import List

## Lexemes
class LexemeVariant(enum.Enum):
    # TODO: use solid integer values
    EOF = enum.auto()

    # Multi char symbols
    SINGLE_MACRO = enum.auto() # !
    DOUBLE_MACRO = enum.auto() # !!
    ARROW = enum.auto() # ->

    # Single char symbols
    SEMICOLON = enum.auto() # ;
    COLON = enum.auto() # :
    EQUAL = enum.auto() # =
    REF = enum.auto() # &
    DEREF = enum.auto() # @
    BLOCK_OPEN = enum.auto() # (
    BLOCK_CLOSE = enum.auto() # )
    DATA_OPEN = enum.auto() # {
    DATA_CLOSE = enum.auto() # }
    TYPE_OPEN = enum.auto() # [
    TYPE_CLOSE = enum.auto() # ]

    # Keywords
    FN = enum.auto() # fn
    LET = enum.auto() # let
    RETURN = enum.auto() # return

    # Contains Direct Data
    INVALID = enum.auto()
    NUMBER = enum.auto()
    IDEN = enum.auto()
    LINE_COMMENT = enum.auto() # //
    ESCAPED_STR = enum.auto() # \"


@dataclasses.dataclass
class Lexeme(object):
    variant: LexemeVariant
    string: str = None

EOF = Lexeme(LexemeVariant.EOF)

SINGLE_MACRO = Lexeme(LexemeVariant.SINGLE_MACRO)
DOUBLE_MACRO = Lexeme(LexemeVariant.DOUBLE_MACRO)
ARROW = Lexeme(LexemeVariant.ARROW)

SEMICOLON = Lexeme(LexemeVariant.SEMICOLON)
COLON = Lexeme(LexemeVariant.COLON)
EQUAL = Lexeme(LexemeVariant.EQUAL)
REF = Lexeme(LexemeVariant.REF)
DEREF = Lexeme(LexemeVariant.DEREF)
BLOCK_OPEN = Lexeme(LexemeVariant.BLOCK_OPEN)
BLOCK_CLOSE = Lexeme(LexemeVariant.BLOCK_CLOSE)
DATA_OPEN = Lexeme(LexemeVariant.DATA_OPEN)
DATA_CLOSE = Lexeme(LexemeVariant.DATA_CLOSE)
TYPE_OPEN = Lexeme(LexemeVariant.TYPE_OPEN)
TYPE_CLOSE = Lexeme(LexemeVariant.TYPE_CLOSE)

FN = Lexeme(LexemeVariant.FN)
LET = Lexeme(LexemeVariant.LET)
RETURN = Lexeme(LexemeVariant.RETURN)


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

def isNameChar(c: int) -> bool:
    return not isWhitespace(c) and not isSymbol(c)


class Scanner(object):
    """The scanner reads into a 32 byte buffer and provides nextByte() and
    backByte() methods.

    The scanner only allows going back by 1 byte before progressing again.
    Any more may throw an error.

    The scanner also tracks the filePos, line, and column.

    Obviously there are python classes that can do this for us, but we
    want something that we can emulate in the forth implementation.
    """
    BUF_SIZE = 32
    LAST_COL_SENTINEL = 0xFFFFFFFF

    def __init__(self, fo: io.TextIOWrapper):
        self.fo = fo
        self.buffer = bytearray(self.BUF_SIZE)
        self.bufMax = 0
        self.bufIndex = 0
        self.filePos = 0
        self.lastColumn = self.LAST_COL_SENTINEL
        self.column = 0
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

        self.lastColumn = self.column

        b = self.buffer[self.bufIndex]
        self.bufIndex += 1
        self.filePos += 1
        if b == ord('\n'):
            self.line += 1
            self.column = 0
        else:
            self.column += 1

        return b

    def backByte(self):
        if self.bufIndex == 0:
            raise IndexError("Cannot go back at start.")
        if self.lastColumn == self.LAST_COL_SENTINEL:
            raise IndexError("Cannot go back > 1 byte")
        self.bufIndex -= 1
        self.filePos -= 1
        self.column = self.lastColumn
        self.lastColumn = self.LAST_COL_SENTINEL
        if self.buffer[self.bufIndex] == ord(b'\n'):
            self.line -= 1


class Lexer(object):
    def __init__(self, fo: io.TextIOWrapper):
        fo.seek(0)
        self.scanner = Scanner(fo)

        self.fo = fo
        self.fileSize = os.stat(fo.fileno()).st_size
        self.tokenIndex = 0
        self.lineno = 0

    def nextLexeme(self) -> Lexeme:
        sc = self.scanner
        c = sc.nextByte()
        while isWhitespace(c):
            if c == b'': break
            c = sc.nextByte()

        if c == 0:
            return EOF

        elif c == ord(b'/'):
            c = sc.nextByte()
            if c == ord(b'/'): # LINE_COMMENT
                out = bytearray(b'//')
                while c != 0 and c != ord(b'\n'):
                    c = sc.nextByte()
                    out.append(c)
                return Lexeme(LexemeVariant.LINE_COMMENT, out)
            else:
                self.backByte()
                return Lexeme(LexemeVariant.INVALID, c)

        elif c == ord(b'!'):
            c = sc.nextByte()
            if c == ord(b'!'):
                return DOUBLE_MACRO
            sc.backByte()
            return SINGLE_MACRO

        elif c == ord(b'-'):
            c = sc.nextByte()
            if c == ord(b'>'):
                return ARROW
            sc.backByte()
            return Lexeme(LexemeVariant.INVALID, b'-')

        elif c == ord(b'\\'): # Escape, must be \"
            out = bytearray([c])
            c = sc.nextByte()
            out.append(c)
            if c != ord(b'"'):
                return Lexeme(LexemeVariant.INVALID, out)

            while True:
                c = sc.nextByte()
                out.append(c)
                if c == ord(b'\\'):
                    c = sc.nextByte()
                    out.append(c)
                    if c == ord(b'"'):
                        break

            return Lexeme(LexemeVariant.ESCAPED_STR, out)

        elif ord('0') <= c <= ord('9'):
            out = bytearray([c])

            c = sc.nextByte()
            if isNumber(c):
                out.append(c)
            elif isNameChar(c):
                sc.backByte()
                out.extend(b' followed by nameChar')
                return Lexeme(LexemeVariant.INVALID, out)
            else:
                sc.backByte()
                return Lexeme(LexemeVariant.NUMBER, out)

        elif c == ord(b';'): return SEMICOLON
        elif c == ord(b':'): return COLON
        elif c == ord(b'='): return EQUAL
        elif c == ord(b'&'): return REF
        elif c == ord(b'@'): return DEREF
        elif c == ord(b'('): return BLOCK_OPEN
        elif c == ord(b')'): return BLOCK_CLOSE
        elif c == ord(b'{'): return DATA_OPEN
        elif c == ord(b'}'): return DATA_CLOSE
        elif c == ord(b'['): return TYPE_OPEN
        elif c == ord(b']'): return TYPE_CLOSE
        elif isSymbol(c): return Lexeme(LexemeVariant.INVALID, bytearray([c]))

        name = bytearray([c])
        while True:
            c = sc.nextByte()
            if not isNameChar(c):
                sc.backByte()
                break
            name.append(c)

        if name == b'fn': return FN
        if name == b'let': return LET
        if name == b'return': return RETURN

        return Lexeme(LexemeVariant.IDEN, name)

    def getAllLexemes(self) -> List[Lexeme]:
        """Used in testing."""
        out = []
        while True:
            token = self.nextLexeme()
            out.append(token)
            if token is EOF:
                return out

    def getAllVariants(self) -> List[LexemeVariant]:
        return [t.variant for t in self.getAllLexemes()]

def testLexer():
    ts = Lexer(open("test_data/hello_world.fn", 'rb', buffering=0))
    result = ts.getAllVariants()
    tv = LexemeVariant
    expected = [
        # let HELLO: &Str = \"Hello world!\";
        tv.LET, tv.IDEN, tv.COLON, tv.REF, tv.IDEN,
        # = \"Hello world!\"; 
        tv.EQUAL, tv.ESCAPED_STR, tv.SEMICOLON,
        # fn main:
        tv.FN, tv.IDEN, tv.COLON, 
        #  [] -> []
        tv.TYPE_OPEN, tv.TYPE_CLOSE, tv.ARROW, tv.TYPE_OPEN, tv.TYPE_CLOSE,
        # ( fdWriteString {
        tv.BLOCK_OPEN, tv.IDEN, tv.DATA_OPEN,
        # fd = 1; // stdout
        tv.IDEN, tv.EQUAL, tv.NUMBER, tv.SEMICOLON, tv.LINE_COMMENT,
        # str = HELLO;
        tv.IDEN, tv.EQUAL, tv.IDEN, tv.SEMICOLON,
        # }; )
        tv.DATA_CLOSE, tv.SEMICOLON, tv.BLOCK_CLOSE,
        tv.EOF,
    ]
    assert expected == result


if __name__ == '__main__':
    def printLexemes(fo):
        ts = Lexer(fo)
        token = None
        while token is not EOF:
            token = ts.nextLexeme()
            print(token)

    printLexemes(open(sys.argv[1], 'rb', buffering=0))

