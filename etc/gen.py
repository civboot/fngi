
from ast import literal_eval as lit
import sys
import re
import dataclasses
from typing import List, Tuple

CONST_RE = re.compile(
    r"const\s+(?P<name>\w+)\s*:"
    + r"\s*(?P<ty>\w+)\s*="
    + r"\s*(?P<value>\w+)*")
STRUCT_RE = re.compile(r"struct\s+(?P<name>\w+)\s*\[(?P<body>.*?)\]", re.DOTALL)
_TY = r""
TY_PAT = r"(?P<refs>&*)\s*(?P<ty>\w+)"
FIELD_RE = re.compile(r"(?P<name>\w+)\s*:\s*" + TY_PAT, re.MULTILINE)

assert CONST_RE.search("const foo : U2 = 0x7")

assert STRUCT_RE.search("struct Foo [ a: U2;  b: Arr[4 U2]]").group('name', 'body') == (
    'Foo', ' a: U2;  b: Arr[4 U2')

NATIVE_TYS = {"U1", "U2", "U4", "S", "I1", "I2", "I4"}

@dataclasses.dataclass
class Const:
  name: str; ty: str; value: str

  def isNative(self):
    return self.ty in NATIVE_TYS

@dataclasses.dataclass
class TyI:
  refs: int; ty: str

@dataclasses.dataclass
class Field:
  name: str
  refs: int
  ty: str

@dataclasses.dataclass
class Struct:
  name: str;
  fields: List[Field]

  def clang(self, f):
    f.write('typedef struct {\n')
    for field in self.fields:
      refs = '*' * field.refs
      f.write(f'{field.ty}{refs} {field.name};\n')
    f.write(f'}} {self.name};\n')

def parseTyI(s: str):
  refs, ty = TYI_RE.match(s).group("refs", "ty")
  refs = int(refs) if refs else 0
  return TyI(refs, ty)

@dataclasses.dataclass
class Parsed:
  consts: List[Tuple[str, int]]
  structs: List[Struct]

def parseFngi(path):
  with open(path) as f: text = f.read()
  parsed = [Const(*m.group("name", "ty", "value")) for m in CONST_RE.finditer(text)]
  consts = [(c.name, lit(c.value)) for c in parsed if c.isNative()]

  structs = []
  for sm  in STRUCT_RE.finditer(text):
    fields = []
    for m in FIELD_RE.finditer(sm.group('body')):
      name, refs, ty = m.group("name", "refs", "ty")
      fields.append(Field(name, len(refs) if refs else 0, ty))
    structs.append(Struct(sm.group('name'), fields))

  return Parsed(consts = consts, structs=structs)

def writeConsts(f, path, consts):
  f.write(f"// GENERATED BY etc/gen.py FROM {path}\n// DO NOT EDIT MANUALLY\n\n")
  for name, value in consts:
    f.write(f"#define {name:<16} {hex(value)}\n")

sporPath = 'src/spor.fn'; spor = parseFngi(sporPath)
with open("gen/spor.h", "w") as f: writeConsts(f, sporPath, spor.consts)

sz = {name: value for (name, value) in spor.consts if name.startswith('SZ')}
instrs = [(instr, name) for (name, instr) in spor.consts if name not in sz]
instrs.sort()

sporConsts = dict(spor.consts)
SLIT = sporConsts['SLIT']

SZ_MASK = sporConsts['SZ_MASK']
SZ1 = sz['SZ1']
SZ2 = sz['SZ2']
SZ4 = sz['SZ4']

unsized = {'LCL', 'XL', 'XW', 'XLL', 'XRL', 'SLIT'}

def writeCase(f, name, ret=None):
  ret = ret or name
  f.write(f'    case {name:<16}: return Slc_ntLit("{ret}");\n')

def withSize(f, name, instr):
  for sz in ('1', '2', '4'):
    writeCase(f, f'{name} + SZ{sz}', name + sz)

def slitCases(f):
  for v in range(0, 0x30):
    writeCase(f, f'SLIT + 0x{v:X}', f'{{0x{v:02X}}}')

with open('gen/name.c', 'w') as f:
  f.write('/* Custom generated by etc/gen.py */\n\n')
  f.write('#include "civ.h"\n')
  f.write('#include "spor.h"\n\n')
  f.write('/*extern*/ U1* unknownInstr = "UNKNOWN";\n\n')
  f.write('Slc instrName(U1 instr) {\n')
  f.write('  switch(instr) {\n')
  for instr, name in instrs:
    if name == 'SLIT': slitCases(f)
    elif instr < 0x40 or name in unsized: writeCase(f, name)
    else:                                 withSize(f, name, instr)

  f.write('  }\n')
  f.write('  return (Slc) {.dat = unknownInstr, .len = 7};\n')
  f.write('}\n')



from pprint import pprint
datPath = 'src/dat.fn'; dat = parseFngi(datPath)
pprint(dat.structs)

compPath = 'src/comp.fn'; comp = parseFngi(compPath)
with open("gen/comp.h", "w") as f:
  writeConsts(f, compPath, comp.consts)

