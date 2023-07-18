require('gciv')
local gen
test('load', nil, function()
  gen = dofile('./etc/gen.lua')
end)

local CONST_EXAMPLE = [[
const FOO: U1 = 42
const BAR: U1 = 33
]]

local STRUCT_EXAMPLE = [=[
struct Foo [ a: U2;  b: &S ]
]=]

test('const', nil, function()
  local expected = Map{
    FOO=gen.FConst{name="FOO", ty="U1", value="42"},
    BAR=gen.FConst{name="BAR", ty="U1", value="33"}, }
  local result = gen.parseConsts(CONST_EXAMPLE)
  assertEq(expected, result)
end)

test('struct', nil, function()
  local _, _, stName, stBody = string.find(
    STRUCT_EXAMPLE, gen.STRUCT_PAT)
  assertEq("Foo", stName)
  assertEq("[ a: U2;  b: &S ]", stBody)

  local _, _, fName, fRefs, fTy = string.find(stBody, gen.FIELD_PAT)
  assertEq("a", fName);
  assertEq("", fRefs);
  assertEq("U2", fTy)
end)


test('parse struct', nil, function()
  local expected = Map{Foo=gen.FStruct{
    name='Foo',
    fields=List{
      gen.FField{name='a', refs=0, ty='U2'},
      gen.FField{name='b', refs=1, ty='S'},
    },
  }}
  local result = gen.parseStructs(STRUCT_EXAMPLE)
  assertEq(expected, result)
end)

local function assertConst(name, ty, value, consts)
  assertEq(
   gen.FConst{name=name, ty=ty, value=value},
   consts[name])
end

local function assertStruct(structs, name, repr)
  assertEq(repr, tostring(structs[name]))
end

test('spor', nil, function()
  assertConst('INC',  'U1', '0x10', gen.spor.consts)
  assertConst('NOT',  'U1', '0x16', gen.spor.consts)
  assertConst('SLIC', 'U1', '0x86', gen.spor.consts)
end)

test('dat', nil, function()
  assertConst('INFO',  'U1', '0x04',   gen.dat.consts)
  assertConst('SIZE',  'U2', '0x1000', gen.dat.consts)
  assertStruct(gen.datStructs, 'Slc',
    'FStruct{name=Slc fields=[{name=dat refs=1 ty=U1}'
    .. ' {name=len refs=0 ty=U2}]}')
  assert(gen.datStructs.Block)
  assert(gen.datStructs.Buf)
  assert(not gen.datStructs.TyDb)
  assert(not gen.datStructs.InOut)
  assert(not gen.datStructs.Globals)
end)

for k, v in pairs(gen.compStructs) do
  print(k, v)
end

test('comp', nil, function()
  assertConst('INFO',  'U1', '0x04',   gen.dat.consts)
  assertConst('FN_STATE_NO', 'U4', '0x0000', gen.dat.consts)
  assertConst('TY_FN', 'U1', '0x80', gen.dat.consts)
  assertConst('TY_FN_INLINE', 'U1', '0x04', gen.dat.consts)
  assert(not gen.compStructs.Block)
  assert(not gen.compStructs.Buf)
  assert(gen.compStructs.TyDb)
  assert(gen.compStructs.InOut)
  assert(gen.compStructs.Globals)
end)


