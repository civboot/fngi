TESTING = true
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
  local expected = List{
    gen.FConst{name="FOO", ty="U1", value="42"},
    gen.FConst{name="BAR", ty="U1", value="33"}, }
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
  local expected = List{gen.FStruct{
    name='Foo',
    fields=List{
      gen.FField{name='a', refs=0, ty='U2'},
      gen.FField{name='b', refs=1, ty='S'},
    },
  }}
  local result = gen.parseStructs(STRUCT_EXAMPLE)
  assertEq(expected, result)
end)

local function nameMap(l)
  return Map.from(l, function(_, v) return v.name, v end)
end
local sporConsts = nameMap(gen.spor.consts)
local datConsts  = nameMap(gen.dat.consts)
local compConsts = nameMap(gen.comp.consts)

local sporStructs = nameMap(gen.spor.structs)
local datStructs  = nameMap(gen.dat.structs)
local compStructs = nameMap(gen.comp.structs)

local function assertConst(name, ty, value, consts)
  assertEq(
   gen.FConst{name=name, ty=ty, value=value},
   consts[name])
end

local function assertStruct(structs, name, repr)
  assertEq(repr, tostring(structs[name]))
end

test('spor', nil, function()
  assertConst('INC',  'U1', '0x10', sporConsts)
  assertConst('NOT',  'U1', '0x16', sporConsts)
  assertConst('SLIC', 'U1', '0x86', sporConsts)
end)

test('dat', nil, function()
  assertConst('INFO',  'U1', '0x04',   datConsts)
  assertConst('SIZE',  'U2', '0x1000', datConsts)
  assertStruct(gen.datStructs, 'Slc',
    'FStruct{name=Slc fields=[{name=dat refs=1 ty=U1}'
    .. ' {name=len refs=0 ty=U2}]}')
  assert(datStructs.Block)
  assert(datStructs.Buf)
  assert(not datStructs.TyDb)
  assert(not datStructs.InOut)
  assert(not datStructs.Globals)
end)

test('comp', nil, function()
  assertConst('FN_STATE_NO', 'U4', '0x0000', compConsts)
  assertConst('TY_FN', 'U1', '0x80', compConsts)
  assertConst('TY_FN_INLINE', 'U1', '0x04', compConsts)
  assert(not compStructs.Block)
  assert(not compStructs.Buf)
  assert(compStructs.TyDb)
  assert(compStructs.InOut)
  assert(compStructs.Globals)
end)


