require('gciv')
local gen = dofile('./etc/gen.lua')

local CONST_EXAMPLE = [[
const FOO: U1 = 42
const BAR: U1 = 33
]]

local expected = Map{
  FOO=gen.FConst{name="FOO", ty="U1", value="42"},
  BAR=gen.FConst{name="BAR", ty="U1", value="33"}, }
local result = gen.parseConsts(CONST_EXAMPLE)
assertEq(expected, result)

local STRUCT_EXAMPLE = [=[
struct Foo [ a: U2;  b: &S ]
]=]
local _, _, stName, stBody = string.find(
  STRUCT_EXAMPLE, gen.STRUCT_PAT)
assertEq("Foo", stName)
assertEq("[ a: U2;  b: &S ]", stBody)

local _, _, fName, fRefs, fTy = string.find(stBody, gen.FIELD_PAT)
assertEq("a", fName);
assertEq("", fRefs);
assertEq("U2", fTy)

local expected = Map{Foo=gen.FStruct{
  name='Foo',
  fields=List{
    gen.FField{name='a', refs=0, ty='U2'},
    gen.FField{name='b', refs=1, ty='S'},
  },
}}
local result = gen.parseStructs(STRUCT_EXAMPLE)
assertEq(expected, result)

-- for k, v in pairs(gen.spor.consts) do
--   print(k, v)
-- end

