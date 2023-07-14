local civ = require('civ')
local gen = dofile('./etc/gen.lua')

local CONST_EXAMPLE = [[
const FOO: U1 = 42
const BAR: U1 = 33
]]

local expected = {
  FOO={name="FOO", ty="U1", value="42"},
  BAR={name="BAR", ty="U1", value="33"}, }
civ.assertEq(expected, gen.parseConsts(CONST_EXAMPLE))

local STRUCT_EXAMPLE = [=[
struct Foo [ a: U2;  b: Arr[4 U2] ]
]=]
local _, _, stName, stBody = string.find(STRUCT_EXAMPLE, gen.STRUCT_PAT)
civ.assertEq("Foo", stName)
civ.assertEq("[ a: U2;  b: Arr[4 U2] ]", stBody)

local _, _, fName, fRefs, fTy = string.find(stBody, gen.FIELD_PAT)
civ.assertEq("a", fName); civ.assertEq("", fRefs); civ.assertEq("U2", fTy)

