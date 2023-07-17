local g = {}; for k in pairs(_G) do g[k] = true end

-- assert Set first, since we are going to use it
local civ = require('civ')
local s = civ.Set{'a', 'b', 'c'}
assert(s.a)
civ.assertEq(civ.Set{'a', 'b', 'c'}, s)
local s2 = civ.Set{'b', 'c', 'd'}
civ.assertEq(s:union(s2), civ.Set{'b', 'c'})

civ.assertGlobals(g)
require('gciv'); g = globals()

test("fmt", nil, function()
  assertEq("{1=1 2=2}", fmt({1, 2}))
  assertEq([[{baz=boo foo=bar}]], fmt({foo="bar", baz="boo"}))
  assertEq({foo=bar, baz=2}, {foo=bar, baz=2})
end)

test("eq", nil, function()
  local a, b = {v=42}, {v=42}
  assert(eq(a, b)); assert(a ~= b) -- not the same instance
  -- add the metamethod __eq
  local vTable = {__eq = function(a, b) return a.v == b.v end, }
  setmetatable(a, vTable)
  assert(a == b); assert(b == a) -- uses a's metatable regardless
  assert(eq(a, b))
  a.other = 7;   assertEq(a, b) -- still uses metatable

  setmetatable(b, vTable)
  assert(a == b); assert(b == a)
end)

test("update-extend", nil, function()
  local a = {'a', 'b', 'c'}
  local t = {a=1, c=5}
  assertEq({a=1, c=5}, t)
  civ.update(t, {a=2, b=3})
  assertEq({a=2, b=3, c=5}, t)

  assertEq({[1]='a', [2]='b', [3]='c'}, a)
  civ.extend(a, {'d', 'e'})
  assertEq({'a', 'b', 'c', 'd', 'e'}, a)
end)

test("struct", nil, function()
  local A = civ.struct('A', {'a2', 'a1'})
  local a = A{a1=3, a2=5}
  assert(A == getmetatable(a))
  assertEq(3, a.a1); assertEq(5, a.a2)
  a.a2 = 4;          assertEq(4, a.a2)
  assertEq('A{a2=4 a1=3}', civ.fmt(a))

  local B = civ.struct('B', {{'b1', civ.Num}, {'b2', civ.Num, 32}})
  local b = B{b1=5}
  assert(B == getmetatable(b))
  assertEq(5, b.b1); assertEq(32, b.b2)
  b.b2 = 7;          assertEq(7, b.b2)
end)

test("iter", nil, function()

end)

assertGlobals(g)
