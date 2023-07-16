civ = require('civ')

local fmt, eq  = civ.fmt, civ.eq
local assertEq = civ.assertEq

assertEq("{1=1 2=2}", fmt({1, 2}))
assertEq([[{baz=boo foo=bar}]], fmt({foo="bar", baz="boo"}))
assertEq({foo=bar, baz=2}, {foo=bar, baz=2})

a = {v=42}; b = {v=42}
assert(eq(a, b)); assert(a ~= b) -- not the same instance
-- add the metamethod __eq
vTable = {__eq = function(a, b) return a.v == b.v end, }
setmetatable(a, vTable)
assert(a == b); assert(b == a) -- uses a's metatable regardless
assert(eq(a, b))
a.other = 7;   assertEq(a, b) -- still uses metatable

setmetatable(b, vTable)
assert(a == b); assert(b == a)

a = {'a', 'b', 'c'}
s = civ.Set(a)
assert(s.a)
civ.assertEq(civ.Set{'a', 'b', 'c'}, s)

t = {a=1, c=5}
assertEq({a=1, c=5}, t)
civ.update(t, {a=2, b=3})
assertEq({a=2, b=3, c=5}, t)

assertEq({[1]='a', [2]='b', [3]='c'}, a)
civ.extend(a, {'d', 'e'})
assertEq({'a', 'b', 'c', 'd', 'e'}, a)

A = civ.struct('A', {'a1', 'a2'})
a = A{a1=3, a2=5}
assert(A == getmetatable(a))
assertEq(3, a.a1); assertEq(5, a.a2)
a.a2 = 4;          assertEq(4, a.a2)
assertEq('A{a1=3 a2=4}', civ.fmt(a))

B = civ.struct('B', {{'b1', civ.Num}, {'b2', civ.Num, 32}})
b = B{b1=5}
assert(B == getmetatable(b))
assertEq(5, b.b1); assertEq(32, b.b2)
b.b2 = 7;          assertEq(7, b.b2)
