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
s = set.new(a)
assert(s.a); assert(not s.d)
assertEq({a=true, b=true, c=true}, s)

t = {a=1, c=5}
assertEq({a=1, c=5}, t)
map.update(t, {a=2, b=3})
assertEq({a=2, b=3, c=5}, t)

assertEq({[1]='a', [2]='b', [3]='c'}, a)
arr.extend(a, {'d', 'e'})
assertEq({'a', 'b', 'c', 'd', 'e'}, a)

A = civ.struct('A', {'a1', 'a2'})
a = A.new({a1=3})
assertEq(3, a.a1)
assert(A == getmetatable(a))
assertEq(nil, a.a2)
a.a2 = 4; assertEq(4, a.a2)

B = civ.struct('B', {'b1', 'b2'})
b = B.new({b1=5})
assertEq(5, b.b1)
assert(A == getmetatable(a))
assertEq(nil, b.b2)
assertEq('A{a1=3 a2=4}', fmt(a))
