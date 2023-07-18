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
  assertEq("{1=1 2=2 3=3}", tostring(Fmt{1, 2, 3}))
  assertEq('\n+ 1=1\n+ 2=2\n+ 3=3\n',
           tostring(Fmt.pretty{1, 2, 3}))
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

test('set', nil, function()
  local s = Set{'a', 'b', 'c'}
  assertEq(Set{'a', 'c', 'b'}, s)

  local l = sort(List.fromIter(s:iter()))
  assertEq(List{'a', 'b', 'c'}, l)
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

local function structs()
  local A = civ.struct('A', {{'a2', civ.Num}, {'a1', Str}})
  local B = civ.struct('B', {
    {'b1', civ.Num}, {'b2', civ.Num, 32},
    {'a', A, false}
  })
  return A, B
end
test("struct", nil, function()
  local A, B = structs()
  local a = A{a1='hi', a2=5}
  assert(A == getmetatable(a))
  assertEq('hi', a.a1); assertEq(5, a.a2)
  a.a2 = 4;             assertEq(4, a.a2)
  assertEq('A{a2=4 a1=hi}', civ.fmt(a))

  local b = B{b1=5, a=a}
  assert(B == getmetatable(b))
  assertEq(5, b.b1); assertEq(32, b.b2)
  b.b2 = 7;          assertEq(7, b.b2)
  assertEq(Num, pathTy(B, {'b1'}))
  assertEq(Str, pathTy(B, {'a', 'a1'}))
  assertEq('hi', pathVal(b, {'a', 'a1'}))

  assertEq({'a32', 'b', 'c1_32'}, dotSplit('a32.b.c1_32'))
end)

test("iter", nil, function()
  local i = List{1, 4, 6}:iterFn()
  assert(1 == select(2, i()))
  assert(4 == select(2, i()))
  assert(6 == select(2, i()))

  local l = List.fromIter(List{1, 4, 6}:iter())
  assertEq(List{1, 4, 6}, l)

  local r = Range(1, 5); assertEq('[1:5]', tostring(r))
  l = List.fromIter(Range(1, 5))
  assertEq(List{1, 2, 3, 4, 5}, l)
end)

local expectDisplay = trim[[
===========+==================+=======================+========================
date       | title            | text                  | todo
===========+==================+=======================+========================
2023-04-03 | "Good day today" | "This was a good day. | false
           |                  | The sun was shining." | 
 -  -  -   +  -  -  -  -  -   +  -  -  -  -  -  -  -  +  -  -  -  -  -  -  -   
" 04-05 "  | "Bad day"        | "Terrible day         | + 1="I don't know"
           |                  | Just terrible okay?." | + 2="have a better day"
 -  -  -   +  -  -  -  -  -   +  -  -  -  -  -  -  -  +  -  -  -  -  -  -  -
]]
test('display', nil, function()
  -- test trim
  assertEq('foo bar', trim('\n  \nfoo bar \n'))

  -- test lines function
  local l = List{}
  for line in lines("hi there\nbob\n") do
    l:add(line)
  end
  assertEq(List{"hi there", "bob", ""}, l)

  -- fillBuf
  local b = List{}; fillBuf(b, 5)
  assertEq(List{'   ', ' ', ' '}, b)

  local J = struct('J', {
    'date', 'title', 'text', 'todo'
  })
  local j = List{
    J{date='2023-04-03', title='Good day today',
      text='This was a good day.\nThe sun was shining.'},
    J{date=' 04-05 ', title='Bad day',
      text='Terrible day\nJust terrible okay?.',
      todo=List{"I don't know", 'have a better day'},
    },
  }
  local disp = Display(J, j:iterFn())
  local result = trim(tostring(disp))
  assertEq(expectDisplay, result)
end)

test('picker', nil, function()
  local A, B = structs()
  local lA = List{
    A{a1='one',   a2=1},
    A{a1='two',   a2=2},
    A{a1='three', a2=3},
  }
  local pA = Picker(A, lA)
  local result = pA.q.a1:eq('one')
  result = result:toList()
  assertEq(List{
    A{a1='one',   a2=1},
  }, result)

  assertEq(List{
    A{a1='one',   a2=1},
  }, pA.q.a1:eq('one'):toList())

  result = pA.q.a2:in_{2, 3}:toList()
  assertEq(List{
    A{a1='two',   a2=2},
    A{a1='three', a2=3},
  }, result)
  assertEq(List{
    A{a1='two',   a2=2},
    A{a1='three', a2=3},
  }, pA.q.a2:in_{2, 3}:toList())

  local G1 = genStruct('G1', {'a', Num, 'b', Str})
  assertEq('G1{a:Num b:Str}', tostring(G1))
  assert(rawequal(G1, genStruct('G1', {'a', Num, 'b', Str})))

  local g1 = G1{a=8, b='hel'}
  assert('G1{a=8 b=hel}', tostring(g1))

  local G2 = genStruct('G2', {'a', true, 'b', Str})
  assert(not rawequal(G1, G2))
  assertEq('G2{a:Any b:Str}', tostring(G2))

  result = pA.q.a2:in_{2, 3}:select{'a1'}
  local b = {}; fmtTableRaw(b, result, orderedKeys(result))
  assertEq('[Q{a1=two} Q{a1=three}]', tostring(result:toList()))

  local lB = List{
    B{b1=3,  b2=1},
    B{b1=5,  b2=2},
    B{b1=7,  b2=3},
  }
  local pB = Picker(B, lB)
  local result = pA.q:joinEq('a2', pB, 'b1')
  assertEq(
    '[joinEq{j1=A{a2=3 a1=three} j2=B{b1=3 b2=1 a=false}}]',
    tostring(result.data))
  local sel = result.q:select{'j1.a1', 'j2.b2'}:toList()
  assertEq('[Q{a1=three b2=1}]', tostring(sel))

  local result = Display(ty(sel[1]), sel:iterFn())
  local expected = trim[[
======+===
a1    | b2
======+===
three | 1
 -    +
]]
  assertEq(expected, trim(tostring(result)))

end)

assertGlobals(g)
