
local CHECK = {field = false}

-- ###################
-- # Utility Functions

-- return keys array
local function keysarr(t)
  local a = {}; for k in pairs(t) do table.insert(a, k) end
  return a
end

-- convert array to an iterator
-- similar to ipairs returns only the values.
local function iterarr(l)
  local i, n = 0, #l
  return function()
    i = i + 1; if i <= n then  return l[i]  end
  end
end

local join = table.concat
local function sort(t) table.sort(t) return t end
local function orderedKeys(t) return iterarr(sort(keysarr(t))) end

-- used primarily for formatting

-- shallow copy and update with add
local function copy(t, add)
  if add then
    local out = {table.unpack(t)}
    for k, v in pairs(add) do out[k] = v end
    return out
  end
  return {table.unpack(t)}
end

-- update is for dict/set, extend for list
local function update (t, add) for k, v in pairs(add) do t[k] = v end end
local function extend (a, vals)
  for _, v in ipairs(vals) do table.insert(a, v) end
end

local function fmtString(s)
  if not string.find(s, '%s') then return s
  else return string.format("%q", s) end
end

-- __index function for only getting methods
local function _methIndex(self, k)
  return getmetatable(self)[k]
end

-- __index function used for most types
local function _tyIndex(self, k)
  local ty = getmetatable(self)
  local v = ty["#defaults"][k] or ty[k]
  if v then  return v  end
  if "string" ~= type(k) then k = "<not str>" end
  error("Unknown member: " .. ty.__name .. "." .. k)
end

local Set = nil

-- A Ty has methods (which holds documentation)
-- and field metadata. It also has a metatable
-- so we can override __call (constructor)
local function newTy(name)
  local ty = {
    __name=name,
    __index=_tyIndex,
    ["#civTy"]=true,
    ["#methods"]={},
    ["#defaults"]={},
  }
  return setmetatable(ty, {
    __name=name,
    __tostring=function(_) return name end,
  })
end

local Nil  = newTy('Nil');  local Fn   = newTy('Fn')
local Bool = newTy('Bool'); local Num  = newTy('Num')
local Str  = newTy('Str');  local Tbl  = newTy('Tbl')

local _tyMap = {
  ['nil']      = function(_) return Nil  end,
  ['function'] = function(_) return Fn   end,
  boolean      = function(_) return Bool end,
  number       = function(_) return Num  end,
  string       = function(_) return Str  end,
  table        = function(t)
    local ty = getmetatable(t)
    if not ty or not ty['#civTy'] then  return Tbl
    else                                return ty  end
  end,
}
local function ty(obj) return _tyMap[type(obj)](obj) end
assert(Str == ty('hi')); assert(Num == ty(3))
assert(Tbl == ty({}))

local function tyName(ty) return ty.__name end
assert('Str' == tyName(Str)); assert(nil   == tyName({}))

-- check if given is a subtype (child of) req
local function tyCheck(req, given)
  if (true == req) or (req == given) then return true end
  local a = given["#ancestors"]
  return a and a[req]
end


-- Define a types method
local function constructor(ty, fn)
  getmetatable(ty).__call = fn
end
local function method(ty, name, fn) ty[name] = fn  end

-- Iter: iterator subtype, traverses indexed table.
local Iter = newTy('Iter')
constructor(Iter, function(ty, l)
  return setmetatable({data=l, i=0}, ty)
end)
method(Iter, '__call', function(self, l)
  if self.i < #self.data then
    self.i = self.i + 1
    return self.data[self.i]
  end
end)

local result = Iter{3, 4}
assert(3 == result()); assert(4 == result())
assert(nil == result());

-- A Map: literally a lua table with some methods.
-- Use like:
--
--   t = Mbl{'a'=2, 'b'=7, 1='d', 2='f'}
--   s[3] = 'e'
local Map = newTy('Map')
getmetatable(Map).__call = function(ty, t)
  return setmetatable(t, ty)
end
local result = Map{a=2, b=3}
assert(2 == result.a); assert(3 == result.b)
-- assert(nil == result.c) -- TODO raises error "unknown member"
assert(Map == ty(result))

-- A set: unique keys in arbitrary order. Values are all `true`
-- Use like:
--
--   s = Set{'a', 'b', 1, 2}
--   s:add(3)
Set = newTy('Set')
getmetatable(Set).__call = function(ty, t)
  local s = {}
  for _, key in pairs(t) do s[key] = true end
  return setmetatable(s, ty)
end
method(Set, '__index',    _methIndex)
method(Set, 'add', function(self, k)  self[k] = true  end)
method(Set, 'union', function(self, r)
  local both = Set{}
  for k in pairs(self) do if r[k] then both[k] = true end end
  return both
end)
method(Set, 'leftOnly', function(self, r)
  local left = Set{}
  for k in pairs(self) do if not r[k] then left[k] = true end end
  return left
end)

-- List: ordered set of possibly duplicated values
-- A list is just the normal Lua list with type information and a few methods.
-- Use iparis to iterate over it, etc
--
--   l = List{'a', 'b', 1, 2}
--   l:add(3)
--   l:extend{1, 2, 3}
--   l[1] get first (aka very first) index.
local List = newTy("List")
getmetatable(List).__call = function(ty, t) return setmetatable(t, ty) end
method(List, '__index', _methIndex)
method(List, 'iter', ipairs)
method(List, 'add', table.insert)
method(List, 'extend', extend)

result = List{5, 6}; assert(5 == result[1])
result:extend{3, 4}; assert(4 == result[4])

-- ###################
-- # Formatting
-- lua cannot format raw tables. We fix that, and also build up
-- for formatting structs/etc
local fmtBuf = nil
local function fmtTableRaw(b, t, keys)
  table.insert(b, '{')
  local endAdd = 1
  for key in keys do
    fmtBuf(b, key);    table.insert(b, '=')
    fmtBuf(b, t[key]); table.insert(b, ' ')
    endAdd = 0 -- remove space for last
  end
  b[#b + endAdd] = '}'
end

local _bufFmtTy = {
  number = table.insert, boolean = table.insert,
  ['nil'] = function(b, obj) table.insert(b, 'nil') end,
  string = function(b, s)
    if not string.find(s, '%s') then table.insert(b, s)
    else
      table.insert(b, '"')
      table.insert(b, s)
      table.insert(b, '"')
    end
  end,
  ['function'] = function(b, f)
    s = debug.getinfo(f, 'S')
    b[#b + 1] = 'function['
    b[#b + 1] = s.short_src;   b[#b + 1] = ':'
    b[#b + 1] = s.linedefined; b[#b + 1] = ']'
  end,
  table = function(b, t)
    local mt = getmetatable(t)
    if mt and mt.__tostring then return table.insert(b, tostring(t)) end
    fmtTableRaw(b, t, orderedKeys(t))
  end,
}

-- Map.__tostring
method(Map, '__tostring', function(t)
  local b = {}; fmtTableRaw(b, t, orderedKeys(t))
  return join(b)
end)
fmtBuf = function (b, obj) _bufFmtTy[type(obj)](b, obj) end

-- Set.__tostring
method(Set, '__tostring', function(self)
  local b, endAdd = {'{'}, 1
  for k in orderedKeys(self) do
    fmtBuf(b, k); table.insert(b, ' '); endAdd = 0
  end
  b[#b + endAdd] = '}'; return join(b)
end)
result = Set{'a', 'b', 'c'}; assert("{a b c}"   == tostring(result))
result:add('d');             assert("{a b c d}" == tostring(result))

-- List.__tostring
method(List, '__tostring', function(self)
  local b, endAdd = {'['}, 1
  for _, v in ipairs(self) do
    fmtBuf(b, v); table.insert(b, ' ')
    endAdd = 0 -- remove last space
  end
  b[#b + endAdd] = ']'; return join(b)
end)
assert("[5 6 3 4]"  == tostring(List{5, 6, 3, 4}))
assert("{a=5 b=77}" == tostring(Map{a=5, b=77}))

-- fmt any object
local function fmt(obj)
  local b = {}; fmtBuf(b, obj); return join(b, "")
end

local function tyError(req, given)
  error(string.format("%s is not an ancestor of %s", given, req))
end

-- ###################
-- # Generic Equality

local function nativeEq(a, b) return a == b end

-- Note: requires that the types are identical
-- modified from: https://www.lua.org/manual/5.1/manual.html
local function getcomphandler (op1, op2, event)
  return (getmetatable(op1) or {})[event] or (getmetatable(op2) or {})[event]
end

local eq = nil
local function eqNoHandler(a, b)
  -- TODO: compare addresses? I can't figure it out
  if ty(a) ~= ty(b)     then return false  end
  local aLen = 0
  for aKey, aValue in pairs(a) do
    local bValue = b[aKey]
    if not eq(aValue, bValue) then return false end
    aLen = aLen + 1
  end
  local bLen = 0
  -- Note: #b only returns length of integer indexes
  for bKey in pairs(b) do bLen = bLen + 1 end
  return aLen == bLen
end

local eqTy = {
  number = nativeEq, boolean = nativeEq, string = nativeEq,
  ['nil'] = nativeEq, ['function'] = nativeEq,
  ['table'] = function(a, b)
    if getcomphandler(a, b, '__eq') then return a == b end
    if a == b             then return true   end
    return eqNoHandler(a, b)
  end,
}
eq = function(a, b) return eqTy[type(a)](a, b) end

assert(eq(2, 2))
assert(eq({4, 5}, {4, 5}))
assert(eq({4, {5, 6}}, {4, {5, 6}}))
assert(not eq({4, {5, 6, 7}}, {4, {5, 6}}))
assert(not eq({4, {5, 6}}, {4, {5, 6, 7}}))

-- Now update the eq for our types
method(Map, '__eq', eqNoHandler)
method(Set, '__eq', eqNoHandler)
assert(Map{4, 5} == Map{4, 5})

local function eqArr(a, b)
  if(#a ~= #b) then return false end
  for i, av in ipairs(a) do
    if not eq(av, b[i]) then return false end
  end
  return true
end
method(List, '__eq', eqArr)

local function assertEq(left, right)
  if eq(left, right) then return end
  err = {}
  fmtBuf(err, "Values not equal:")
  fmtBuf(err, "\n   left: "); fmtBuf(err, left)
  fmtBuf(err, "\n  right: "); fmtBuf(err, right)
  error(join(err))
end

local result = List{1, 'a', 2}
assertEq(List{1, 'a', 2},   result)
assert  (List{1, 'a', 2} == result)
assert  (List{2, 'a', 2} ~= result)

-- ###################
-- # Struct

local function structInvalidField (st, f)
  error(string.format("%s does not have field %s", st, f))
end
local function structTy (t) return getmetatable(t).__name end
local function structIndex (t, k)
  local m = (
    getmetatable(t)["#defaults"][k]
    or getmetatable(t)[k]);
  if nil ~= m then return m end
  structInvalidField(getmetatable(t), k)
end

local function structNewIndex (t, k, v)
  if CHECK.field and not getmetatable(t)["#fields"][k] then
    structInvalidField(getmetatable(t), k)
  end
  rawset(t, k, v)
end

local function structFmt(t)
  local b = {tyName(getmetatable(t))}
  fmtTableRaw(b, t, Iter(getmetatable(t)['#fieldOrder']))
  return join(b, '')
end

local function specifyFields(fields)
  local tys, defaults, ordered = {}, {}, {}
  for _, f in ipairs(fields) do
    local fname, fty, fdef = nil, nil, false
    if 'string' == type(f) then fname = f
    else fname, fty, fdef = table.unpack(f) end
    assert(type(fname) == 'string');
    tys[fname] = fty or true; defaults[fname] = fdef;
    List.add(ordered, fname)
  end
  return tys, defaults, ordered
end
local function struct(name, fields)
  local st = newTy(name)
  local tys, defaults, ordered = specifyFields(fields)
  update(st, {
    ["#fields"]=tys,  ["#fieldOrder"]=ordered,
    ["#defaults"]=defaults,
    __index=structIndex, __newindex=structNewIndex,
    __tostring=structFmt,
  })
  getmetatable(st).__call = function(st, t)
    for f, v in pairs(t) do
      local fTy = st["#fields"][f]
      if not fTy then structInvalidField(st, f) end
      if not tyCheck(fTy, ty(v)) then tyError(fTy, ty(v)) end
    end
    for f in pairs(st["#fields"]) do
      if nil == t[f] and nil == st["#defaults"][f] then
        error("missing field: " .. f)
      end
    end
    return setmetatable(t, st)
  end
  return st
end

-- ###################
-- # Schema
-- A schema type is a set of named columns with indexed data.
-- The data can be a callable object that accepts a Query
-- object.

local Query = struct('Query', {
  'indexes',
  'filter',  'pattern',
  'eq', 'lt', 'gt', 'lte', 'gte'
})

local Schema = newTy('Schema')
local _schConstructor = {
  [List] = function(l)
    local cols, it = Map{}, Iter(l)
    -- first row is list of column names
    for _, k in ipairs(it()) do cols[k] = List{} end
    -- other rows are Maps/structs with data
    for row in it do
      for k, v in pairs(row) do
        l = cols[k]; if not l then
          error(string.format("unknown column %s", k))
        end
        l:add(v)
      end
    end
    return cols
  end,
}

constructor(Schema, function(ty, data)
  local sch = {cols = _schConstructor(data)}
  return setmetatable(sch, ty)
end)

-- ###################
-- # Test Harness

-- assert that globals haven't changed
local function globals()
  local out = Set{}
  for k in pairs(_G) do out[k] = true end
  return out
end
local function assertGlobals(prev, expect)
  expect = expect or Set{}
  local new = globals():leftOnly(prev)
  if not eq(expect, new) then
    error(string.format("New globals: %s", new))
  end
end
assertGlobals(globals())

local function test(name, options, fn)
  local g, options = globals(), options or {}
  print("## Test", name)
  fn()
  assertGlobals(g, options.expectGlobals)
end

return {
  Nil = Nil, Bool = Bool, Str  = Str,  Num = Num,
  Fn  = Fn,  Tbl  = Tbl,
  Map = Map, List = List, Set = Set, Iter = Iter,

  -- Generic table operations
  eq = eq, update = update, extend = extend,

  -- Formatters
  fmt = fmt,
  fmtBuf = fmtBuf,

  -- struct
  struct = struct,

  -- tests
  test = test,
  assertEq = assertEq,
  globals = globals, assertGlobals = assertGlobals,
}
