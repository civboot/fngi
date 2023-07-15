local fmtBuf = nil

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

-- used primarily for formatting
local function orderedKeys(t)
  l = keysarr(t); table.sort(l)
  return iterarr(l)
end

-- shallow copy and update with add
local function copy(t, add)
  if add then
    local out = {table.unpack(t)}
    for k, v in pairs(add) do out[k] = v end
    return out
  end
  return {table.unpack(t)}
end

-- update t with the key/values in add
local function update (t, add)
  for k, v in pairs(add) do t[k] = v end
end

local function withmetatable(t, mt)
  setmetatable(t, mt); return t
end

local function fmtString(s)
  if not string.find(s, '%s') then return s
  else return string.format("%q", s) end
end

local function fmtKeys(s)
  local out = {}
  for key, value in pairs(s) do
    table.insert(out, fmtString(key)) end
  table.sort(out)
  return table.concat(out, ' ')
end

local result = fmtKeys({a=true, b=true, c=true})
assert("a b c" == result)

-- __index function used for types
local function _tyIndex(self, k)
  local ty = getmetatable(self)
  -- TODO: handle defaults
  return ty[k]
end

-- A Ty has methods (which holds documentation)
-- and field metadata. It also has a metatable
-- so we can override __call (constructor)
local function newTy(name, fields)
  local ty = {
    __name=name,
    __index=_tyIndex,
    ["#civTy"]=true,
    ["#methods"]={},
  }
  if fields then
    update(ty, {
      ["#fields"]={},
      ["#fieldOrder"]={},
      ["#fieldTy"]={},
    })
  end
  return setmetatable(ty, {__name='Meta_' .. name })
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

-- Define a types method
local function method(ty, name, body) ty[name] = body  end

-- A table: literally a lua table with some methods.
-- Use like:
--
--   t = Tbl{'a'=2, 'b'=7, 1='d', 2='f'}
--   s[3] = 'e'
getmetatable(Tbl).__call = function(ty, t)
  return setmetatable(t, ty)
end
local result = Tbl{a=2, b=3}
assert(2 == result.a); assert(3 == result.b)
assert(nil == result.c)
assert(Tbl == ty(result))

-- A set: unique keys in arbitrary order. Values are all `true`
-- Use like:
--
--   s = Set{'a', 'b', 1, 2}
--   s:add(3)
local Set = newTy('Set')
getmetatable(Set).__call = function(ty, t)
  local s = {}
  for _, key in pairs(t) do s[key] = true end
  return setmetatable(s, ty)
end
method(Set, '__tostring', fmtKeys)
method(Set, 'add', function(self, k)  self[k] = true  end)

result = Set{'a', 'b', 'c'}; assert("a b c"   == tostring(result))
result:add('d');             assert("a b c d" == tostring(result))

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
method(List, 'iter', ipairs)
method(List, 'add', table.insert)
method(List, 'extend', function (a, extend)
  for _, value in ipairs(extend) do table.insert(a, value) end
end)

result = List{5, 6}; assert(5 == result[1])
result:extend{3, 4}; assert(4 == result[4])

-- ###################
-- # Formatting
-- lua cannot format raw tables. We fix that, and also build up
-- for formatting structs/etc
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
    print('table', t, ' mt', mt)
    if mt and mt.__tostring then return table.insert(b, tostring(t)) end
    fmtTableRaw(b, t, orderedKeys(t))
  end,
}

method(Tbl, '__tostring', function(t)
  local b = {}
  fmtTableRaw(b, t, orderedKeys(t))
  return table.concat(b)
end)
fmtBuf = function (b, obj) _bufFmtTy[type(obj)](b, obj) end

local function fmt(obj)
  local b = {}; fmtBuf(b, obj); return table.concat(b, "")
end

method(List, '__tostring', function(self)
  local b, endAdd = {'['}, 1
  for _, v in ipairs(self) do
    fmtBuf(b, v); table.insert(b, ' ')
    endAdd = 0 -- remove last space
  end
  b[#b + endAdd] = ']'; return table.concat(b)
end)
assert("[5 6 3 4]"  == tostring(result))
assert("{a=5 b=77}" == tostring(Tbl{a=5, b=77})

local function nativeEq(a, b) return a == b end

-- Note: requires that the types are identical
-- modified from: https://www.lua.org/manual/5.1/manual.html
local function getcomphandler (op1, op2, event)
  return (getmetatable(op1) or {})[event] or (getmetatable(op2) or {})[event]
end

local eq = nil
local eqTy = {
  number = nativeEq, boolean = nativeEq, string = nativeEq,
  ['nil'] = nativeEq, ['function'] = nativeEq,
  ['table'] = function(a, b)
    if ty(a) ~= ty(b)     then return false  end
    if a == b             then return true   end
    if getcomphandler(a, b, '__eq') then return a == b end
    local aLen = 0
    for aKey, aValue in pairs(a) do
      bValue = b[aKey]
      if not eq(aValue, bValue) then return false end
      aLen = aLen + 1
    end
    local bLen = 0
    -- Note: #b only returns length of integer indexes
    for bKey in pairs(b) do bLen = bLen + 1 end
    return aLen == bLen
  end,
}
eq = function(a, b) return eqTy[type(a)](a, b) end

assert(eq(2, 2))
assert(eq({4, 5}, {4, 5}))
assert(eq({4, {5, 6}}, {4, {5, 6}}))
assert(not eq({4, {5, 6, 7}}, {4, {5, 6}}))
assert(not eq({4, {5, 6}}, {4, {5, 6, 7}}))

-- Now update the eq for our types
method(Tbl, '__eq', eq)
method(Set, '__eq', eq)

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
  error(table.concat(err))
end

local result = List{1, 'a', 2}
assertEq(List{1, 'a', 2},   result)
assert  (List{1, 'a', 2} == result)
assert  (List{2, 'a', 2} ~= result)

-- local function structInvalidField (st, f)
--   error(st.ty .. " does not have field " .. f)
-- end
-- local function structTy (t) return getmetatable(t).ty end
-- local function structIndex (t, k)
--   if getmetatable(t)._fields[k] then  return nil  end
--   structInvalidField(getmetatable(t), k)
-- end
--
-- 
-- local function structNewIndex (t, k, v)
--   if not getmetatable(t)._fields[k] then structInvalidField(getmetatable(t), k) end
--   rawset(t, k, v)
-- end
-- local function structFmt(t)
--   local b = {structTy(t)}; fmtTableRaw(b, t, getmetatable(t)._orderedFields)
--   return table.concat(b, '')
-- end
-- 
-- -- TODO: add sortedFields and improve print
-- -- TODO: add methods
-- local function struct(ty, fields)
--   fields = set.new(fields)
--   local st = {ty = ty}
--   map.update(st, {
--     _fields=fields, _orderedFields=orderedKeys(fields),
--     __index=structIndex, __newindex=structNewIndex,
--     __tostring=structFmt,
--   })
--   st.new = function(data)
--     for f in pairs(data) do
--       if not fields[f] then structInvalidField(st, f) end
--     end
--     return setmetatable(data, st)
--   end
--   return st
-- end
-- 
-- 
-- return {
--   -- Generic table operations
--   eq = eq,
-- 
--   -- Formatters
--   fmt = fmt,
--   fmtBuf = fmtBuf,
--   assertEq = assertEq,
-- 
--   -- struct
--   struct = struct,
-- }

return {}

