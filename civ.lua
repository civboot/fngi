local fmtBuf = nil

-- Use like:
--   for i, key in orderedKeys(myTable)
local function orderedKeys(t)
  local a = {}; for n in pairs(t) do table.insert(a, n) end
  table.sort(a)
  local i, n = 0, #a
  return function()
    i = i + 1
    if i <= n then  return a[i]  end
  end
end

set = {}

-- set({'a', 'b', 'c'}) makes those values the keys (all values are true)
set.new = function(t)
  local s = {}; for _, key in pairs(t) do s[key] = true end
  return s
end

map = {}

-- update t with the key/values in add
map.update = function(t, add)
  for key, value in pairs(add) do t[key] = value end
end

arr = { append = table.insert }

-- extend array with values in add
arr.extend = function (a, add)
  for i, value in ipairs(add) do table.insert(a, value) end
end

local function fmtTableRaw(b, t, keys)
  table.insert(b, '{')
  local endAdd = 1
  for key in orderedKeys(t) do
    fmtBuf(b, key);    table.insert(b, '=')
    fmtBuf(b, t[key]); table.insert(b, ' ')
    endAdd = 0 -- remove space for last
  end
  b[#b + endAdd] = '}'
end

local bufFmtTy = {
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
    if t.__tostring then return table.insert(b, tostring(t)) end
    fmtTableRaw(b, t)
  end,
}
fmtBuf = function (b, obj) bufFmtTy[type(obj)](b, obj) end

local function fmt(obj)
  local b = {}; fmtBuf(b, obj); return table.concat(b, "")
end

local function nativeEq(a, b) return a == b end

-- Note: requires that the types are identical
-- modified from: https://www.lua.org/manual/5.1/manual.html
local function hascomphandler (op1, op2, event)
  return (getmetatable(op1) or {})[event] or (getmetatable(op2) or {})[event]
end

local eq = nil
local eqTy = {
  number = nativeEq, boolean = nativeEq, string = nativeEq,
  ['nil'] = nativeEq, ['function'] = nativeEq,
  ['table'] = function(a, b)
    if type(b) ~= 'table' then return false  end
    if a == b             then return true   end
    if hascomphandler(a, b, '__eq') then return a == b end
    local aLen = 0
    for aKey, aValue in pairs(a) do
      bValue =b[aKey]
      if not eq(aValue, bValue) then return false end
      aLen = aLen + 1
    end
    local bLen = 0
    -- Note: #b only returns length of integer indexes
    for bKey in pairs(b) do bLen = bLen + 1 end
    return aLen == bLen
  end,
}

eq = function(a, b)
  return eqTy[type(a)](a, b)
end

assert(eq(2, 2))
assert(eq({4, 5}, {4, 5}))
assert(eq({4, {5, 6}}, {4, {5, 6}}))
assert(not eq({4, {5, 6, 7}}, {4, {5, 6}}))
assert(not eq({4, {5, 6}}, {4, {5, 6, 7}}))

local function assertEq(left, right)
  if eq(left, right) then return end
  err = {}
  fmtBuf(err, "Values not equal:")
  fmtBuf(err, "\n   left: "); fmtBuf(err, left)
  fmtBuf(err, "\n  right: "); fmtBuf(err, right)
  error(table.concat(err))
end

local function structInvalidField (st, f)
  error(st.ty .. " does not have field " .. f)
end
local function structTy (t) return getmetatable(t).ty end
local function structIndex (t, k)
  if getmetatable(t)._fields[k] then  return nil  end
  structInvalidField(getmetatable(t), k)
end

local function structNewIndex (t, k, v)
  if not getmetatable(t)._fields[k] then structInvalidField(getmetatable(t), k) end
  rawset(t, k, v)
end
local function structFmt(t)
  local b = {structTy(t)}; fmtTableRaw(b, t)
  return table.concat(b, '')
end

-- TODO: add sortedFields and improve print
-- TODO: add methods
local function struct(ty, fields)
  fields = set.new(fields)
  local st = {ty = ty}
  map.update(st, {
    _fields=fields, __index=structIndex, __newindex=structNewIndex,
    __tostring=structFmt,
  })
  st.new = function(data)
    for f in pairs(data) do
      if not fields[f] then structInvalidField(st, f) end
    end
    return setmetatable(data, st)
  end
  return st
end


return {
  -- Generic table operations
  eq = eq,

  -- Formatters
  fmt = fmt,
  fmtBuf = fmtBuf,
  assertEq = assertEq,

  -- struct
  struct = struct,
}
