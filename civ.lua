
local CHECK = {field = false}

-- ###################
-- # Utility Functions

local function identity(v) return v end
-- return keys array
local function keysarr(t)
  local a = {}; for k in pairs(t) do table.insert(a, k) end
  return a
end

-- convert array to an iterator
local function iterarr(l)
  local i, n = 0, #l
  return function()
    i = i + 1; if i <= n then  return i, l[i]  end
  end
end

local concat = table.concat
local function sort(t) table.sort(t) return t end
local function orderedKeys(t) return iterarr(sort(keysarr(t))) end

-- return as the index (first value) from an iterator when there is no index
local NoIndex = setmetatable({}, {__name='NoIndex'})
local function keysIter(t)
  local i, keys = 0, keysarr(t)
  return function()
    i = i + 1; if i < #keys then return NoIndex, keys[i] end
  end
end

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

local _tyName = {
  boolean = identity, number = identity, string = identity,
  ['nil'] = identity, ['function'] = identity,
}
-- Get the name of obj's type
local function tyName(obj)
  local n = _tyName[type(obj)]; if n then return n end
  local mt = getmetatable(obj)
  if not mt or not mt.__name then return type(obj) end
  return mt.__name
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
  error("Unknown member: " .. tyName(self) .. "." .. k)
end

local Set = nil

local ALL_TYS = {}

-- Types are generated from other types.
-- You can retreive or add them here.
--
-- Example:
--   generated = genTyRepo({A, B})
--   if not generated then
--     generated = genTyRepo({A, B})
--   end
local GEN_TYS = {}
local function genTyRepo(tys, first)
  local t = GEN_TYS
  -- if first then t = genTyRepo(first) end
  for _, ty_ in ipairs(tys) do
    if not ty_ then return nil end -- error
    local e = t[ty_]
    if not e then e = {}; t[ty_] = e end
    t = e
  end
  return t
end

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
  ALL_TYS[ty] = ty
  return setmetatable(ty, {
    __name=concat{'Ty[', name, ']'},
    __tostring=function(_) return name end,
  })
end

local Nil  = newTy('Nil');  local Fn   = newTy('Fn')
local Bool = newTy('Bool'); local Num  = newTy('Num')
local Str  = newTy('Str');  local Tbl  = newTy('Tbl')

assert('Ty[Str]' == tyName(Str)); assert('table' == tyName({}))

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


-- Get the type of a container based on the value
local function containerTy(cont)
  for _, v in pairs(cont) do
    return ty(v)
  end
  error("contains no values!")
end

-- In some cases we cannot trust an object's __index
local function callMethod(obj, meth, ...)
  return ty(obj)[meth](obj, ...)
end


-- check if given is a subtype (descendant) of req
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
-- this is NOT right
-- method(Iter, 'filter', function(self, fn)
--   return function()
--     for i, v in self do
--       if fn(v) then return i, v end
--     end
--   end
-- end)
method(Iter, '__call', function(self, l)
  if self.i < #self.data then
    self.i = self.i + 1
    return self.i, self.data[self.i]
  end
end)

local Range = newTy('Range')
constructor(Range, function(ty, start, end_, step)
  return setmetatable({start=start, end_=end_, step=step})
end)
method(Range, '__call', function(self, l)
  if self.i < self.end_ then
    self.i = self.i + self.step
    return self.i
  end
end)

local result = Iter{3, 4}
assert('Iter' == tyName(result))
assert(3 == select(2, result()))
assert(4 == select(2, result()))
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
  for _, key in pairs(t) do s[key] = key end
  return setmetatable(s, ty)
end
method(Set, '__index',    _methIndex)
method(Set, 'add', function(self, k)  self[k] = true  end)
method(Set, 'union', function(self, r)
  local both = Set{}
  for k in pairs(self) do if r[k] then both[k] = k end end
  return both
end)
method(Set, 'leftOnly', function(self, r)
  local left = Set{}
  for k in pairs(self) do if not r[k] then left[k] = k end end
  return left
end)

-- Note: Bool is intentionally not here.
-- Technically it can be used, but it causes awkwardness
-- in APIs and there is really no value in using it.
local KEY_TYS = Set{Num, Str}

-- List: ordered set of possibly duplicated values
-- A list is just the normal Lua list with type information and a few methods.
-- Use iparis to iterate over it, etc
--
--   l = List{'a', 'b', 1, 2}
--   l:add(3)
--   l:extend{1, 2, 3}
--   l[1] get first (aka very first) index.
local List = newTy("List")
getmetatable(List).__call = function(ty, t)
  return setmetatable(t, ty)
end
method(List, '__index', _methIndex)
method(List, 'iter', ipairs)
method(List, 'add', table.insert)
method(List, 'extend', extend)
List.fromIter = function(iter)
  local out = List{}
  for _, v in iter do out:add(v) end
  return out
end
method(List, 'iter', iterarr)

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
  for _, key in keys do
    fmtBuf(b, key);    table.insert(b, '=')
    fmtBuf(b, t[key]); table.insert(b, ' ')
    endAdd = 0 -- remove space for last
  end
  b[#b + endAdd] = '}'
end

local _bufFmtTy = {
  number = table.insert,
  boolean = function(b, bool)
    if(bool) then table.insert(b, 'true')
    else          table.insert(b, 'false') end
  end,
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
  return concat(b)
end)
fmtBuf = function (b, obj) _bufFmtTy[type(obj)](b, obj) end

-- Set.__tostring
method(Set, '__tostring', function(self)
  local b, endAdd = {'{'}, 1
  for _, k in orderedKeys(self) do
    fmtBuf(b, k); table.insert(b, ' '); endAdd = 0
  end
  b[#b + endAdd] = '}'; return concat(b)
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
  b[#b + endAdd] = ']'; return concat(b)
end)
assert("[5 6 3 4]"  == tostring(List{5, 6, 3, 4}))
assert("{a=5 b=77}" == tostring(Map{a=5, b=77}))

-- fmt any object
local function fmt(obj)
  local b = {}; fmtBuf(b, obj);
  return concat(b, "")
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
local function eqDeep(a, b)
  if rawequal(a, b)     then return true   end
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
    if a == b                       then return true   end
    return eqDeep(a, b)
  end,
}
eq = function(a, b) return eqTy[type(a)](a, b) end

assert(eq(2, 2))
assert(eq({4, 5}, {4, 5}))
assert(eq({4, {5, 6}}, {4, {5, 6}}))
assert(not eq({4, {5, 6, 7}}, {4, {5, 6}}))
assert(not eq({4, {5, 6}}, {4, {5, 6, 7}}))

-- Now update the eq for our types
method(Map, '__eq', eqDeep)
method(Set, '__eq', function(self, s)
  for k in pairs(self) do
    if(not s[k])    then return false end
  end
  for k in pairs(s) do
    if(not self[k]) then return false end
  end
  return true
end)
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
  error(concat(err))
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
  if CHECK.field and not getmetatable(t)["#tys"][k] then
    structInvalidField(getmetatable(t), k)
  end
  rawset(t, k, v)
end

local function structFmt(t)
  local b = {tyName(t)}
  fmtTableRaw(b, t, Iter(getmetatable(t)['#ordered']))
  return concat(b, '')
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

-- Examples: to get value of
--   a.a1       pathVal(a, {'a1'})
--   a.a1.b2    pathVal(a, {'a1', 'b2'})
local function pathVal(st, path)
  for _, p in ipairs(path) do st = st[p] end
  return st
end

-- Examples: to get type of
--   A.a1       pathTy(A, {'a1'})
--   A.a1.b2    pathTy(A, {'a1', 'b2'})
local function pathTy(st, path)
  for _, p in ipairs(path) do st = st["#tys"][p] end
  return st
end

local function matches(text, m)
  local out = {}; for v in string.gmatch(text, m) do
    table.insert(out, v) end
  return out
end
local function dotSplit(pathStr)
  return matches(pathStr, "[^%.]*")
end

local function tyCheckPath(st, path, given)
  local req = pathTy(st, path)
  if not tyCheck(req, given) then error(string.format(
    "%s not is not ancestor of %s (%s.%s)",
    given, req, st, fmt(path)
  ))end
end

local struct = newTy('Struct')
constructor(struct, function(ty_, name, fields)
  local st = newTy(name)
  local tys, defaults, ordered = specifyFields(fields)
  update(st, {
    ["#tys"]=tys,  ["#ordered"]=ordered,
    ["#defaults"]=defaults,
    __index=structIndex, __newindex=structNewIndex,
    __tostring=structFmt,
  })
  constructor(st, function(st, t)
    for f, v in pairs(t) do
      local fTy = st["#tys"][f]
      if not fTy then structInvalidField(st, f) end
      if not tyCheck(fTy, ty(v)) then tyError(fTy, ty(v)) end
    end
    for f in pairs(st["#tys"]) do
      if nil == t[f] and nil == st["#defaults"][f] then
        error("missing field: " .. f)
      end
    end
    return setmetatable(t, st)
  end)
  return st
end)

-- ###################
-- # File Helpers
local function readAll(path)
  local f = io.open(path, 'r')
  local out = f:read('a'); f:close()
  return out
end

-- ###################
-- # Picker / Query
-- Picker is an ergonomic way to query over a list of structs
-- while using struct indexes

local Picker = newTy('Picker')
local Query = struct('Query', {
    -- data comes from one of these
    {'#picker', Picker, false}, {'#i', Num, false}, -- i=picker index
    {'#iter', nil, false}, {'#struct', nil, false},

    -- path and ops are built-up by user
    {'#path', List}, {'#ops', List},
  })
local PickerIter = newTy('PickerIter')
local PathBuilder = struct('PathBuilder', {'#query'})
local QueryOp = struct('Op',
  {{'name', Str}, {'path'}, {'value'}})

local function fmtStructFull(stTy)
  local endAdd, b = 1, List{stTy.__name, '{'}
  for _, field in ipairs(stTy['#ordered']) do
    local fieldTy = stTy['#tys'][field]
    if true == fieldTy then fieldTy = 'Any' end
    b:extend{field, ':', tostring(fieldTy), ' '}
    endAdd = 0
  end
  b[#b + endAdd] = '}'
  return concat(b)
end
local function genStruct(name, namedTys)
  local repo = genTyRepo(namedTys, {name})
  if not repo then error(string.format(
    "All types must be defined in path: %s", sel))
  end
  if repo.ty then return repo.ty end
  local fields = List{}
  local i = 1; while i+1 <= #namedTys do
    fields:add{namedTys[i], namedTys[i+1]}
    i = i + 2
  end
  local st = struct(name, fields); repo.ty = st
  getmetatable(st).__tostring = fmtStructFull
  return repo.ty
end

-- A set of query operations to perform on a Picker
-- path is built up by multiple field accesses.
method(Query, 'new',  function(picker)
  return Query{['#picker']=picker, ['#path']=List{}, ['#ops']=List{},
               ['#i']=0}
end)

-- A picker itself, which holds the struct type
-- and the data (or the way to access the data)
constructor(Picker, function(ty, struct, data)
  local p = {struct=struct, data=data, len=#data}
  return setmetatable(p, ty)
end)
method(Picker, '__index', function(self, k)
  local mv = getmetatable(self)[k]; if mv then return mv end
  return Query.new(self)[k]
end)
method(Picker, '__tostring', function(self)
  return string.format("Picker[%s len=%s]",
    rawget(self, 'struct').__name, #rawget(self, 'len'))
end)

local function queryStruct(q)
  return rawget(q, '#struct') or q['#picker'].struct
end
local function queryCheckTy(query, ty_)
  local st = queryStruct(query)
  tyCheckPath(st, query['#path'], ty_)
end

local function _queryOpImpl(query, op, value, ty_)
  queryCheckTy(query, ty_)
  query['#ops']:add(QueryOp{
    name=op, path=query['#path'], value=value})
  query['#path'] = List{}
  return query

end
method(Query, 'in_', function(self, value)
  if 'table' == type(value) then value = Set(value) end
  if Set ~= ty(value) then error(
    "in_ must be on Set, got " .. tyName(value)
  )end
  return _queryOpImpl(self, 'in_', value, containerTy(value))
end)
local function _queryOp(op)
  return function(self, value)
    queryCheckTy(self, ty(value))
    self['#ops']:add(QueryOp{
      name=op, path=self['#path'], value=value})
    self['#path'] = List{}
    return self
  end
end
for _, op in pairs({'filter', 'eq', 'lt', 'lte', 'gt', 'gte'}) do
  method(Query, op, _queryOp(op))
end

local function querySelect(iter, stTy, paths)
  return function()
    local i, st = iter(); if nil == i then return end
    local keys = {}; for key, path in pairs(paths) do
      keys[key] = pathVal(st, path)
    end
    local out = stTy(keys)
    return i, out
  end
end
-- select{'a.b', 'c.d'}}       -- accessible through .c, .d
-- select{{x='a.b', y='c.d'}}, -- now .x, .y
method(Query, 'select', function(self, sel)
  local picker = self['#picker']
  local st = queryStruct(self)
  local paths, tys, namedTys = {}, {}, List{}
  for key, p in pairs(sel) do
    -- path: can 'a.b' instead of {'a', 'b'}
    if 'string' == type(p) then p = dotSplit(p) end
    -- by index uses the last field name
    if 'number' == type(key) then key = p[#p] end
    if 'string' ~= type(key) then error(
      'must provide name for non-key path: ' .. fmt(key)
    )end
    if paths[key] then error(
      'key used multiple times: ' .. key
    )end
    paths[key] = p; tys[key] = pathTy(st, p)
    namedTys:extend{key, tys[key]}
  end
  self['#path'] = List{}
  local st = genStruct('Q', namedTys)
  return Query{
    ['#iter']=querySelect(self:iter(), st, paths),
    ['#struct']=st,
    ['#path']=List{}, ['#ops']=List{},
  }
end)

-- The __index function for Query.
-- Mostly you do queries doing `q.structField.otherField.lt(3)`
--
-- If a struct field is (i.e) 'lt' you can use `path` like so:
--   q.path.lt.eq(3) -- lt less equal to 3

local function buildQuery(query, field)
  local st = queryStruct(query)
  st = pathTy(st, query['#path'])
  if not st['#tys'][field] then error(
    string.format("%s does not have field %s", st, field)
  )end
  query['#path']:add(field);
  return query
end
method(Query, '__index', function(self, k)
  local mv = getmetatable(self)[k]; if mv then return mv end
  if 'path' == k then return PathBuilder{['#query']=self} end
  return buildQuery(self, k)
end)
method(PathBuilder, '__index', function(self, field)
  return buildQuery(self['#query'])
end)

local opIml = {
  in_=function(op, v)    return op.value[v]     end,
  filter=function(op, f) return f(op.value)     end,
  eq =function(op, v) return op.value == v      end,
  lt =function(op, v) return op.value <  v      end,
  lte=function(op, v) return op.value <= v      end,
  gt =function(op, v) return op.value >  v      end,
  gte=function(op, v) return op.value >= v      end,
}

method(Query, 'iter', function(self)
  if rawget(self, '#i') then self['#i'] = 0 end
  return self
end)
method(Query, '__call', function(self)
  local i = rawget(self, '#iter')
  if i then return i() end
  ::top::
  i          = self['#i']
  local data = self['#picker'].data
  if i >= #data then return nil end
  i = i+1; self['#i'] = i; local v = data[i]
  for _, op in ipairs(self['#ops']) do
    if not opIml[op.name](op, pathVal(v, op.path)) then
      goto top
    end
  end
  return i, v
end)
-- method(Join, 'join', function(self, qOther, using)
-- 
-- end)
method(Query, 'toList', function(self)
  return List.fromIter(callMethod(self, 'iter'))
end)

constructor(PickerIter, function(ty_, query)
  local pi = {i=0, query=query}
  return setmetatable(pi, ty_)
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
  Map = Map, List = List, Set = Set,
  Iter = Iter, NoIndex = NoIndex,

  -- Generic table operations
  eq = eq, update = update, extend = extend,

  -- Formatters
  fmt = fmt,
  fmtBuf = fmtBuf,

  -- struct
  struct = struct, pathVal = pathVal, pathTy = pathTy,
  dotSplit = dotSplit,
  genStruct = genStruct,

  -- Picker
  Picker = Picker,

  -- file
  readAll = readAll,

  -- tests
  test = test,
  assertEq = assertEq,
  globals = globals, assertGlobals = assertGlobals,
}
