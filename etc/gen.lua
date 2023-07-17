require('gciv')

local CONST_PAT = (
  'const%s+(%w+)%s*:'
  .. '%s*(%w+)%s*='
  .. '%s*(%w+)'
)
local STRUCT_PAT = 'struct%s+(%w+)%s*(%b[])'
local TY_PAT = '(&*)%s*(%w+)'
local FIELD_PAT = '(%w+)%s*:%s*' .. TY_PAT

local NATIVE_TYS = Set{"U1", "U2", "U4", "S", "I1", "I2", "I4"}

local RENAME = {
    Arena  = "SpArena",
    Reader = "SpReader",
    Writer = "SpWriter",
    Fmt    = "SpFmt",
    Logger = "SpLogger",
}

local tyDictDefined = false

local FConst = struct('FConst', {'name', 'ty', 'value'})
-- Const: {name, ty, value}
local function parseConsts(s, to)
  if not to then to = Map{} end
  for name, ty, value in string.gmatch(s, CONST_PAT) do
    to[name] = FConst{name=name, ty=ty, value=value}
  end
  return to
end

local FField = struct('FField', {
  {'name', String}, {'refs', Int}, {'ty', String}
})
local FStruct = struct('FStruct', {
  {'name', String}, {'fields', List}
})

local function parseStructs(s, to)
  if not to then to = Map{} end
  for sname, body in string.gmatch(s, STRUCT_PAT) do
    fields = List{}
    for name, refs, ty in string.gmatch(body, FIELD_PAT) do
      if name == 'parent' then fields:extend(to[ty].fields)
      else
        fields:add(FField{
          name=name, refs=string.len(refs), ty=ty
        })
      end
    end
    to[sname] = FStruct{name=sname, fields=fields}
  end
  return to
end

local function parseFngi(path, to)
  local text = readAll(path)
  if not to then
    to = Map{consts=Map{}, structs=Map{}}
  end
  parseConsts(text, to.consts)
  parseStructs(text, to.structs)
  return to
end

local spor = parseFngi('src/spor.fn')
local dat = parseFngi('src/dat.fn')
local structsBefore = Set(dat.structs)

--  parsed = [Const(*m.group("name", "ty", "value")) for m in CONST_RE.finditer(text)]
--  consts = [(c.name, lit(c.value)) for c in parsed if c.isNative()]
--
--  for sm  in STRUCT_RE.finditer(text):
--    sname = sm.group('name'); fields = []
--    for m in FIELD_RE.finditer(sm.group('body')):
--      name, refs, ty = m.group("name", "refs", "ty")
--      if name == 'parent': fields.extend(structs[ty].fields)
--      else: fields.append(Field(name, len(refs) if refs else 0, ty))
--    structs[sname] = Struct(sname, fields)
--
--  return consts

return {
  parseConsts = parseConsts,
  parseStructs = parseStructs,
  STRUCT_PAT = STRUCT_PAT,
  FIELD_PAT = FIELD_PAT,
  FConst = FConst, FField = FField, FStruct = FStruct,
  spor = spor,
}
