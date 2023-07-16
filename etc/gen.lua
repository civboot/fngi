local civ = require('civ'); local fmt = civ.fmt

local CONST_PAT = (
  'const%s+(%w+)%s*:'
  .. '%s*(%w+)%s*='
  .. '%s*(%w+)'
)
local STRUCT_PAT = 'struct%s+(%w+)%s*(%b[])'
local TY_PAT = '(&*)%s*(%w+)'
local FIELD_PAT = '(%w+)%s*:%s*' .. TY_PAT

local NATIVE_TYS = civ.Set{"U1", "U2", "U4", "S", "I1", "I2", "I4"}

local RENAME = {
    Arena  = "SpArena",
    Reader = "SpReader",
    Writer = "SpWriter",
    Fmt    = "SpFmt",
    Logger = "SpLogger",
}

local tyDictDefined = False

-- Const: {name, ty, value}
local function parseConsts(s)
  local out = {}
  for name, ty, value in string.gmatch(s, CONST_PAT) do
    out[name] = {name=name, ty=ty, value=value}
  end
  return out
end

local function parseStructs(s, to)
  local out = {}
  for sname, body in string.gmatch(s, STRUCT_PAT) do
    fields = {}
    for name, refs, ty in string.gmatch(body, FIELD_PAT) do
      if name == 'parent' then arr.extend(fields, to.structs[ty].fields)
      else
        arr.append(fields, {name=name, refs=string.len(refs), ty=ty})
      end

    end
  end
end

local function parseFngi(text, to)
  civ.update(to.consts, parseConsts(text))
  civ.update(to.structs, parseStructs(text))
end

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
  STRUCT_PAT = STRUCT_PAT,
  FIELD_PAT = FIELD_PAT,
}
