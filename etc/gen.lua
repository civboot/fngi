require('gciv')

local CONST_PAT = (
  'const%s+([_%w]+)%s*:'
  .. '%s*([_%w]+)%s*='
  .. '%s*([_%w]+)'
)
local STRUCT_PAT = 'struct%s+(%w+)%s*(%b[])'
local TY_PAT = '(&*)%s*([_%w]+)'
local FIELD_PAT = '([_%w]+)%s*:%s*' .. TY_PAT

local NATIVE_TYS = Set{"U1", "U2", "U4", "S", "I1", "I2", "I4"}

local UNSIZED = Set{'MMV', 'LCL', 'XL', 'XW', 'XLL', 'XRL', 'SLIT'}

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
  if not to then to = List{} end
  for name, ty, value in string.gmatch(s, CONST_PAT) do
    to:add(FConst{name=name, ty=ty, value=value})
  end
  return to
end

local FField = struct('', {
  {'name', String}, {'refs', Int}, {'ty', String}
})
local FStruct = struct('FStruct', {
  {'name', String}, {'fields', List}
})

local allStructs = Map{}
local function parseStructs(s, to)
  if not to then to = List{} end
  for sname, body in string.gmatch(s, STRUCT_PAT) do
    local fields = List{}
    for name, refs, ty in string.gmatch(body, FIELD_PAT) do
      if name == 'parent' then fields:extend(allStructs[ty].fields)
      else
        fields:add(FField{
          name=name, refs=string.len(refs), ty=ty,
        })
      end
    end
    local st = FStruct{name=sname, fields=fields}
    allStructs[sname] = st; to:add(st)
  end
  return to
end

local function parseFngi(path, to)
  local text = readAll(path)
  if not to then
    to = Map{consts=List{}, structs=List{}}
  end
  parseConsts(text,  to.consts)
  parseStructs(text, to.structs)
  return to
end


local spor = parseFngi('src/spor.fn')
local dat = parseFngi('src/dat.fn')

local datStructs = copy(allStructs)
local comp = parseFngi('src/comp.fn')
local compStructs = allStructs:diff(datStructs)

local function writeConsts(f, path, consts)
  f:write'// DO NOT EDIT MANUALLY! GENERATED BY etc/gen.lua\n'
  f:write('// See docs at: ' .. path .. '\n')
  f:write'#include "civ.h"\n\n'
  for _, c in ipairs(consts) do
    f:write('#define ' .. c.name .. '  ' .. c.value .. '\n')
  end
  return b
end

RENAME = {
    Arena=  "SpArena",
    Reader= "SpReader",
    Writer= "SpWriter",
    Fmt=    "SpFmt",
    Logger= "SpLogger",
}


local function writeStruct(f, s, needDeclare)
end

local function writeStructs(f, structs, needDeclare)
  local needDeclare = needDeclare or {}
  for _, s in ipairs(structs) do
    local sname = RENAME[s.name] or s.name
    f:write(concat{'\ntypedef struct _', sname, ' {\n'})
    for _, field in ipairs(s.fields) do
      local ty_ = RENAME[field.ty] or field.ty
      if ty_ == 'Self' then ty_ = 'struct _' .. sname end
      if needDeclare[ty_] then ty_ = 'struct _' .. ty_ end
      f:write(concat{'  ', ty_}); for _=1, field.refs do f:write'*' end
      f:write(concat{' ', field.name, ';\n'})
    end
    f:write(concat{'} ', sname, ';\n'})
    needDeclare[sname] = nil
  end
end

local COMP_H = [=====[

// -- this part is actually hand written in gen.lua --
struct _TyDict;
struct _TyFn;

typedef struct {
  struct _TyFn*  drop;     // this:&This -> ()
  struct _TyFn*  alloc;    // this:&This sz:S alignment:U2 -> Ref
  struct _TyFn*  free;     // this:&This dat:Ref sz:S alignment:U2 -> ()
  struct _TyFn*  maxAlloc; // this:&This -> S
} MSpArena;
typedef struct { void* d; MSpArena* m; } SpArena;

typedef struct {
  struct _TyFn*  read;   // this:&This -> ()
  struct _TyFn*  asBase; // this:&This -> &BaseFile
} MSpReader;
typedef struct { void* d; MSpReader* m; } SpReader;

typedef struct {
  struct _TyFn*  asBase; // this:&This -> &BaseFile
  struct _TyFn*  write;  // this:&This -> ()
} MSpWriter;
typedef struct { void* d; MSpWriter* m; } SpWriter;

typedef struct {
  MSpWriter w;
  struct _TyFn*  state;  // this:&This -> &FmtState
} MSpFmt;
typedef struct { void* d; MSpFmt* m; } SpFmt;

typedef struct {
  MSpFmt         fmt;
  struct _TyFn*  logConfig;  // this:&This -> &LogConfig

  struct _TyFn*  start;      // this:&This U1  -> U1
  struct _TyFn*  add;        // this:&This Slc -> ()
  struct _TyFn*  end;        // this:&This     -> ()
} MSpLogger;
typedef struct { void* d; MSpLogger* m; } SpLogger;

// -- everything after this is truly generated --
]=====]

local NAME_C = [===[
/* Custom generated by etc/gen.lua */

#include "civ.h"
#include "spor.h"

/*extern*/ U1* unknownInstr = "UNKNOWN";

Slc instrName(U1 instr) {
  switch(instr) {
]===]


local function writeCase(f, name, ret)
  ret = ret or name
  f:write(concat{'    case ', name, ': return Slc_ntLit("', ret, '");\n'})
end

local function withSize(f, name)
  for _, sz in ipairs({'1', '2', '4'}) do
    writeCase(f, string.format('%s + SZ%s', name, sz), name .. sz)
  end
end

local function slitCases(f)
  for v=0,0x29 do
    writeCase(f, string.format('SLIT + 0x%X', v), string.format('0x%X', v))
  end
end

local sz, instrs = Map{}, List{}
for _, c in pairs(spor.consts) do
  if string.match(c.name, '^SZ') then sz[c.name] = c.value
  else instrs:add({instr=tonumber(c.value), name=c.name}) end
end

local function writeCLang(f, path, mod)
  writeConsts(f,  path, mod.consts)
  writeStructs(f, mod.structs)
  return f
end

local function main()
  print("## Generating C using lua")
  local f = io.open('gen/name.c', 'w')
  f:write(NAME_C)
  for _, i in ipairs(instrs) do
    if i.name == 'SLIT'                        then slitCases(f)
    elseif (i.instr < 0x40) or UNSIZED[i.name] then writeCase(f, i.name)
    else                                withSize(f, i.name, i.instr)
    end
  end
  f:write'  }\n'
  f:write'  return (Slc) {.dat = unknownInstr, .len = 7};\n'
  f:write'}\n'
  f:close()

  local sporPath, compPath = 'gen/spor.h', 'gen/comp.h'
  writeCLang(io.open(sporPath, 'w'), sporPath, spor):close()
  f = io.open(compPath, 'w')
  writeConsts(f,  compPath, comp.consts)
  f:write(COMP_H)

  writeStructs(f, comp.structs, {TyDict=1, TyFn=1})
  f:close()
end


if not TESTING then main() end
return {
  parseConsts = parseConsts,
  parseStructs = parseStructs,
  STRUCT_PAT = STRUCT_PAT,
  FIELD_PAT = FIELD_PAT,
  FConst = FConst, FField = FField, FStruct = FStruct,
  spor = spor, dat = dat, comp=comp,
  datStructs = datStructs, compStructs = compStructs,

  genConsts = genConsts,
}
