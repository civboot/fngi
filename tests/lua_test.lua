-- this is just for me to understand lua better

function myIter(i)
  return function(newI)
    if newI then i = newI end
    i = i + 1
    if i < 10 then
      return i, i * 10
    end
  end
end

function list(iter)
  local l = {}
  for v in iter do
    table.insert(l, v)
  end
  return l
end

function join(l)
  local b = {}
  for _, v in ipairs(l) do
    table.insert(b, tostring(v))
  end
  return table.concat(b, ' ')
end


myI = myIter(5)
print("myI", join(list(myI)))
print("myI", join(list(myI)))
print("myI", join(list(myI)))

myI2 = {i=0}
setmetatable(myI2, {__call=function(self)
  self.i = self.i + 1
  if self.i < 10 then
    return self.i, self.i * 10
  end
end})


print("myI2", join(list(myI2)))
myI2.i = 3
print("myI2", join(list(myI2)))

-- ... expression causes a
-- table.unwrap(additionalArgs) anywhere it is used.
function aPrint(a, ...)
  print('...', type('...'), ...)
  print('select(1, ...)', select(1, ...))
  local b, c, d = ...
  print('b', b, 'c', c, 'd', d)
  local args = {..., z=7}
  for k, v in pairs(args) do
    print('args', k, v)
  end
  print(
    string.format('a=%s; ', a),
    table.unpack(args))
end
aPrint('thisIsA', 'and this is', 'something', 1, {})
