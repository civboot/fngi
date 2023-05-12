"""
I was confused how mergesort works on linked lists, and many of the answers did
not make it clear.

"""
import dataclasses

@dataclasses.dataclass
class Sll:
  v: int
  next: 'Sll' = None

  def __repr__(self):
    out = ["["]
    node = self
    while node:
      out.append(str(node.v))
      node = node.next
    out.append("]")
    return ' '.join(out)


@dataclasses.dataclass
class Root:
  root: Sll = None

  def pop(self) -> Sll:
    out = self.root
    self.root = out.next
    out.next = None
    return out

  def __len__(self):
    l = 0; node = self.root
    while node:
      l += 1; node = node.next
    return l

  def __repr__(self):
    if not self.root: return "[ ]"
    return repr(self.root)

  # For debug/tests

  @classmethod
  def frStr(cls, s):
    """Create from string."""
    first = Sll(None); last = first
    for v in s.split():
      last.next = Sll(int(v))
      last = last.next
    return Root(first.next)


def sortedMerge(a: Sll, b: Sll) -> Sll:
  """Merge two sorted Slls so the output is sorted."""
  first = Sll(None)
  addTo = first
  while a and b:
    if a.v < b.v:
      addTo.next = a; a = a.next
    else:
      addTo.next = b; b = b.next
    addTo = addTo.next
  while a: addTo.next = a; a = a.next; addTo = addTo.next
  while b: addTo.next = b; b = b.next; addTo = addTo.next
  return first.next


def popSort2(sll: Root) -> Sll:
  """Pop 2 items from Sll and return them sorted."""
  if(not sll.root): return Root(None)
  a = sll.pop()
  if(not sll.root): return a
  b = sll.pop()
  if a.v < b.v:
    a.next = b; return a
  else:
    b.next = a; return b


def _mergesort(fr: Root, count: int) -> Sll:
  if count <= 2: return popSort2(fr)
  aLen = count // 2
  a = _mergesort(fr, aLen)
  b = _mergesort(fr, count - aLen)
  return sortedMerge(a, b)


def mergesort(fr: Root):
  out = _mergesort(fr, len(fr))
  assert not fr.root
  return out


print("basic root:", Root.frStr("1 2 3 4"))
print("sort2:", popSort2(Root.frStr("2 1")))
print("sort2:", popSort2(Root.frStr("1 2")))

print("sorted merge: ",
      sortedMerge(
          Root.frStr("1 2 3 5").root,
          Root.frStr("1 3 7 8 10 10 13").root))

nodes = Root.frStr("9 7 9 3 4 6 1 2")
print("Start with:", nodes)
s = mergesort(nodes)
print("Done:", s)


