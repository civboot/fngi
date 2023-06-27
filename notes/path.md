I've found the current ftOffset and srOffset, while quite clever, to be lacking.

There needs to be a better API for "get next token chain."

Let's do some example code:

```
struct A [ a0:U2 a1:U2 ]
struct B [ b0:A  b1:&A b2:U4]
struct C [ c0:B  c1:&B c2:A ]


impl A [
  meth aMeth[self:&Self -> U4 ] do (self.a0 + self.a1 )
]
```

Okay, how do we represent these paths. Assume that all
the first elements are variables.

What do we actually _NEED_:

* We need to know the _TyI of the result_
* We need to know the _offset necessary for fetching or storing_

```
\ field compiles to how to _get_ the variable
\ { op=FTLL, tyI=(U2, &0), offset=a.v + a0.v }
a.a0

\ { op=FTLL, tyI=(U2, &0), offset=a.v + a1.v }
a.a1

\ Method compiles to which method and the location
\ of Self.
\ Note: method always ends the path, so if
\       the below was followed by `.+` then that
\       would be compiled next.

\ { op=FTLL, tyI=(A, &0), offset=a.v }
\ { op=XL, tyI=(TyFn(aMeth), &0), offset=0 }
a.aMeth

\ { op=FTLL, tyI=(A, &0), offset=b.v + b0.v }
b.b0

\ { op=FTLL, tyI=(U2, &0), offset=b.v + b0.v + a1.v }
b.b0.a1

\ { op=FTLL, tyI=(A, &1),  offset=b.v + b1.v }
\ { op=FTO,  tyI=(U2, &0), offset=a1.v}
b.b1.a1

\ { op=FTLL, tyI=(U4, &0), offset=b.v + b2.v }
b.b2

\ { op=FTLL, tyI=(A, &0), offset=c.v + c0.v + b0.v }
c.c0.b0

\ { op=FTLL, tyI=(U2, &0), offset=c.v + c0.v + b0.v + a1.v }
c.c0.b0.a1

\ { op=FTLL, tyI=(B, &1),  offset=c.v + c1.v }
\ { op=FTO,  tyI=(U2, &0), offset=b0.v + a0.v }
c.c1.b0.a0

\ { op=FTLL, tyI=(B, &1),           offset=c.v + c1.v }
\ { op=FTO,  tyI=(A, &1),           offset=b1.v }
\ { op=XL,   tyI=(aMeth, &0), offset=0 }
c.c1.b1.aMeth
```

Through references
```
var br: &B

\ { op=FTLL, tyI=(B, &1),  offset=b.v }
\ { op=FTO,  tyI=(A, &1),  offset=b1.v }
br.b1
```

Role Access
```
role Inner [
  absmeth i[ -> S ]
]
role R [
  absmeth m[ -> S ]
]

struct RS [
  rs0: U4
  rs1: R
]

var r: R

\ { op=FTLL, tyI=(R, &0), offset=r.v }
\ { op=XL,   tyI=(m, &0), offset=0 }
r.m

var rs: RS

\ { op=FTLL, tyI=(R, &0), offset=rs.v + rs1.v }
\ { op=XL,   tyI=(m, &0), offset=0 }
rs.rs1.m
```
