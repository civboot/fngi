```
type = name | array fnType | data

integer -> numeric+
iden -> alpha alphanumeric* ;

# i.e. name, name[foo], name[foo[bar; baz]]
name -> iden nameblock?

nameblock -> (
  "["
  name 
  (
    ( ";" name )*
    | ( "," name )*
  )
  "]"
)

# arr[32; U32]
array -> "arr[" integer ";" name "]"
inout -> type "->" type

fn -> "fn" name ":" inout block
let -> "let" iden ":" type "=" expression ";"

structElem = iden ( ":" name )? ( "=" expression )? ";"
data = "{" (structElem*) "}"


```
