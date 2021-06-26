```
integer -> numeric+
iden -> alpha alphanumeric* ;
// i.e. name, name[foo], name[foo[bar; baz]]
name -> iden nameblock? ;
nameblock -> 
  "["
  name 
  (
    ( ; name )*
    | ( , name )*
  )
  "]" ;

array -> "arr[" integer name "]" ;

fnsubset -> type "->" type

fntype = "fn[" fnsubset "]"
type = name | array | fnType

fn -> "fn" name ":" fnType block
let -> "let" iden ":" type "=" expression ";" ;


```
