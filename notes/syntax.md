```
iden -> alphanumeric* ;
name -> iden nameblock? ;

// i.e. name, name[foo], name[foo[bar; baz]]
nameblock -> 
  "["
  name 
  (
    ( ; name )*
    | ( , name )*
  )
  "]" ;

array -> "[" numeric* name "]" ;

letExpr -> "let" name iden "=" expression ";" ;
```
