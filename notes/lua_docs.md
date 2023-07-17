
# Metamethods
Copy/paste of http://lua-users.org/wiki/MetatableEvents

```
__index - Control 'prototype' inheritance. When accessing "myTable[key]" and the key does not appear in the table, but the metatable has an __index property:
if the value is a function, the function is called, passing in the table and the key; the return value of that function is returned as the result.
if the value is another table, the value of the key in that table is asked for and returned
(and if it doesn't exist in that table, but that table's metatable has an __index property, then it continues on up)
Use "rawget(myTable,key)" to skip this metamethod.
__newindex - Control property assignment. When calling "myTable[key] = value", if the metatable has a __newindex key pointing to a function, call that function, passing it the table, key, and value.
Use "rawset(myTable,key,value)" to skip this metamethod.
(If the __newindex function does not set the key on the table (using rawset) then the key/value pair is not added to myTable.)
__mode - Control weak references. A string value with one or both of the characters 'k' and 'v' which specifies that the the keys and/or values in the table are weak references.
__call - Treat a table like a function. When a table is followed by parenthesis such as "myTable( 'foo' )" and the metatable has a __call key pointing to a function, that function is invoked (passing the table as the first argument, followed by any specified arguments) and the return value is returned.
__metatable - Hide the metatable. When "getmetatable( myTable )" is called, if the metatable for myTable has a __metatable key, the value of that key is returned instead of the actual metatable.
__tostring - Control string representation. When the builtin "tostring( myTable )" function is called, if the metatable for myTable has a __tostring property set to a function, that function is invoked (passing myTable to it) and the return value is used as the string representation.
__len - (Lua 5.2+) Control table length that is reported. When the table length is requested using the length operator ( '#' ), if the metatable for myTable has a __len key pointing to a function, that function is invoked (passing myTable to it) and the return value used as the value of "#myTable".
__pairs - (Lua 5.2+) Handle iteration through table pairs when for k,v in pairs(tbl) do ... end is called (See GeneralizedPairsAndIpairs).
__ipairs - (Lua 5.2+) Handle iteration through table pairs when for k,v in ipairs(tbl) do ... end is called (See GeneralizedPairsAndIpairs).
__gc - Userdata finalizer code. When userdata is set to be garbage collected, if the metatable has a __gc field pointing to a function, that function is first invoked, passing the userdata to it. The __gc metamethod is not called for tables. (See http://lua-users.org/lists/lua-l/2006-11/msg00508.html)
__name - When it contains a string, may be used by tostring and in error messages.
__close - (Lua 5.4+) Makes metatable to-be-closed variable, if not set to nil or false.
Mathematic Operators
__unm - Unary minus. When writing "-myTable", if the metatable has a __unm key pointing to a function, that function is invoked (passing the table), and the return value used as the value of "-myTable".
__add - Addition. When writing "myTable + object" or "object + myTable", if myTable's metatable has an __add key pointing to a function, that function is invoked (passing the left and right operands in order) and the return value used.
''If both operands are tables, the left table is checked before the right table for the presence of an __add metaevent.
__sub - Subtraction. Invoked similar to addition, using the '-' operator.
__mul - Multiplication. Invoked similar to addition, using the '*' operator.
__div - Division. Invoked similar to addition, using the '/' operator.
__idiv - (Lua 5.3) Floor division (division with rounding down to nearest integer). '//' operator.
__mod - Modulo. Invoked similar to addition, using the '%' operator.
__pow - Involution. Invoked similar to addition, using the '^' operator.
__concat - Concatenation. Invoked similar to addition, using the '..' operator.
Bitwise Operators
Lua 5.3 introduced the ability to use true integers, and with it bitwise operations. These operations are invoked similar to the addition operation, except that Lua will try a metamethod if any operand is neither an integer nor a value coercible to an integer.

__band - (Lua 5.3) the bitwise AND (&) operation.
__bor - (Lua 5.3) the bitwise OR (|) operation.
__bxor - (Lua 5.3) the bitwise exclusive OR (binary ^) operation.
__bnot - (Lua 5.3) the bitwise NOT (unary ~) operation.
__shl - (Lua 5.3) the bitwise left shift (<<) operation.
__shr - (Lua 5.3) the bitwise right shift (>>) operation.
Equivalence Comparison Operators
__eq - Check for equality. This method is invoked when "myTable1 == myTable2" is evaluated, but only if both tables have the exact same metamethod for __eq.
For example, see the following code:

__lt - Check for less-than. Similar to equality, using the '<' operator.
Greater-than is evaluated by reversing the order of the operands passed to the __lt function.
a > b == b < a
__le - Check for less-than-or-equal. Similar to equality, using the '<=' operator.
Greater-than-or-equal is evaluated by reversing the order of the operands passed to the __le function.
a >= b == b <= a
```
