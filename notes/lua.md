# Ideas for Lua

Need to add a `help` function

* displays globals information
* can query it for nested search
* data is output in a Schema

Iterator:
* function which when called repeatedly with no arguments returns (possibly
  different) values.
* when it returns nil as the first value it is done.
* it can also be a table with `__call`

Schema type:

* named columns, each of which is a list
* prints into a nice table
* all columns have implicit primary key of index
* has keyed columns of data
* each column can be in a different form:
  * List: normal way
  * function: call with Query object, get iterator

Query

* indexes: List or iterator of indexes
* filter: function(index, value) returns true for keep
* eq, lt, gt, lte, gte: value search
* pattern: pattern search
