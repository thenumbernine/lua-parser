[![Donate via Stripe](https://img.shields.io/badge/Donate-Stripe-green.svg)](https://buy.stripe.com/00gbJZ0OdcNs9zi288)<br>

# Lua Parser in Lua

Parses to an abstract syntax tree representation.
Call tostring() on the AST to get equivalent Lua code.

Works for versions 5.1 5.2 5.3 5.4 and maybe some luajit versions depending on their compatability.

AST also contains some functions like flatten() for use with optimizing / auto-inlining Lua.

See the tests folder for example usage.

### Reference

`Parser = require 'parser'`
This will return the parser class.

`Parser.parse(data[, version, source])`
This parses the code in `data` and returns an `ast._block` object.
This is shorthand for `Parser(data, version, source).tree`
`version` is a string `'5.1', '5.2', '5.3'`, etc., corresponding to your Lua version.
The `Parser` object has a few more functions to it corresponding with internal use while parsing.
`source` is a description of the source, i.e. filename, which is included in some nodes (functions) for information on where they are declared.

`ast = require 'parser.lua.ast'`
This is the AST (abstract syntax tree) library,
it hold a collection of AST classes, each representing a different token in the Lua syntax.


`n = ast.node()`
= This is the superclass of all AST classes.

Each has the following properties:

`n.type` = returns the type of the node, coinciding with the classname in the `ast` library with underscore removed.

`n.span` = source code span information (`from` and `to` subtables each with `source`, `line` and `col` fields)

`n:copy()` = returns a copy of the node.

`n:flatten(func, varmap)` = flattens / inlines the contents of all function call of this function.  Used for performance optimizations.

`n:toLua()` = generate Lua code.  same as the node's `__tostring`.

`n:serialize(apply)` = apply a to-string serialization function to the AST.

## ast.node subclasses:

`n = ast._block(...)` = a block of code in Lua.<br>
`...` is a list of initial child `stmt` nodes to populate the `block` node with.<br>
`n.type == 'block'`.<br>
`n[1] ... n[#n] =` nodes of statements within the block.<br>
<br>
`n = ast._stmt()` = a statement-node parent-class.<br>
<br>
`n = ast._assign(vars, exprs)` = <br>
An assignment operation.<br>
Subclass of `_stmt`.  <br>
`n.type == 'assign'`.<br>
Represents the assignment of `n.vars` to `n.exprs`.<br>
<br>
`n = ast._do(...)` = <br>
A `do ... end` block.  <br>
Subclass of `_stmt`.  <br>
`n.type == 'do'`.<br>
`n[1] ... n[#n] =` nodes of statements within the block.<br>
<br>
`n = ast._while(cond, ...)` = <br>
A `while cond do ... end` block.<br>
Subclass of `_stmt`.  <br>
`n.type == 'while'`.<br>
`n.cond` holds the condition expression.<br>
`n[1] ... n[#n] =` nodes of statements within the block.<br>
<br>
`n = ast._repeat(cond, ...)` = <br>
A `repeat ... until cond` block.<br>
Subclass of `_stmt`.<br>
`n.type == 'repeat'`.<br>
`n.cond` holds the condition expression.<br>
`n[1] ... n[#n] =` nodes of statements within the block.<br>
<br>
`n = ast._if(cond, ...)` =<br>
A `if cond then ... elseif ... else ... end` block.<br>
Subclass of `_stmt`.<br>
`n.type == 'if'`.<br>
`n.cond` holds the condtion expression of the first `if` statement.<br>
All subsequent arguments must be `ast._elseif` objects, optionally with a final `ast._else` object.<br>
`n.elseifs` holds the `ast._elseif` objects.<br>
`n.elsestmt` optionally holds the final `ast._else`.<br>
<br>
`n = ast._elseif(cond, ...)` =<br>
A `elseif cond then ...` block.<br>
Subclass of `_stmt`.<br>
`n.type == 'elseif'`.<br>
`n.cond` holds the condition expression of the `else` statement.<br>
`n[1] ... n[#n] =` nodes of statements within the block.<br>
<br>
`n = ast._else(...)` =<br>
A `else ...` block.<br>
`n.type == 'else'`.<br>
`n[1] ... n[#n] =` nodes of statements within the block.<br>
<br>
`n = ast._foreq(var, min, max, step, ...)` =<br>
A `for var=min,max[,step] do ... end` block.<br>
Subclass of `_stmt`.<br>
`n.type == 'foreq'`.<br>
`n.var =` the variable node.<br>
`n.min =` the min expression.<br>
`n.max =` the max expression.<br>
`n.step =` the optional step expression.<br>
`n[1] ... n[#n] =` nodes of statements within the block.<br>
<br>
`n = ast._forin(vars, iterexprs, ...)`<br>
A `for var1,...varN in expr1,...exprN do ... end` block.<br>
Subclass of `_stmt`.<br>
`n.type == 'forin'`.<br>
`n.vars = ` table of variables of the for-in loop.<br>
`n.iterexprs = ` table of iterator expressions of the for-in loop.<br>
`n[1] ... n[#n] =` nodes of statements within the block.<br>
<br>
`n = ast._function(name, args, ...)`<br>
A `function [name](arg1, ...argN) ... end` block.<br>
Subclass of `_stmt`.<br>
`n.type == 'function'`.<br>
`n.name = ` the function name.  This is optional.  Omit name for this to represent lambda function. (Which technically becomes an expression and not a statement...)<br>
`n.args = ` table of arguments.  This does get modified: each argument gets assigned an `.param = true`, and an `.index =` for which index it is in the argument list.<br>
`n[1] ... n[#n] =` nodes of statements within the block.<br>
<br>
`n = ast._local(exprs)`<br>
A `local ...` statement.<br>
Subclass of `_stmt`.<br>
`n.type == 'local'`<br>
`n.exprs =` list of expressions to be declared as locals.<br>
Expects its member-expressions to be either functions or assigns.<br>
<br>
`n = ast._return(...)`<br>
A `return ...` statement.<br>
Subclass of `_stmt`.<br>
`n.type == 'return'`<br>
`n.exprs =` list of expressions to return.<br>
<br>
`n = ast._break(...)`<br>
A `break` statement.<br>
Subclass of `_stmt`.<br>
`n.type == 'break'`<br>
<br>
`n = ast._call(func, ...)`<br>
A `func(...)` function-call expression.<br>
`n.type == 'call'`<br>
`n.func =` expression of the function to call.<br>
`n.args =` list argument expressions to pass into the function-call. <br>
<br>
`n = ast._nil()`<br>
A `nil` literal expression.<br>
`n.type == 'nil'`.<br>
`n.const == true`.<br>
<r>
`n = ast._boolean()`<br>
The parent class of the `true`/`false` AST nodes.
<br>
`n = ast._true()`<br>
A `true` boolean literal expression<br>
`n.type == 'true'`.<br>
`n.const == true`.<br>
`n.value == true`.<br>
`ast._boolean:isa(n)` evaluates to `true`<br>
<br>
`n = ast._false()`<br>
A `false` boolean literal expression<br>
`n.type == 'true'`.<br>
`n.const == true`.<br>
`n.value == false`.<br>
`ast._boolean:isa(n)` evaluates to `true`<br>
<br>
`n = ast._number(value)`<br>
A numeric literal expression.<br>
`n.type == 'number'`.<br>
`n.value =` the numerical value.<br>
<br>
`n = ast._string(value)`<br>
A string literal expression.<br>
`n.type == 'string'`.<br>
`n.value =` the string value.<br>
<br>
`n = ast._vararg()`<br>
A vararg `...` expression.<br>
`n.type == 'vararg'`.<br>
For use within function arguments, assignment expressions, function calls, etc.<br>
<br>
`n = ast._table(...)`<br>
A table `{ ... }` expression.<br>
`n.type == 'table'`.<br>
`n[1] ... n[#n] =` expressions of the table.<br>
If the expression in `n[i]` is an `ast._assign` then an entry is added into the table as `key = value`.  If it is not an `ast._assign` then it is inserted as a sequenced entry.<br>
<br>
`n = ast._var(name)`<br>
A variable reference expression.<br>
`n.type == 'var'`<br>
`n.name =` the variable name.<br>
<br>
`n = ast._par(expr)`<br>
A `( ... )` parenthesis expression.<br>
`n.type == 'par'`.<br>
`n.expr =` the expression within the parenthesis.<br>
<br>
`n = ast._index(expr, key)`<br>
An `expr[key]` expression, i.e. an `__index`-metatable operation.<br>
`n.type == 'index'`.<br>
`n.expr =` the expression to be indexed.<br>
`n.key =` the expression of the index key.<br>
<br>
`n = ast._indexself(expr, key)`<br>
An `expr:key` expression, to be used as the expression of a `ast._ call` node for member-function-calls. These are Lua's shorthand insertion of `self` as the first argument.<br>
`n.type == 'indexself'`.<br>
`n.expr =` the expression to be indexed.<br>
`n.key =` the key to index.  Must only be a Lua string, (not an `ast._ string`, but a real Lua string).<br>

Binary operations:

|node type|Lua operator|      |
|---------|------------|------|
|`_add`   |`+`         |      |
|`_sub`   |`-`         |      |
|`_mul`   |`*`         |      |
|`_div`   |`/`         |      |
|`_mod`   |`%`         |      |
|`_concat`|`..`        |      |
|`_lt`    |`<`         |      |
|`_le`    |`<=`        |      |
|`_gt`    |`>`         |      |
|`_ge`    |`>=`        |      |
|`_eq`    |`==`        |      |
|`_ne`    |`~=`        |      |
|`_and`   |`and`       |      |
|`_or`    |`or`        |      |
|`_idiv`  |`//`        | 5.3+ |
|`_band`  |`&`         | 5.3+ |
|`_bxor`  |`~`         | 5.3+ |
|`_bor`   |`\|`        | 5.3+ |
|`_shl`   |`<<`        | 5.3+ |
|`_shr`   |`>>`        | 5.3+ |

`n[1] ... n[#n] =` a table of the arguments of the operation.

Unary operations:

|node type|Lua operator|      |
|---------|------------|------|
|`_unm`   |`-`         |      |
|`_not`   |`not`       |      |
|`_len`   |`#`         |      |
|`_bnot`  |`~`         | 5.3+ |

`n[1] =` the single argument of the operation.

## more extra functions:

Some more useful functions in AST:
- `ast.copy(node)` = equivalent of `node:copy()`
- `ast.flatten(node, func, varmap)` = equivalent of `node:flatten(func, varmap)`
- `ast.refreshparents`
- `ast.traverse`
- `ast.nodeclass(type, parent, args)`
- `ast.tostringmethod` = this specifies the serialization method.  It is used to look up the serializer stored in `ast.tostringmethods`

### TODO:

- Option for parsing LuaJIT -i number suffixes.
- Speaking of LuaJIT, it has different edge case syntax for 2.0.5, 2.1.0, and whether 5.2-compat is enabled or not.  It isn't passing the `minify_tests.lua`.
- How about flags to turn off and on each feature, then a function for auto-detect flag sets based on Lua VERSION string or by running some local `load()` tests 
- Make all node allocation routed through `Parser:node` to give the node a .parser field to point back to the parser - necessary for certain AST nodes that need to tell what parser keywords are allowed. I do this where necessary but I should do it always.
	- I've also made this keyword test optional since in some rare projects (`vec-lua` for one) I am inserting AST nodes for the sake of a portable AST that I can inject as inline'd code, but without a parser, so I don't have a proper enumeration of keywords. So for now I'm making ast node `.parser` optional and the keyword test bypassed if `.parser` isn't present.  I'll probably make it a hard constraint later when I rework `vec-lua`.
	- It seems like a quick fix to just convert all `a.b`s into `a['b']`s ... but Lua for some reason doesn't support `a['b']:c()` as an equivalent of `a.b:c()` ... so converting everything from dot to brack index could break some regenerated Lua scripts.
- Node locations have token ranges.  Including comment and space tokens.  I need to use this for exact reconstruction of parse syntax.  I would like to also combine this with replacing parsed content (like with my `langfix` transpiler) and that means, instead of using ast fields which are tokens, I should use token-references as fields and allow them to be replaced ... maybe ...
- I'm very tempted to switch the AST index names to remove the preceding underscore.  Pro of keeping it: the keywords become valid Lua names.  Pro of removing it: the AST index matches the keyword that the AST node represents ...

### Dependencies:

- https://github.com/thenumbernine/lua-ext
- https://github.com/thenumbernine/lua-template

While I was at it, I added a require() replacement for parsing Lua scripts and registering callbacks,
so any other script can say `"require 'parser.load_xform':insert(function(tree) ... modify the parse tree ... end)"`
and voila, Lua preprocessor in Lua!

`minify_tests.txt` taken from the tests at https://github.com/stravant/LuaMinify

I tested this by parsing itself,
then using the parsed & reconstructed version to parse itself,
then using the parsed & reconstructed version to parse the parsed & reconstructed version,
then using the 2x parsed & reconstructed version to parse itself
