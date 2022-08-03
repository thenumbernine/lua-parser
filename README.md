[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=KYWUWS86GSFGL)

Lua parser written in Lua.
Parses to an abstract syntax tree representation.
Call tostring() on the AST to get equivalent Lua code.

AST also contains some functions like flatten() for use with optimizing / auto-inlining Lua.

See the tests folder for example usage.

### Dependencies:

- https://github.com/thenumbernine/lua-ext

while I was at it, I added a require() replacement for parsing Lua scripts and registering callbacks,
so any other script can say "require 'parser.require'.callbacks:insert(function(tree) ... modify the parse tree ... end)"
and viola, Lua preprocessor in Lua!

minify_tests.txt taken from the tests at https://github.com/stravant/LuaMinify

Known bugs: 
- still doesn't parse hex floating point precision numbers introduced in lua 5.2

I tested this by parsing itself,
then using the parsed & reconstructed version to parse itself,
then using the parsed & reconstructed version to parse the parsed & reconstructed version,
then using the 2x parsed & reconstructed version to parse itself
