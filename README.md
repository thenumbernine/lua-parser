[![Donate via Stripe](https://img.shields.io/badge/Donate-Stripe-green.svg)](https://buy.stripe.com/00gbJZ0OdcNs9zi288)<br>
[![Donate via Bitcoin](https://img.shields.io/badge/Donate-Bitcoin-green.svg)](bitcoin:37fsp7qQKU8XoHZGRQvVzQVP8FrEJ73cSJ)<br>
[![Donate via Paypal](https://img.shields.io/badge/Donate-Paypal-green.svg)](https://buy.stripe.com/00gbJZ0OdcNs9zi288)

# Lua Parser in Lua

Parses to an abstract syntax tree representation.
Call tostring() on the AST to get equivalent Lua code.

AST also contains some functions like flatten() for use with optimizing / auto-inlining Lua.

See the tests folder for example usage.

### Dependencies:

- https://github.com/thenumbernine/lua-ext

While I was at it, I added a require() replacement for parsing Lua scripts and registering callbacks,
so any other script can say `"require 'parser.require'.callbacks:insert(function(tree) ... modify the parse tree ... end)"`
and viola, Lua preprocessor in Lua!

`minify_tests.txt` taken from the tests at https://github.com/stravant/LuaMinify

Known bugs: 
- still doesn't parse hex floating point precision numbers introduced in lua 5.2

I tested this by parsing itself,
then using the parsed & reconstructed version to parse itself,
then using the parsed & reconstructed version to parse the parsed & reconstructed version,
then using the 2x parsed & reconstructed version to parse itself
