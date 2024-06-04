package = "parser"
version = "dev-1"
source = {
   url = "git+https://github.com/thenumbernine/lua-parser"
}
description = {
   summary = "Lua Parser written in Lua.",
   detailed = [[
Lua parser written in Lua.
Parses to an abstract syntax tree representation.
Call tostring() on the AST to get equivalent Lua code.

AST also contains some functions like flatten() for use with optimizing / auto-inlining Lua.

See the tests folder for example usage.

while I was at it, I added a require() replacement for parsing Lua scripts and registering callbacks,
so any other script can say "require 'parser.require'.callbacks:insert(function(tree) ... modify the parse tree ... end)"
and viola, Lua preprocessor in Lua!
]],
   homepage = "https://github.com/thenumbernine/lua-parser",
   license = "MIT"
}
dependencies = {
   "lua => 5.1"
}
build = {
   type = "builtin",
   modules = {
      ["parser"] = "parser.lua",
      ['parser.base.ast'] = "base/ast.lua",
      ['parser.base.datareader'] = "base/datareader.lua",
      ['parser.base.parser'] = "base/parser.lua",
      ['parser.base.tokenizer'] = "base/tokenizer.lua"
      ["parser.load_xform"] = "load_xform.lua",
      ['parser.lua.ast'] = "lua/ast.lua",
      ['parser.lua.parser'] = "lua/parser.lua",
      ['parser.lua.tokenizer'] = "lua/tokenizer.lua"
      ["parser.tests.flatten"] = "tests/flatten.lua",
      ["parser.tests.lua_to_c"] = "tests/lua_to_c.lua",
      ["parser.tests.lua_to_c_test"] = "tests/lua_to_c_test.lua",
      ["parser.tests.minify_tests"] = "tests/minify_tests.lua",
      ["parser.tests.parse"] = "tests/parse.lua"
   },
   copy_directories = {
      "tests"
   }
}
