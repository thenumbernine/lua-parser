package = "parser"
version = "dev-1"
source = {
	url = "git+https://github.com/thenumbernine/lua-parser"
}
description = {
	summary = "Lua Parser in Lua",
	detailed = "Lua Parser in Lua",
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
		["parser.load_xform"] = "load_xform.lua",
		["parser.base.ast"] = "base/ast.lua",
		["parser.base.datareader"] = "base/datareader.lua",
		["parser.base.parser"] = "base/parser.lua",
		["parser.base.tokenizer"] = "base/tokenizer.lua",
		["parser.grammar.parser"] = "grammar/parser.lua",
		["parser.grammar.tokenizer"] = "grammar/tokenizer.lua",
		["parser.lua.ast"] = "lua/ast.lua",
		["parser.lua.parser"] = "lua/parser.lua",
		["parser.lua.tokenizer"] = "lua/tokenizer.lua",
	},
	copy_directories = {
		"tests"
	}
}
