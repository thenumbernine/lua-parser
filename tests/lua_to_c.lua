#!/usr/bin/env lua
local parser = require 'parser'
local ast = require 'parser.lua.ast'
local path = require 'ext.path'
local assert = require 'ext.assert'
local table = require 'ext.table'

local requires = table()
local cobjtype = 'Object'

local cppReservedWord = {
	'class',
}

local tabs = -1	-- because everything is in one block
function tab()
	return ('\t'):rep(tabs)
end
function tabblock(t, consume)
	tabs = tabs + 1
	for i,ti in ipairs(t) do
		consume(tab())
		consume(ti)
		if i < #t then consume'\n' end
	end
	tabs = tabs - 1
end

for k,cl in pairs(ast) do
	if ast.node:isa(cl) then
		function cl:toC()
			local s = ''
			local consume
			consume = function(x)
				if type(x) == 'number' then
					x = tostring(x)
				end
				if type(x) == 'string' then
					s = s .. x
				elseif type(x) == 'table' then
					assert.is(x, ast.node)
					assert.index(x, 'toC_recursive')
					x:toC_recursive(consume)
				else
					error('here with unknown type '..type(x))
				end
			end
			self:toC_recursive(consume)
			return s
		end
		-- weakness to this design ...i need to always keep specifying the above toC() wrapper, or I have to make a seprate member function...
		function cl:toC_recursive(consume)
			self:serialize(consume)
		end
	end
end


-- make lua output the default for nodes' c outputw
for _,info in ipairs{
	{'concat','+'},
	{'and','&&'},
	{'or','||'},
	{'ne','!='},
} do
	local name, op = table.unpack(info)
	-- hmm, can I override serialize but only for specific consume()'s ?
	-- I guess if I want to test consume == my new custom one vs otherwise call super ...
	ast['_'..name].toC_recursive = function(self, consume)
		for i,x in ipairs(self) do
			consume(x)
			if i < #self then
				consume' '
				consume(op)
				consume ' '
			end
		end
	end
end
function ast._not:toC_recursive(consume)
	consume'!'
	consume(self[1])
end
function ast._len:toC_recursive(consume)
	consume(self[1])
	consume'.size()'
end
function ast._assign:toC_recursive(consume)
	for i=1,#self.vars do
		if self.exprs[i] then
			consume(self.vars[i])
			consume' = '
			consume(self.exprs[i])
		else
			consume(self.vars[i])
		end
		if i < #self.vars then consume', ' end
	end
end
function ast._block:toC_recursive(consume)
	tabblock(self, consume)
end
function ast._call:toC_recursive(consume)
	consume(self.func)
	consume'('
	for i,x in ipairs(self.args) do
		consume(x)
		if i < #self.args then consume', ' end
	end
	consume')'
	if self.func.name == 'require' then
		if self.args[1].type == 'string' then
			-- ok here we add the require file based on our lua path
			-- does this mean we need to declare the lua path up front to lua_to_c?
			requires:insert(self.args[1].value)
		else
			consume'\n#error require arg not a string'
		end
	end
end
function ast._foreq:toC_recursive(consume)
	consume'for ('
	consume(cobjtype)
	consume' '
	consume(self.var)
	consume' = '
	consume(self.min)
	consume'; '
	consume(self.var)
	consume' < '
	consume(self.max)
	consume'; '
	if self.step then
		consume(self.var)
		consume' += '
		consume(self.step)
	else
		consume'++'
		consume(self.var)
	end
	consume') {\n'
	tabblock(self, consume)
	consume(tab())
	consume'}'
end
function ast._forin:toC_recursive(consume)
	consume'for ('
	for i,v in ipairs(self.vars) do
		consume(v)
		if i < #self.vars then consume', ' end
	end
	consume' in '
	for i,v in ipairs(self.iterexprs) do
		consume(v)
		if i < #self.iterexprs then consume', ' end
	end
	consume') {\n'
	tabblock(self, consume)
	consume(tab())
	consume'}'
end
function ast._function:toC_recursive(consume)
	if self.name then
		-- global-scope def?
		--return cobjtype..' '..self.name..'('..table(self.args):mapi(function(arg) return cobjtype..' '..apply(arg) end):concat', '..') {\n' .. tabblock(self, apply) .. tab() .. '}'
		-- local-scope named function def ...
		consume(cobjtype)
		consume' '
		consume(self.name)
		consume' = []('
		for i,arg in ipairs(self.args) do
			consume(cobjtype)
			consume' '
			consume(arg) 
			if i < #self.args then consume', ' end
		end
		consume') {\n'
		tabblock(self, consume)
		consume(tab())
		consume'}'
	else
		-- lambdas?
		consume'[]('
		for i,arg in ipairs(self.args) do
			consume(cobjtype)
			consume' '
			consume(arg) 
			if i < #self.args then consume', ' end
		end
		consume') {\n'
		tabblock(self, consume)
		consuem(tab())
		consume'}'
	end
end
function ast._if:toC_recursive(consume)
	consume'if ('
	consume(self.cond)
	consume') {\n'
	tabblock(self, consume)
	consume(tab()..'}')
	for _,ei in ipairs(self.elseifs) do
		consume(ei)
	end
	if self.elsestmt then consume(self.elsestmt) end
end
function ast._elseif:toC_recursive(consume)
	consume' else if ('
	consume(self.cond)
	consume') {\n'
	tabblock(self, consume)
	consume(tab())
	consume'}'
end
function ast._else:toC_recursive(consume)
	consume' else {\n'
	tabblock(self, consume)
	consume(tab())
	consume'}'
end
function ast._index:toC_recursive(consume)
	consume(self.expr)
	consume'['
	consume(self.key)
	consume']'
end
function ast._indexself:toC_recursive(consume)
	consume(self.expr)
	consume'.'
	consume(self.key)
end
function ast._local:toC_recursive(consume)
	if self.exprs[1].type == 'function' or self.exprs[1].type == 'assign' then
		-- if exprs[1] is a multi-assign then an 'cobjtype' needs to prefix each new declaration
		consume(cobjtype)
		consume' '
		consume(self.exprs[1])
	else
		for i=1,#self.exprs do
			consume(cobjtype)
			consume' '
			consume(self.exprs[i])
			if i < #self.exprs then consume'\n' end
		end
	end
end
function ast._vararg:toC_recursive(consume)
	consume'reserved_vararg'	-- reserved name?
end
function ast._var:toC_recursive(consume)
	if cppReservedWord[self.name] then
		consume('cppreserved_' .. self.name)
	else
		consume(self.name)
	end
end


local function addtab(s)
	return '\t'..(s:gsub('\n', '\n\t'))	-- tab
end

-- also populates requires()
local function luaFileToCpp(fn)
	assert(fn, "expected filename")
	local luacode = assert(path(fn):exists(), "failed to find "..tostring(fn))
	local luacode = assert(path(fn):read(), "failed to find "..tostring(fn))
	local tree = parser.parse(luacode)
	local cppcode = tree:toC()
	cppcode = '//file: '..fn..'\n'..cppcode
	cppcode = addtab(cppcode)
	return cppcode
end



print[[

#include "CxxAsLua/Object.h"
using namespace CxxAsLua;

// how to handle _G ...
// esp wrt locals ...
// if we use _G then that incurs overhead ...
Object _G;

// for global calls ...
Object error;
Object type;
Object require;
Object table;

int main(int argc, char** argv) {
	_G = Object::Map();
	_G["package"] = Object::Map();
	_G["package"]["loaded"] = Object::Map();

	error = _G["error"] = [](Object x) -> Object {
		throw std::runtime_error((std::string)x);
	};

	//hmm, 'type' might be used as a global later, so i might have to remove the 'using namespace' and instead replace all Object's with Object::Object's
	::type = _G["type"] = [](Object x) -> Object {
		if (x.is_nil()) {
			return "nil";
		} else if (x.is_string()) {
			return "string";
		} else if (x.is_table()) {
			return "table";
		} else if (x.is_boolean()) {
			return "boolean";
		} else if (x.is_function()) {
			return "function";
		} else if (x.is_nil()) {
			return "nil";
		}
		//or use getTypeIndex()
		// or better yet, rewrite our x.details to be a std::variant,
		// and map the variant index to a type,
		// then just store type info in that extra arra
	};

	table = _G["table"] = Object::Map();

	table["concat"] = [](VarArg arg) -> Object {
		if (!arg[1].is_table()) error("expected a table");
	//TODO FINISHME
		// list, sep, i
		std::ostringstream s;
		std::string sep = "";
		for (const Object& o : arg.objects) {
			std::cout << sep;
			std::cout << o;
			sep = "\t";
		}
		std::cout << std::endl;
	};

	require = _G["require"] = [&](std::string const & s) -> Object {
		Object x = _G["package"]["loaded"][s];
		if (x != nil) return x;

		x = _G["cppmodules"][s];
		if (x != nil) {
			x = x();
			_G["package"]["loaded"][s] = x;
			return x;
		}

		return error(Object("idk how to load ") + s);
	};

	_G["cppmodules"] = Object::Map();
]]

local cppcode = luaFileToCpp(... or 'lua_to_c_test.lua')

for _,req in ipairs(requires) do
	-- ok here's where lua_to_c has to assume the same LUA_PATH as the c++ runtime
	print('//require: '..req)
	local fn = package.searchpath(req, package.path)
	if not fn then
		print("// package.searchpath couldn't find file")
	else
		print([[
	_G["cppmodules"]["]]..req..[["] = []() -> Object {
]])
		print(addtab(luaFileToCpp(fn)))

		print[[
	};
]]
	end
end

print(cppcode)

print[[
}
]]
