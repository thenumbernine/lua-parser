#!/usr/bin/env lua
local parser = require 'parser'
local ast = require 'parser.ast'
require 'ext'

local tabs = -1	-- because everything is in one block
function tab()
	return ('\t'):rep(tabs)
end
function tabblock(t)
	tabs = tabs + 1
	local s = table(t):mapi(function(expr)
		return tab() .. tostring(expr)
	end):concat';\n'
	tabs = tabs - 1
	return s..';\n'
end

-- make lua output the default for nodes' c outputw
local names = table()
for name,nc in pairs(ast) do
	if ast.node.is(nc) then
		names:insert(name)
		nc.tostringmethods.c = nc.tostringmethods.lua
	end
end
for _,info in ipairs{
	{'concat','+'},
	{'and','&&'},
	{'or','||'},
	{'ne','!='},
} do
	local name, op = table.unpack(info)
	ast['_'..name].tostringmethods.c = function(self) 
		return table(self.args):mapi(tostring):concat(' '..op..' ')
	end
end
ast._not.tostringmethods.c = function(self)
	return '!'..self[1]
end
ast._len.tostringmethods.c = function(self)
	return self[1]..'.size()';
end
ast._assign.tostringmethods.c = function(self)
	local s = table()
	for i=1,#self.vars do
		if self.exprs[i] then
			s:insert(self.vars[i]..' = '..self.exprs[i])
		else
			s:insert(tostring(self.vars[i]))
		end
	end
	return s:concat', '
end
ast._block.tostringmethods.c = function(self)
	return tabblock(self)
end
ast._call.tostringmethods.c = function(self)
	return tostring(self.func)..'('..table.mapi(self.args, tostring):concat', '..')'
end
ast._foreq.tostringmethods.c = function(self)
	local s = 'for (auto '..self.var..' = '..self.min..'; '..self.var..' < '..self.max..'; '
	if self.step then
		s = s .. self.var..' += '..self.step
	else
		s = s .. '++'..self.var
	end
	s = s ..') {\n' .. tabblock(self) .. tab() .. '}'
	return s
end
ast._forin.tostringmethods.c = function(self)
	return 'for ('..table(self.vars):mapi(tostring):concat', '..' in '..table(self.iterexprs):mapi(tostring):concat', '..') {\n' .. tabblock(self) .. tab() .. '}'
end
ast._function.tostringmethods.c = function(self)
	if self.name then
		-- global-scope def?
		--return 'auto '..self.name..'('..table(self.args):mapi(function(arg) return 'auto '..tostring(arg) end):concat', '..') {\n' .. tabblock(self) .. tab() .. '}'
		-- local-scope named function def ...
		return 'auto '..self.name..' = []('..table(self.args):mapi(function(arg) return 'auto '..tostring(arg) end):concat', '..') {\n' .. tabblock(self) .. tab() .. '}'
	else
		-- lambdas?
		return '[]('..table(self.args):mapi(function(arg) return 'auto '..tostring(arg) end):concat', '..') {\n' .. tabblock(self) .. tab() .. '}'
	end
end
ast._if.tostringmethods.c = function(self)
	local s = 'if ('..self.cond..') {\n' .. tabblock(self) .. tab() .. '}'
	for _,ei in ipairs(self.elseifs) do
		s = s .. ei
	end
	if self.elsestmt then s = s .. self.elsestmt end
	return s
end
ast._elseif.tostringmethods.c = function(self)
	return ' else if ('..self.cond..') {\n' .. tabblock(self) .. tab() .. '}'
end
ast._else.tostringmethods.c = function(self)
	return ' else {\n' .. tabblock(self) .. tab() .. '}'
end
ast._indexself.tostringmethods.c = function(self)
	return tostring(self.expr)..'.'..tostring(self.key)
end
ast._local.tostringmethods.c = function(self)
	if self.exprs[1].type == 'function' or self.exprs[1].type == 'assign' then
		-- if exprs[1] is a multi-assign then an 'auto' needs to prefix each new declaration
		return 'auto '..self.exprs[1]
	else
		local s = table()
		for i=1,#self.exprs do
			s:insert('auto '..self.exprs[i])
		end
		return s:concat'\n'
	end
end
--print(names:sort():concat' ')

local code = file[... or 'lua_to_c.lua']
--[[
print('original:')
print(code)
--]]

local tree = parser.parse(code)
--[[
print('lua:')
print(tree)
print()
--]]

ast.tostringmethod = 'c'
--print('c:')
print(tree)
