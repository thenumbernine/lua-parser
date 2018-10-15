#!/usr/bin/env lua
--[[
batch file is a train wreck
logical operators are missing
calling internal is broke
callnig external is broke
local variables are broke
--]]
local infile = ...

local parser = require 'parser'
local ast = require 'parser.ast'
local table = require 'ext.table'
local file = require 'ext.file'
local io = require 'ext.io'
local range = require 'ext.range'

local tabs = -1	-- because everything is in one block
function tab()
	return ('\t'):rep(tabs)
end
function tabblock(t)
	tabs = tabs + 1
	local s = table.mapi(t, function(ti)
		return tab() .. tostring(ti)
	end):concat'\n'
	tabs = tabs - 1
	return s
end

local function linesep(stmts)
	return tabblock(stmts)
	--return table.mapi(stmts, tostring):concat'\n'
end



-- make lua output the default for nodes' output
local names = table()
for name,nc in pairs(ast) do
	if ast.node.is(nc) then
		names:insert(name)
		nc.tostringmethods.batch = nc.tostringmethods.lua
	end
end

function ast._concat.tostringmethods:batch()
	local a,b = table.unpack(self.args)
	return a..b
end

function ast._call.tostringmethods:batch()
	local f = self.func.name
	if f == 'select' then
		error("select() should have be replaced already")
	end
	if f == 'print' then
		return 'echo '..table.mapi(self.args, tostring):concat'\t'
	end
	return 'call :'..f..' '..table(self.args):map(tostring):concat'\t'
end

function ast._string.tostringmethods:batch()
	return self.value	-- no quotes
end

function ast._local.tostringmethods:batch()
	-- local has function or assign as children
	-- if an assign isn't the child of a local then it will need to be exported at the end of a setlocal block (endlocal & set ... )
	-- otherwise, upon local, we will need a setlocal block
	-- if we're inside a function ... ever ... then when the first setlocal is executed, it must be followed with EnableDelayedExpansion
	--  and then all subsequent local variable references will need to be surrounded by !'s instead of %'s
	return tostring(self.exprs[1])
end

function ast._foreq.tostringmethods:batch()
	-- if a for-loop arg is an expression, can it be evaluated immediately?
	-- for-loop vars must have two parenthesis prefix
	return 'for /l %%'..tostring(self.var.name)..' in ('
		..tostring(self.min)..','
		..tostring(self.step or '1')..','
		..tostring(self.max)..') do (\n'
			-- TODO call into loop body, and exit /b
			..linesep(self)
		..'\n)'
end

function ast._if.tostringmethods:batch()
	local s = 'if '..tostring(self.cond)
		..' (\n'
			..tabblock(self)
	for _,ei in ipairs(self.elseifs) do
		s = s .. tostring(ei)
	end
	if self.elsestmt then s = s .. tostring(self.elsestmt) end
	s = s .. '\n' .. ('\t'):rep(tabs) .. ')'
	return s
end
function ast._elseif.tostringmethods:batch()
	return '\n'
		..('\t'):rep(tabs)
		..') else if '..tostring(self.cond)
		..' (\n'
		..tabblock(self)
end

for _,info in ipairs{
	{'lt','lss'},
	{'le','leq'},
	{'gt','gtr'},
	{'ge','geq'},
	{'eq','equ'},
	{'ne','neq'},
} do
	local name, sym = table.unpack(info)
	ast['_'..name].tostringmethods.batch = function(self)
		return table.mapi(self.args, tostring):concat(' '..sym..' ')
	end
end

function ast._block.tostringmethods:batch()
	return linesep(self)
end
--[[
function ast._vararg.tostringmethods:batch()
if the ... is in global scope then it will work as an accessor to $*
if it's in function scope then ... the same?
in both cases, ... isn't converted directly, but instead, how it's used
end
--]]

local varmap = {}
local function getBatchVarForLuaVar(varname)
	local batchvar = varmap[varname]
	if not batchvar then
		batchvar = varname	-- TODO sanitize
		varmap[varname] = batchvar
	end
	return batchvar
end

function ast._assign.tostringmethods:batch()
	local lines = table()
	for i=1,#self.vars do
		local var = getBatchVarForLuaVar(self.vars[i])
		local expr = self.exprs[i]
		local cmd = 'set '
		if self.arith then
			cmd = cmd .. '/a '
		end
		cmd = cmd .. '"'..tostring(var.name)..'='..tostring(expr)..'"'
		lines:insert(cmd)
	end
	return lines:concat'\n'
end

local function findsource(src)
	local node = src
	while node do
		if ast._foreq.is(node) and node.var.name == src.name then return node end
		node = node.parent
	end
end

function ast._var.tostringmethods:batch()
	local name = self.name
	local varsource = findsource(self)
	if varsource and ast._foreq.is(varsource) then
		name = '%%' .. name
	else
		if type(name) == 'number' then
			name = '%' .. name
		else
			if not ast._index.is(self.parent) then
				name = '!'..name..'!'
			end
		end
	end
	return name
end

local oldtostring = ast._index.tostringmethods.lua
function ast._index.tostringmethods:batch(...)
	return '!'..oldtostring(self, ...)..'!'
end

ast._argcount = ast.nodeclass{type='argcount'}
function ast._argcount:init(...)
end
function ast._argcount.tostringmethods:batch()
	return [[
set __tmp__argcount=0
for %%x in (%*) do (
	set /a __tmp__argcount+=1
	set "__tmp__argvalue[!__tmp__argcount!]=%%x"
)]]
end

-- modulus is two %'s
-- along with the two % prefix to for-loops and % wrappers to variables, this is not confusing at all
function ast._mod.tostringmethods:batch()
	return table.mapi(self.args, tostring):concat' %% '
end

function ast._function.tostringmethods:batch()
	-- if it is a local function then move it to global scope ... this means closures are in danger of being invalid
	-- args will have to be remapped beforehand as well...
	local name = assert(self.name)	-- if it's a lambda then generate it a name
	assert(ast._var.is(name))
	name = name.name	-- TODO function .name is a var, with .name a string (should it be a _string ?)
	return '\n:'..name..'\n'
		.. tabblock(self)..'\n'
		.. 'exit /b'
end

-- TODO add to default lua
ast._goto = ast.nodeclass{type='goto'}
function ast._goto:init(label)
	self.label = label
end
function ast._goto.tostringmethods:batch()
	return 'goto '..self.label
end

ast._label = ast.nodeclass{type='label'}
function ast._label:init(name)
	self.name = name
end
function ast._label.tostringmethods:batch()
	return ':'..self.name
end

-- notice this doesn't return any values
-- I'm only using this for inserting an exit at the end of the global scope block, before all temp functions
--  I should also move all other funtcions beneath this inserted statement...
function ast._return.tostringmethods:batch()
	return 'exit /b'
end

function ast._or.tostringmethods:batch()
	error("should've replaced all of the or's")
end
function ast._and.tostringmethods:batch()
	error("should've replaced all of the and's")
end



-- returns ancestors as a table, including self
local function ancestors(node)
	local t = table()
	repeat
		t:insert(node)
		node = node.parent
	until not node
	return t
end


infile = infile or 'test1.lua'
local inbase, ext = io.getfileext(infile)
local outfile = inbase..'.bat'
print('reading '..infile)
local code = file[infile]
local tree = parser.parse(code)

-- if there is a _call to "select('#', ...)" then find the parent block and put some argcount code at the beginning
local addhere
tree:traverse(function(node)
	if ast._call.is(node)
	and node.func.name == 'select'
	and ast._vararg.is(node.args[2])
	then
		if ast._string.is(node.args[1])
		and node.args[1].value == '#'
		then
			local _, p = ancestors(node):find(nil, function(n) return ast._block.is(n) end)
			assert(p, "expected a parent to be a block")
			addhere = p
			return ast._var'__tmp__argcount'
		end
		return ast._index(
			ast._var('__tmp__argvalue'),
			node.args[1]
		)
	end
	return node
end)
ast.refreshparents(tree)
-- don't insert while traversing or else...
--if addhere then
do addhere = tree	-- always use setlocal
	table.insert(addhere, 1, ast._argcount())
end

-- if there is an assignment, then check the statement for operations
-- if it has string literals then use default assignment
-- if it has arithmetic of any sort then use /a on the assignment's "set"
-- TODO in fact, we might have to break down all expressions into their own statements
tree:traverse(function(node)
	if ast._add.is(node)
	or ast._sub.is(node)
	or ast._mul.is(node)
	or ast._div.is(node)
	or ast._mod.is(node)
	then
		local _, p = ancestors(node):find(nil, function(n) return ast._stmt.is(n) end)
		assert(p, "expected a parent to be a stmt")
		p.arith = true
	end
	return node
end)

-- TODO
-- move all functions to global namespace
-- make sure there's an exit /b before the first function

-- replace foreq bodies with function calls
-- this messes up scope of variables.
-- TODO make sure everything is passed into the function
--  then rename all variable references accordingly
-- this is bad because if and or exprs need to be unraveled as gotos in batch,
--  and goto inside of for-loops fail (unless they are calls)
-- [[
local newfuncindex = 0
local newfuncs = table()
tree:traverse(function(node)
	if ast._foreq.is(node) then
		newfuncindex = newfuncindex + 1
		local newfuncname = '__tmpfunc__'..newfuncindex
		local stmts = {table.unpack(node)}
		-- replace all instances of the for-loop variable with the function arg %1
		for _,stmt in ipairs(stmts) do
			stmt:traverse(function(node2)
				if ast._var.is(node2) then
					if node2.name == node.var.name then
						return ast._var(1)
					end
				end
				return node2
			end)
		end
		newfuncs:insert(ast._function(
			ast._var(newfuncname),
			{ast._var(1)},
			table.unpack(stmts)
		))
		-- any harm in removing nodes? or better to return new objects?
		for i=#node,1,-1 do node[i] = nil end
		node[1] = ast._call(
			ast._var(newfuncname),
			node.var
		)
	end
	return node
end)
table.insert(tree, ast._return())
for _,f in ipairs(newfuncs) do
	table.insert(tree, f)
end
ast.refreshparents(tree)
--]]


--[[
if is tricky...
since batch supports no boolean operators in if conditions,
 and batch does support gotos
 I'll treat the transpiler like an asm code generator:
if expr then stmts else stmts2 end => if expr ( stmts ) else ( stmts2 )
if a or b then stmts else stmts2 end =>
	if a goto tmp1
	if b goto tmp1
	stmts2
	goto tmp2
:tmp1
	stmts
:tmp2

if a and b then stmts else stmts2 end =>
	if not a goto tmp1
	if not b goto tmp2
	stmts2
	goto tmp2
:tmp1
	stmts
:tmp2

general case: if there's an 'if' stmt then generate 2 labels
	and do the short-circuit evaluation
	replacing all 'a and b's with 'not (not a or not b)'
--]]
local gotoindex = 0
local function nextgoto()
	gotoindex = gotoindex + 1
	return '__tmpgoto__'..gotoindex
end
tree:traverse(function(node)
	if ast._if.is(node) then
		local l1 = nextgoto()
		local l2 = nextgoto()
		
		local stmts = table()

		local function processcond(node, l1, l2, nott)
			if ast._or.is(node) then
				processcond(node.args[1], l1, l2, nott)
				processcond(node.args[2], l1, l2, nott)
			elseif ast._and.is(node) then
				processcond(node.args[1], l2, l1, not nott)
				processcond(node.args[2], l2, l1, not nott)
			else
				if nott then node = ast._not(node) end
				stmts:insert(ast._if(node, ast._goto(l1)))
			end
		end
		processcond(node.cond, l1, l2, false)
		-- if condition is false:
		-- (else stmts goes here)
		-- TODO handle elseifs like nested if's in an else
		if node.elsestmt then
			for _,n in ipairs(node.elsestmt) do
				stmts:insert(n)
			end
		end
		stmts:insert(ast._goto(l2))
		-- if condition is true:
		stmts:insert(ast._label(l1))
		for _,n in ipairs(node) do
			stmts:insert(n)
		end
		stmts:insert(ast._label(l2))
		return ast._block(stmts:unpack())
	end
	return node
end)

ast.tostringmethod = 'batch'
file[outfile] = table{
	'@echo off',
	'setlocal enabledelayedexpansion',
	tostring(tree)
}:concat'\n'
print('writing '..outfile)
