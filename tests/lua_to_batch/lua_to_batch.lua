#!/usr/bin/env lua
--[[
batch file is a train wreck
logical operators are missing
calling internal functions is broke
calling external scripts is broke
local variables are broke
so I'm fixing it

features:
- globals are transpiled into environment variables
- locals are transpiled into ... setlocal variables, which operate like Lua globals, that are not environment variables
- global scope functions work
- if conditions work.  'and's, 'or's, 'not's, elseifs and else
- arithmetic and concatenation operations work with assignment
- command-line arguments, and command-line argument counting, works

still todo:
- inline and lambda functions need to be moved to global scope
-  and keep track of what vars are in scope when a function is moved 
-  local functions won't respect the local scope
- scope of local variables isn't respected
- breaking apart boolean expressions of statements into preceding temporary statements, and using them in either conditions or assignment
-  boolean operations are based on short-circuiting, so assigning to a boolean operation still needs to be implemented
-  in fact, merging the assignment and if-conditions, it might be best to use temp registers for bool expressions (and all expressions, for that matter)
- heterogeneous tables -- non-numbers as keys
- assigning functions to variables
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


local varindex = 0
local function nextvar()
	varindex = varindex + 1
	return '__tmpvar__'..varindex
end



local gotoindex = 0
local function nextgoto()
	gotoindex = gotoindex + 1
	return '__tmpgoto__'..gotoindex
end

local funcindex = 0
local function nextfunc()
	funcindex = funcindex + 1
	return '__tmpfunc__'..funcindex
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
	return tostring(a)..tostring(b)
end

function ast._call.tostringmethods:batch()
	local funcname
	if ast._var.is(self.func) then
		funcname = self.func.name
		if funcname == 'select' then
			error("select() should have be replaced already")
		end
		if funcname == 'print' then
			return 'echo '..table.mapi(self.args, tostring):concat'\t'
		end
	elseif ast._index.is(self.func) then
		if ast._var.is(self.func.expr)
		and self.func.expr.name == 'os'
		and ast._string.is(self.func.key)
		and self.func.key.value == 'exit'
		then
			-- os.exit(1) ... returns an error ... but shouldn't kill calling batch files, right?
			return 'goto :eof'
		end
		funcname = tostring(self.func)
	end
	return 'call :'..funcname..table.mapi(self.args, function(arg) return ' '..tostring(arg) end):concat()
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
			..tabblock(self)
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
	s = s .. '\n' .. tab() .. ')'
	return s
end
function ast._elseif.tostringmethods:batch()
	return '\n'
		..tab()
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
	return tabblock(self)
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
	local l = nextgoto()
	-- for safety's sake I'll add gotos around the function 
	-- so global scope code keeps executing 
	return '\n'
		..'goto '..l..'\n'
		..':'..name..'\n'
		.. tabblock(self)..'\n'
		.. 'exit /b\n'
		.. ':'..l
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
	return '\n:'..self.name
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

ast._endlocal = ast.nodeclass{type='endlocal'}
function ast._endlocal:init(globals)
	self.globals = table(globals)
end
function ast._endlocal.tostringmethods:batch()
	-- TODO & set VARIABLE=value at the end
	-- for all the global variables we assigned to 
	local t = tab()
	local s = '\n'..t..'endlocal'
	t = t .. '\t'
	for _,name in ipairs(self.globals) do
		s = s .. ' ^\n'
			..t..'& set "'..name..'=%'..name..'%"'
	end
	return s
end

infile = infile or 'test1.lua'
local inbase, ext = io.getfileext(infile)
local outfile = inbase..'.bat'
print('reading '..infile)
local code = file[infile]
local tree = parser.parse(code)

-- if there is a _call to "select('#', ...)" then find the parent block and put some argcount code at the beginning
local found
tree:traverse(function(node)
	if ast._call.is(node)
	and node.func.name == 'select'
	and ast._vararg.is(node.args[2])
	then
		if ast._string.is(node.args[1])
		and node.args[1].value == '#'
		then
			local _, p = node:ancestors():find(nil, ast._block.is)
			assert(p, "expected a parent to be a block")
			found = p
			return ast._var'__tmp__argcount'
		end
		return ast._index(
			ast._var'__tmp__argvalue',
			node.args[1]
		)
	end
	return node
end)
-- don't insert while traversing or else...
--if found then
table.insert(tree, 1, ast._argcount())
--end
ast.refreshparents(tree)


-- break down all composite expressions into temp stmts
local assignsForStmts = {}
local function opis(node)
	return (ast._op.is(node) or ast._par.is(node))
	and not (
	-- not handling boolean right now
		ast._eq.is(node)
		or ast._ne.is(node)
		or ast._ge.is(node)
		or ast._gt.is(node)
		or ast._le.is(node)
		or ast._lt.is(node)
		or ast._and.is(node)
		or ast._or.is(node)
		or ast._not.is(node)
	)
end
tree:traverse(nil, function(node)
	if opis(node) then
		--  then move node to before the containing statement 
		local _, p = node:ancestors():find(nil, ast._stmt.is)
		if ast._local.is(p.parent) then p = p.parent end

		local function replace(arg)
			local varname = nextvar()
			local var = ast._var(varname)
			local assign = ast._local{ast._assign({var}, {arg})}
			assignsForStmts[p] = assignsForStmts[p] or table()
			assignsForStmts[p]:insert(assign)
			return var
		end

		if ast._op.is(node) then
			for i=1,#node.args do
				if opis(node.args[i]) then
					node.args[i] = replace(node.args[i])
				end
			end
		elseif ast._par.is(node) then
			--node.expr = replace(node.expr)
			node = node.expr
		end
	end
	return node
end)
tree:traverse(function(node)
	local assigns = assignsForStmts[node]
	if assigns then
		assigns:insert(node)
		return ast._block(assigns:unpack())
	end
	return node
end)

-- now replace 


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
		local _, p = node:ancestors():find(nil, ast._stmt.is)
		assert(p, "expected a parent to be a stmt")
		p.arith = true
	end
	return node
end)


-- TODO
-- move all functions to global namespace


-- rename all function arguments to %'s
tree:traverse(function(node)
	if ast._function.is(node) then
		-- TODO make sure the function is global scope
		local args = table.mapi(node.args, function(arg) 
			assert(type(arg.name) == 'string')
			return arg.name 
		end)
		return node:traverse(function(node2)
			if ast._var.is(node2) then
				assert(type(node2.name) == 'string')
				local i = table.find(args, node2.name)
				if i then
					return ast._var(i)
				end
			end
			return node2
		end)
	end
	return node
end)


-- replace foreq bodies with function calls
-- this messes up scope of variables.
-- TODO make sure everything is passed into the function
--  then rename all variable references accordingly
-- this is bad because if and or exprs need to be unraveled as gotos in batch,
--  and goto inside of for-loops fail (unless they are calls)
local newfuncs = table()
tree:traverse(function(node)
	if ast._foreq.is(node) then
		local newfuncname = nextfunc()
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
for _,f in ipairs(newfuncs) do
	table.insert(tree, f)
end
ast.refreshparents(tree)


--[[
if is tricky...
since batch supports no boolean operators in if conditions,
 and batch does support gotos
 I'll treat the transpiler like an asm code generator:
if there's an 'if' stmt then generate 2 labels
	and do the short-circuit evaluation
	replacing all 'a and b's with 'not (not a or not b)'
--]]
tree:traverse(function(node)
	local function handle_if(
		cond,
		ifstmts,
		elseifs,
		elsestmt
	)
		local l1 = nextgoto()
		local l2 = nextgoto()
		
		local stmts = table()

		local function processcond(boolexpr, l1, l2, nott)
			if ast._or.is(boolexpr) then
				processcond(boolexpr.args[1], l1, l2, nott)
				processcond(boolexpr.args[2], l1, l2, nott)
			elseif ast._and.is(boolexpr) then
				processcond(boolexpr.args[1], l2, l1, not nott)
				processcond(boolexpr.args[2], l2, l1, not nott)
			elseif ast._not.is(boolexpr) then
				processcond(boolexpr.args[1], l1, l2, not nott)
			else
				if nott then boolexpr = ast._not(boolexpr) end
				stmts:insert(ast._if(boolexpr, ast._goto(l1)))
			end
		end
		processcond(cond, l1, l2, false)
		-- if condition is false:
		-- (else stmts goes here)
		-- TODO handle elseifs like nested if's in an else
		if #elseifs > 0 or elsestmt then
			-- handle else alone
			if #elseifs == 0 then
				for _,n in ipairs(elsestmt) do
					stmts:insert(n)
				end
			else
			-- handle else and then if's
				elseifs = table(elseifs)
				local firstelseif = elseifs:remove(1)
				stmts:insert(handle_if(
					firstelseif.cond,
					firstelseif,
					elseifs,
					elsestmt
				))
			end
		end
		stmts:insert(ast._goto(l2))
		-- if condition is true:
		stmts:insert(ast._label(l1))
		for _,n in ipairs(ifstmts) do
			stmts:insert(n)
		end
		stmts:insert(ast._label(l2))
		return ast._block(stmts:unpack())
	end
	
	if ast._if.is(node) then
		return handle_if(
			node.cond,		-- cond
			node,			-- stmts
			node.elseifs,	-- elseifs
			node.elsestmt	-- else
		)
	end
	return node
end)

-- keep track of all global assignments
-- notice I'm not keeping track of scope
local locals = {}
local globals = {}
tree:traverse(function(node)
	if ast._assign.is(node) then
		local names = table.mapi(node.vars, function(var)
			assert(type(var.name) == 'string')
			return var.name
		end)
		if ast._local.is(node.parent) then
			for _,name in ipairs(names) do
				locals[name] = true
			end
		else
			for _,name in ipairs(names) do
				globals[name] = true
			end
		end
	end
	return node
end)
for name,_ in ipairs(globals) do
	if locals[name] then
		print("Warning, you used the variable '..name..' as a local and a global.  I'm not keeping track of scope.")
	end
end

table.insert(tree, ast._endlocal(table.keys(globals)))

ast.tostringmethod = 'batch'
file[outfile] = table{
	'@echo off',
	'setlocal enabledelayedexpansion',
	tostring(tree)
}:concat'\n'
	-- until I can solve my tab problems:
	:gsub('\t+', '\t')
print('writing '..outfile)
