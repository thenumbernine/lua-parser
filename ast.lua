local table = require 'ext.table'
local class = require 'ext.class'
local string = require 'ext.class'
local tolua = require 'ext.tolua'

local ast = {}

local node = class()

node.tostringmethods = {}	-- each class gets a unique one

-- returns ancestors as a table, including self
function node:ancestors()
	local n = self
	local t = table()
	repeat
		t:insert(n)
		n = n.parent
	until not n
	return t
end

ast.node = node

local fields = {
	{'cond', 'one'},
	{'var', 'one'},
	{'min', 'one'},
	{'max', 'one'},
	{'step', 'one'},
	{'vars', 'many'},
	{'func', 'one'},		-- should this be a _function, or a string depicting a function?
	{'args', 'many'},
	{'arg', 'one'},
	{'key', 'one'},
	{'expr', 'one'},
	{'exprs', 'many'},
	{'stmt', 'one'},
	{'elseifs', 'many'},
	{'elsestmt', 'many'},
	{'name', 'field'},
	{'index', 'field'},
	{'value', 'field'},
}

function node.exec(n, ...)
	return assert(load(tostring(n), ...))
end
ast.exec = node.exec

--[[
I need to fix this up better to handle short-circuiting, replacing, removing, etc...
parentFirstCallback is the parent-first traversal method
childFirstCallback is the child-first traversal
return what value of the callbacks you want
returning a new node at the parent callback will not traverse its subsequent new children added to the tree
--]]
local function traverseRecurse(
	node,
	parentFirstCallback,
	childFirstCallback,
	parentNode
)
	if not ast.node:isa(node) then return node end
	if parentFirstCallback then
		local ret = parentFirstCallback(node, parentNode)
		if ret ~= node then
			return ret
		end
	end
	if type(node) == 'table' then
		-- treat the object itself like an array of many
		for i=1,#node do
			node[i] = traverseRecurse(node[i], parentFirstCallback, childFirstCallback, node)
		end
		for _,field in ipairs(fields) do
			local name = field[1]
			local howmuch = field[2]
			if node[name] then
				if howmuch == 'one' then
					node[name] = traverseRecurse(node[name], parentFirstCallback, childFirstCallback, node)
				elseif howmuch == 'many' then
					local value = node[name]
					for i=#value,1,-1 do
						value[i] = traverseRecurse(value[i], parentFirstCallback, childFirstCallback, node)
					end
				elseif howmuch == 'field' then
				else
					error("unknown howmuch "..howmuch)
				end
			end
		end
	end
	if childFirstCallback then
		node = childFirstCallback(node, parentNode)
	end
	return node
end

function ast.refreshparents(node)
	traverseRecurse(node, function(node, parent)
		node.parent = parent
		return node
	end)
end

local function traverse(node, ...)
	local newnode = traverseRecurse(node, ...)
	ast.refreshparents(newnode)
	return newnode
end

node.traverse = traverse
ast.traverse = traverse

function node.copy(n)
	local newn = {}
	setmetatable(newn, getmetatable(n))
	for i=1,#n do
		newn[i] = node.copy(n[i])
	end
	for _,field in ipairs(fields) do
		local name = field[1]
		local howmuch = field[2]
		local value = n[name]
		if value then
			if howmuch == 'one' then
				if type(value) == 'table' then
					newn[name] = node.copy(value)
				else
					newn[name] = value
				end
			elseif howmuch == 'many' then
				local newmany = {}
				for k,v in ipairs(value) do
					if type(v) == 'table' then
						newmany[k] = node.copy(v)
					else
						newmany[k] = v
					end
				end
				newn[name] = newmany
			elseif howmuch == 'field' then
				newn[name] = value
			else
				error("unknown howmuch "..howmuch)
			end
		end
	end
	return newn
end
ast.copy = node.copy

--[[
flatten a function:
for all its calls, insert them as statements inside the function
this is only possible if the called functions are of a specific form...
varmap is the mapping from function names to _function objects to inline in the _call's place


if the nested function ends with a return ...
... then insert its declarations (for var remapping) into a statement just before the one with this call
... and wrap our return contents in parenthesis ... or make general use of ()'s everywhere (for resolution order)

f stmt
f stmt
f stmt
return something(g(...), h(...))

becomes

f stmt
f stmt
f stmt
local g ret
g stmt
g stmt
g stmt
g ret = previous return value of h
local h ret
h stmt
h stmt
h stmt
h ret = previous return value of h
return something(g ret, h ret)

--]]
function node.flatten(f, varmap)
	f = node.copy(f)
	traverseRecurse(f, function(n)
		if type(n) == 'table'
		and ast._call:isa(n)
		then
			local funcname = tostring(n.func)	-- in case it's a var ... ?
			assert(funcname, "can't flatten a function with anonymous calls")
			local f = varmap[funcname]
			if f
			and #f == 1
			and ast._return:isa(f[1])
			then
				local retexprs = {}
				for i,e in ipairs(f[1].exprs) do
					retexprs[i] = node.copy(e)
					traverseRecurse(retexprs[i], function(v)
						if type(v) == 'table'
						and ast._arg:isa(v)
						then
							return node.copy(n.args[i])
						end
					end)
					retexprs[i] = ast._par(retexprs[i])
				end
				return ast._block(table.unpack(retexprs))	-- TODO exprlist, and redo assign to be based on vars and exprs
			end
		end
		return n
	end)
	return f
end
ast.flatten = node.flatten

-- TODO something more flexible than this
ast.spaceseparator = '\n'

local function spacesep(stmts)
	return table.mapi(stmts, tostring):concat(ast.spaceseparator)
end

local function commasep(exprs)
	return table.mapi(exprs, tostring):concat','
end


--[[
make __tostring modular
give each node class a lookup table for whatever the current 'tostringmethod' is
then remove all __tostring methods and replace the base class __tostring with something to call into the lookup table
--]]
ast.tostringmethod = 'lua'
local nodeToString = function(self)
	local f = self.tostringmethods[ast.tostringmethod]
	if not f then error("failed to find tostringmethod for method "..tostring(ast.tostringmethod).." for node of type "..tostring(self.type)) end
	return f(self)
end

local allclasses = table{node}
ast.allclasses = allclasses
local function nodeclass(contents, parent)
	local newclass = class(parent or ast.node)
	newclass.tostringmethods = {}
	for k,v in pairs(contents) do
		newclass[k] = v
	end

	-- TODO put in root-most ast class
	newclass.__tostring = nodeToString

	newclass.__concat = string.defaultConcat

	allclasses:insert(newclass)
	return newclass
end
ast.nodeclass = nodeclass

-- generic global stmt collection
ast._block = nodeclass{type = 'block'}
function ast._block:init(...)
	for i,stmt in ipairs{...} do
		self[i] = stmt
	end
end
function ast._block.tostringmethods:lua()
	return spacesep(self)
end

--statements

local _stmt = class(node)
ast._stmt = _stmt

ast._assign = nodeclass({type = 'assign'}, _stmt)
function ast._assign:init(vars, exprs)
	self.vars = table(vars)
	self.exprs = table(exprs)
end
function ast._assign.tostringmethods:lua()
	return commasep(self.vars)..'='..commasep(self.exprs)
end

-- should we impose construction constraints _do(_block(...))
-- or should we infer?  _do(...) = {type = 'do', block = {type = 'block, ...}}
-- or should we do neither?  _do(...) = {type = 'do', ...}
-- neither for now
ast._do = nodeclass({type = 'do'}, _stmt)
function ast._do:init(...)
	for i,stmt in ipairs{...} do
		self[i] = stmt
	end
end
function ast._do.tostringmethods:lua()
	return 'do '..spacesep(self)..' end'
end

ast._while = nodeclass({type = 'while'}, _stmt)
function ast._while:init(cond, ...)
	self.cond = cond
	for i,stmt in ipairs{...} do
		self[i] = stmt
	end
end
function ast._while.tostringmethods:lua()
	return 'while '..tostring(self.cond)..' do '..spacesep(self)..' end'
end

ast._repeat = nodeclass({type = 'repeat'}, _stmt)
function ast._repeat:init(cond, ...)
	self.cond = cond
	for i,stmt in ipairs{...} do
		self[i] = stmt
	end
end
function ast._repeat.tostringmethods:lua()
	return 'repeat '..spacesep(self)..' until '..tostring(self.cond)
end

--[[
_if(_eq(a,b),
	_assign({a},{2}),
	_elseif(...),
	_elseif(...),
	_else(...))
--]]
ast._if = nodeclass({type = 'if'}, _stmt)
function ast._if:init(cond,...)
	local elseifs = table()
	local elsestmt, laststmt
	for _,stmt in ipairs{...} do
		if ast._elseif:isa(stmt) then
			elseifs:insert(stmt)
		elseif ast._else:isa(stmt) then
			assert(not elsestmt)
			elsestmt = stmt -- and remove
		else
			if laststmt then
				assert(laststmt.type ~= 'elseif' and laststmt.type ~= 'else', "got a bad stmt in an if after an else: "..laststmt.type)
			end
			table.insert(self, stmt)
		end
		laststmt = stmt
	end
	self.cond = cond
	self.elseifs = elseifs
	self.elsestmt = elsestmt
end
function ast._if.tostringmethods:lua()
	local s = 'if '..tostring(self.cond)..' then '..spacesep(self)
	for _,ei in ipairs(self.elseifs) do
		s = s .. tostring(ei)
	end
	if self.elsestmt then s = s .. tostring(self.elsestmt) end
	s = s .. ' end'
	return s
end

-- aux for _if
ast._elseif = nodeclass({type = 'elseif'}, _stmt)
function ast._elseif:init(cond,...)
	self.cond = cond
	for i,stmt in ipairs{...} do
		self[i] = stmt
	end
end
function ast._elseif.tostringmethods:lua()
	return ' elseif '..tostring(self.cond)..' then '..spacesep(self)
end

-- aux for _if
ast._else = nodeclass({type = 'else'}, _stmt)
function ast._else:init(...)
	for i,stmt in ipairs{...} do
		self[i] = stmt
	end
end
function ast._else.tostringmethods:lua()
	return ' else '..spacesep(self)
end

ast._foreq = nodeclass({type = 'foreq'}, _stmt)
-- step is optional
function ast._foreq:init(var,min,max,step,...)
	self.var = var
	self.min = min
	self.max = max
	self.step = step
	for i,stmt in ipairs{...} do
		self[i] = stmt
	end
end
function ast._foreq.tostringmethods:lua()
	local s = 'for '..tostring(self.var)..' = '..tostring(self.min)..','..tostring(self.max)
	if self.step then s = s..','..tostring(self.step) end
	s = s .. ' do '..spacesep(self)..' end'
	return s
end

ast._forin = nodeclass({type = 'forin'}, _stmt)
function ast._forin:init(vars,iterexprs,...)
	self.vars = vars
	self.iterexprs = iterexprs
	for i,stmt in ipairs{...} do
		self[i] = stmt
	end
end
function ast._forin.tostringmethods:lua()
	return 'for '..commasep(self.vars)..' in '..commasep(self.iterexprs)..' do '..spacesep(self)..' end'
end

ast._function = nodeclass({type = 'function'}, _stmt)
-- name is optional
function ast._function:init(name, args, ...)
	-- prep args...
	for i=1,#args do
		args[i].index = i
		args[i].param = true
	end
	self.name = name
	self.args = args
	for i,stmt in ipairs{...} do
		self[i] = stmt
	end
end
function ast._function.tostringmethods:lua()
	local s = 'function '
	if self.name then s = s .. tostring(self.name) end
	s = s .. '('
		.. commasep(table.mapi(self.args, tostring))
		.. ') ' .. spacesep(self) .. ' end'
	return s
end

-- aux for _function
ast._arg = nodeclass{type = 'arg'}
function ast._arg:init(index)
	self.index = index
end
-- params need to know what function they're in
-- so they can reference the function's arg names
function ast._arg.tostringmethods:lua()
	return 'arg'..self.index
end

-- _local can be an assignment of multi vars to muli exprs
--  or can optionally be a declaration of multi vars with no statements
-- so it will take the form of assignments
-- but it can also be a single function declaration with no equals symbol ...
-- the parser has to accept functions and variables as separate conditions
--  I'm tempted to make them separate symbols here too ...
-- exprs is a table containing: 1) a single function 2) a single assign statement 3) a list of variables
ast._local = nodeclass({type = 'local'}, _stmt)
function ast._local:init(exprs)
	if ast._function:isa(exprs[1]) or ast._assign:isa(exprs[1]) then
		assert(#exprs == 1, "local functions or local assignments must be the only child")
	end
	self.exprs = table(assert(exprs))
end
function ast._local.tostringmethods:lua()
	if ast._function:isa(self.exprs[1]) or ast._assign:isa(self.exprs[1]) then
		return 'local '..tostring(self.exprs[1])
	else
		return 'local '..commasep(self.exprs)
	end
end

-- control

ast._return = nodeclass({type = 'return'}, _stmt)
function ast._return:init(...)
	self.exprs = {...}
end
function ast._return.tostringmethods:lua()
	return 'return '..commasep(self.exprs)
end

ast._break = nodeclass({type = 'break'}, _stmt)
function ast._break.tostringmethods:lua() return 'break' end

ast._call = nodeclass{type = 'call'}
function ast._call:init(func, ...)
	self.func = func
	self.args = {...}
end
function ast._call.tostringmethods:lua()
	if #self.args == 1
	and (ast._table:isa(self.args[1])
		or ast._string:isa(self.args[1])
	) then
		return tostring(self.func)..tostring(self.args[1])
	end
	return tostring(self.func)..'('..commasep(self.args)..')'
end

-- please don't change these
ast._nil = nodeclass{
	type = 'nil',
	const = true,
	tostringmethods = {lua = function() return 'nil' end},
}
ast._true = nodeclass{
	type = 'boolean',
	const = true,
	value = true,
	tostringmethods = {lua = function() return 'true' end},
}
ast._false = nodeclass{
	type = 'boolean',
	const = true,
	value = false,
	tostringmethods = {lua = function() return 'false' end},
}

ast._number = nodeclass{type = 'number'}
function ast._number:init(value) self.value = value end
function ast._number.tostringmethods:lua() return self.value end

ast._string = nodeclass{type = 'string'}
function ast._string:init(value) self.value = value end
function ast._string.tostringmethods:lua()
	-- use ext.tolua's string serializer
	return tolua(self.value)
end

ast._vararg = nodeclass{type = 'vararg'}
function ast._vararg.tostringmethods:lua() return '...' end

ast._table = nodeclass{type = 'table'}	-- single-element assigns
function ast._table:init(args)
	self.args = table(assert(args))
end
function ast._table.tostringmethods:lua()
	return '{'..self.args:mapi(function(arg)
		-- if it's an assign then wrap the vars[1] with []'s
		if ast._assign:isa(arg) then
			assert(#arg.vars == 1)
			assert(#arg.exprs == 1)
			return '[' .. tostring(arg.vars[1]) .. '] = '..tostring(arg.exprs[1])
		end
		return tostring(arg)
	end):concat(',')..'}'
end

ast._var = nodeclass{type = 'var'}	-- variable, lhs of ast._assign's, similar to _arg
function ast._var:init(name, attrib)
	self.name = name
	self.attrib = attrib
end
function ast._var.tostringmethods:lua()
	local s = self.name
	if self.attrib then s = s .. '<'..self.attrib..'>' end
	return s
end

ast._par = nodeclass{type = 'parenthesis'}
function ast._par:init(expr)
	self.expr = expr
end
function ast._par.tostringmethods:lua()
	return '('..tostring(self.expr)..')'
end

local function isLuaName(s)
	return s:match'^[_%a][_%w]*$'
end

ast._index = nodeclass{type = 'index'}
function ast._index:init(expr,key)
	self.expr = expr
	-- helper add wrappers to some types:
	if type(key) == 'string' then
		key = ast._string(key)
	elseif type(key) == 'number' then
		key = ast._number(key)
	end
	self.key = key
end
function ast._index.tostringmethods:lua()
-- TODO - if self.key is a string and has no funny chars the use a .$key instead of [$key]
	if ast._string:isa(self.key)
	and isLuaName(self.key.value)
	then
		return tostring(self.expr)..'.'..self.key.value
	end
	return tostring(self.expr)..'['..tostring(self.key)..']'
end

-- this isn't the () call itself, this is just the : dereference
-- a:b(c) is _call(_indexself(_var'a', _var'b'), _var'c')
-- technically this is a string lookup, however it is only valid as a lua name, so I'm just passing the Lua string itself
ast._indexself = nodeclass{type = 'indexself'}
function ast._indexself:init(expr,key)
	self.expr = assert(expr)
	assert(isLuaName(key))
	self.key = assert(key)
end
function ast._indexself.tostringmethods:lua()
	return tostring(self.expr)..':'..tostring(self.key)
end

ast._op = class(node)

for _,info in ipairs{
	{'add','+'},
	{'sub','-'},
	{'mul','*'},
	{'div','/'},
	{'pow','^'},
	{'mod','%'},
	{'concat','..'},
	{'lt','<'},
	{'le','<='},
	{'gt','>'},
	{'ge','>='},
	{'eq','=='},
	{'ne','~='},
	{'and','and'},
	{'or','or'},
	{'//', 'idiv'},	-- 5.3+
	{'&', 'band'},	-- 5.3+
	{'~', 'bxor'},	-- 5.3+
	{'|', 'bor'},	-- 5.3+
	{'<<', 'shl'},	-- 5.3+
	{'>>', 'shr'},	-- 5.3+
} do
	local name = info[1]
	local op = info[2]
	local cl = nodeclass({type = info[1], op = op}, ast._op)
	ast['_'..name] = cl
	function cl:init(...)
		self.args = {...}
	end
	function cl.tostringmethods:lua()
		return table.mapi(self.args, tostring):concat(' '..self.op..' ') -- spaces required for 'and' and 'or'
	end
end

for _,info in ipairs{
	{'unm','-'},
	{'not','not'},
	{'len','#'},
	{'bnot','~'},		-- 5.3+
} do
	local name = info[1]
	local op = info[2]
	local cl = nodeclass{type = info[1], op = op}
	ast['_'..name] = cl
	function cl:init(arg)
		self.arg = arg
	end
	function cl.tostringmethods:lua()
		return ' '..self.op..' '..tostring(self.arg)	-- spaces required for 'not'
	end
end

ast._goto = nodeclass({type = 'goto'}, _stmt)
function ast._goto:init(name)
	self.name = name
end
function ast._goto.tostringmethods:lua()
	return 'goto '..self.name
end

ast._label = nodeclass({type = 'label'}, _stmt)
function ast._label:init(name)
	self.name = name
end
function ast._label.tostringmethods:lua()
	return '::'..self.name..'::'
end

--[[
function building and eventually reconstructing and inlining

TODO full-on AST:

f = _function(
		'vec3.add',
		{'a','b'},
		-- rest is stmts
		_return(
			_call(_var('vec3'),
				add(index(param(1),1), index(param(2),1)),
				add(index(param(1),2), index(param(2),2)),
				add(index(param(1),3), index(param(2),3)),
			)
		)
	}

	-- becomes --

	"function vec3.add(a,b)
		return vec3(
			a[1] + b[1],
			a[2] + b[2],
			a[3] + b[3])
	end"

	-- convert + to -
	traverseRecurse(f, function(n)
		if ast._add:isa(n) then n.type = 'sub' end
	end

	-- then we do a tree-descent with replace rule:
	traverseRecurse(f, function(n)
		if ast._param:isa(n) and n.param == 2 then
			n.param = 1
		end
	end)
	-- and that's how dot() becomes lenSq()

	-- inline a function
	traverseRecurse(f, function(n)
		if ast._call:isa(n) then
			local inline = clonetree(n.func.body)
			-- first scope/local declare args
			local vars = {}
			for i=#n.args,1,-1 do
				local arg = n.args[i]
				local v = var()
				table.insert(vars, v)
				table.insert(inline, 1, localassign(v, arg))			-- names are all anonymous, just specify the var and assignment value.	 local() for declaring vars only, assign() for assigning them only, and localassign() for both?  or should i nest the two cmds?
			end
			-- then convert the body
			traverseRecurse(inline, function(v)
				if ast._param:isa(v) then
					return vars[v.param]
				end
			end)
			return inline
		end
	end)

	--[=[
	make a list ... check it twice ...

	block		- statement block
		[1]...[n] - array of stmt objects
		.tostring = table.concat(block, ' ')

statements:
	assign		- assignment operation
		.vars	- array of var objects
		.exprs	- array of expressions
		.tostring = table.concat(map(vars, 'name'), ',')..'='..table.concat(exprs, ',')

	do			- do / end block wrapper
		[1]...[n] - array of stmt objects
		.tostring = 'do '..table.concat(do, ' ')..' end'

	while
		.cond	- condition expression
		[1]...[n] - statements to execute
		.tostring = 'while '..cond..' do '..table.concat(tostring, ' ')..' end'

	repeat
		.cond	- condition expression
		[1]...[n] - statements to execute
		.tostring = 'repeat '..table.concat(tostring, ' ')..' until '..cond

	-- this could be prettier if we just had 'else' as a var, and did a special-case reinterpret for else->if's
	-- but it would also have more nodes...
	if
		.cond		- condition expression
		[1]...[n] - statements if this condition option works
		.elseifs	- array of 'elseif' condition options
			.cond	- condition expression
			[1]...[n] - statements if this condition option works
		.else		- statements to execute if all other condition options fail
		.tostring = 'if '..cond..' then '..table.concat(if, ' ')..
					table.concat(
						map(elseifs or {}, function(ei) return " elseif "..ei.cond.." then "..table.concat(stmts, " ") end),
						' ')..
					map(else or {}, function(else) return " else "..table.concat(else, ' ') end)

	-- for =
	-- for in
	-- local function
	-- local

last-statements:
	return		- returns a list of expressions
		.exprs
		.tostring = 'return '..table.concat(exprs, ',')

	break		- breaks out of the current loop


	stmt	general parent class of all statements


	block
		[1]...[n]: array of stmt objects
		-tostring: "do "..all statement's tostring().." end"

	func
		-name - string of function name.  optional.
		-args - array of strings of argument names
		-body - block of the statements in the function body



	--]=]

	-- we can similarly insert debug.traceback EVERYWHERE something gets referenced
	so that I CAN GET STACK TRACES FROM ERRORS IN COROUTINES


then we could do tree traversing and graph inferencing
and do some real inline optimization
--]]

return ast
