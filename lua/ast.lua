--[[
TODO ...
... parser.base.ast returns the ASTNode root of all AST nodes
... but parser.lua.ast (and maybe soon parser.grammar.ast) return a collection-of-nodes, which are key'd to the token ... hmm ...
maybe for consistency I should have parser.lua.ast return the LuaNode, which is an ASTNode child, and parent of all Lua AST nodes ...
... and give that node a member htat holds a key/value map to all nodes per token ...
--]]
local table = require 'ext.table'
local string = require 'ext.string'
local tolua = require 'ext.tolua'

local ASTNode = require 'parser.base.ast'


-- namespace table of all Lua AST nodes
-- TODO get rid of parser's dependency on this?  or somehow make the relation between parser rules and ast's closer, like generate the AST from the parser-rules?
local ast = {}

-- put all subclasses here
local allclasses = table()
ast.allclasses = allclasses


-- Lua-specific parent class:
local LuaNode = ASTNode:subclass()
allclasses:insert(LuaNode)
ast.node = LuaNode		-- assign to 'ast.node' to define it as the ast's parent-most node class

LuaNode.__concat = string.concat

function LuaNode:setspan(span)
	self.span = span
	return self
end


--[[
TODO ... how to make tostring traversal - or any traversal for that matter - modular
this needs to go with 'flatten'
and honestly if we do save all tokens, that's an easy case for in-order traversal and for easily reconstructing the syntax / prettyprint / tostring()
--]]

local function asttolua(x)
	return x:toLua()
end

function LuaNode:toLua()
	return self:serialize(asttolua)	-- :serialize() impl provided by child classes
end

-- lua is the default serialization ... but change this function to change that
function LuaNode:__tostring()
	return self:toLua()
end


function LuaNode:exec(...)
	return assert(load(self:toLua(), ...))
end


-- TODO what's a more flexible way of iterating through all child fields?
-- and what's a more flexible way of constructing AST node subclass, and of specifying their fields,
--  especially with grammar rule construction?
-- ... how about instead make all fields indexed, and then for certain classes give them aliases into the fields?
-- ... same with htmlparser?
-- then in line with this, fields will either point to nodes, or point to tables to nodes?
--  or maybe the tables-of-nodes should themselves be AST nodes?
local fields = {
	{'name', 'field'},
	{'index', 'field'},
	{'value', 'field'},
	{'span', 'field'},
	{'cond', 'one'},
	{'var', 'one'},
	{'min', 'one'},
	{'max', 'one'},
	{'step', 'one'},
	{'func', 'one'},		-- should this be a _function, or a string depicting a function?
	{'arg', 'one'},
	{'key', 'one'},
	{'expr', 'one'},
	{'stmt', 'one'},
	{'args', 'many'},
	{'exprs', 'many'},
	{'elseifs', 'many'},
	{'elsestmt', 'many'},
	{'vars', 'many'},
}

ast.exec = LuaNode.exec

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
	if not LuaNode:isa(node) then return node end
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

LuaNode.traverse = traverse
ast.traverse = traverse

function LuaNode.copy(n)
	local newn = {}
	setmetatable(newn, getmetatable(n))
	for i=1,#n do
		newn[i] = LuaNode.copy(n[i])
	end
	for _,field in ipairs(fields) do
		local name = field[1]
		local howmuch = field[2]
		local value = n[name]
		if value then
			if howmuch == 'one' then
				if type(value) == 'table' then
					newn[name] = LuaNode.copy(value)
				else
					newn[name] = value
				end
			elseif howmuch == 'many' then
				local newmany = {}
				for k,v in ipairs(value) do
					if type(v) == 'table' then
						newmany[k] = LuaNode.copy(v)
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
ast.copy = LuaNode.copy

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
function LuaNode.flatten(f, varmap)
	f = LuaNode.copy(f)
	traverseRecurse(f, function(n)
		if type(n) == 'table'
		and ast._call:isa(n)
		then
			local funcname = asttolua(n.func)	-- in case it's a var ... ? TODO modular asttolua => apply ?
			assert(funcname, "can't flatten a function with anonymous calls")
			local f = varmap[funcname]
			if f
			and #f == 1
			and ast._return:isa(f[1])
			then
				local retexprs = {}
				for i,e in ipairs(f[1].exprs) do
					retexprs[i] = LuaNode.copy(e)
					traverseRecurse(retexprs[i], function(v)
						if type(v) == 'table'
						and ast._arg:isa(v)
						then
							return LuaNode.copy(n.args[i])
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
ast.flatten = LuaNode.flatten

-- TODO something more flexible than this
ast.spaceseparator = '\n'

local function spacesep(stmts, apply)
	return table.mapi(stmts, apply):concat(ast.spaceseparator)
end

local function commasep(exprs, apply)
	return table.mapi(exprs, apply):concat','
end


-- TODO get rid of this function completely plz ... but it's convenient for capturing all children as I declare them ...
local function nodeclass(contents, parent)
	parent = parent or ast.node
	local newclass = parent:subclass()
	for k,v in pairs(contents) do
		newclass[k] = v
	end
	allclasses:insert(newclass)
	return newclass
end
ast.nodeclass = nodeclass

local function nodeclasstyped(type, parent, args)
	args = args or {}
	args.type = type					-- assign type
	local cl = nodeclass(args, parent)	-- make class and add to 'allclasses'
	ast['_'..type] = cl					-- add to namespace
	return cl
end

-- generic global stmt collection
local _block = nodeclasstyped'block'
function _block:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _block:serialize(apply)
	return spacesep(self, apply)
end

--statements

local _stmt = LuaNode:subclass()
ast._stmt = _stmt

-- TODO 'vars' and 'exprs' should be nodes themselves ...
ast._assign = nodeclasstyped('assign', _stmt)
function ast._assign:init(vars, exprs)
	self.vars = table(vars)
	self.exprs = table(exprs)
end
function ast._assign:serialize(apply)
	return commasep(self.vars, apply)..'='..commasep(self.exprs, apply)
end

-- should we impose construction constraints _do(_block(...))
-- or should we infer?  _do(...) = {type = 'do', block = {type = 'block, ...}}
-- or should we do neither?  _do(...) = {type = 'do', ...}
-- neither for now
ast._do = nodeclasstyped('do', _stmt)
function ast._do:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function ast._do:serialize(apply)
	return 'do '..spacesep(self, apply)..' end'
end

ast._while = nodeclasstyped('while', _stmt)
function ast._while:init(cond, ...)
	self.cond = cond
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function ast._while:serialize(apply)
	return 'while '..apply(self.cond)..' do '..spacesep(self, apply)..' end'
end

ast._repeat = nodeclasstyped('repeat', _stmt)
function ast._repeat:init(cond, ...)
	self.cond = cond
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function ast._repeat:serialize(apply)
	return 'repeat '..spacesep(self, apply)..' until '..apply(self.cond)
end

--[[
_if(_eq(a,b),
	_assign({a},{2}),
	_elseif(...),
	_elseif(...),
	_else(...))
--]]
-- weird one, idk how to reformat
ast._if = nodeclasstyped('if', _stmt)
function ast._if:init(cond,...)
	local elseifs = table()
	local elsestmt, laststmt
	for i=1,select('#', ...) do
		local stmt = select(i, ...)
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
function ast._if:serialize(apply)
	local s = 'if '..apply(self.cond)..' then '..spacesep(self, apply)
	for _,ei in ipairs(self.elseifs) do
		s = s .. apply(ei)
	end
	if self.elsestmt then s = s .. apply(self.elsestmt) end
	s = s .. ' end'
	return s
end

-- aux for _if
ast._elseif = nodeclasstyped('elseif', _stmt)
function ast._elseif:init(cond,...)
	self.cond = cond
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function ast._elseif:serialize(apply)
	return ' elseif '..apply(self.cond)..' then '..spacesep(self, apply)
end

-- aux for _if
ast._else = nodeclasstyped('else', _stmt)
function ast._else:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function ast._else:serialize(apply)
	return ' else '..spacesep(self, apply)
end

ast._foreq = nodeclasstyped('foreq', _stmt)
-- step is optional
function ast._foreq:init(var,min,max,step,...)
	self.var = var
	self.min = min
	self.max = max
	self.step = step
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function ast._foreq:serialize(apply)
	local s = 'for '..apply(self.var)..' = '..apply(self.min)..','..apply(self.max)
	if self.step then s = s..','..apply(self.step) end
	s = s .. ' do '..spacesep(self, apply)..' end'
	return s
end

-- TODO 'vars' should be a node itself
ast._forin = nodeclasstyped('forin', _stmt)
function ast._forin:init(vars,iterexprs, ...)
	self.vars = vars
	self.iterexprs = iterexprs
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function ast._forin:serialize(apply)
	return 'for '..commasep(self.vars, apply)..' in '..commasep(self.iterexprs, apply)..' do '..spacesep(self, apply)..' end'
end

ast._function = nodeclasstyped('function', _stmt)
-- name is optional
-- TODO make 'args' a node
function ast._function:init(name, args, ...)
	-- prep args...
	for i=1,#args do
		args[i].index = i
		args[i].param = true
	end
	self.name = name
	self.args = args
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function ast._function:serialize(apply)
	local s = 'function '
	if self.name then s = s .. apply(self.name) end
	s = s .. '('
		.. table.mapi(self.args, apply):concat','
		.. ') ' .. spacesep(self, apply) .. ' end'
	return s
end

-- aux for _function
ast._arg = nodeclasstyped'arg'
function ast._arg:init(index)
	self.index = index
end
-- params need to know what function they're in
-- so they can reference the function's arg names
function ast._arg:serialize(apply)
	return 'arg'..self.index
end

-- _local can be an assignment of multi vars to muli exprs
--  or can optionally be a declaration of multi vars with no statements
-- so it will take the form of assignments
-- but it can also be a single function declaration with no equals symbol ...
-- the parser has to accept functions and variables as separate conditions
--  I'm tempted to make them separate symbols here too ...
-- exprs is a table containing: 1) a single function 2) a single assign statement 3) a list of variables
ast._local = nodeclasstyped('local', _stmt)
function ast._local:init(exprs)
	if ast._function:isa(exprs[1]) or ast._assign:isa(exprs[1]) then
		assert(#exprs == 1, "local functions or local assignments must be the only child")
	end
	self.exprs = table(assert(exprs))
end
function ast._local:serialize(apply)
	if ast._function:isa(self.exprs[1]) or ast._assign:isa(self.exprs[1]) then
		return 'local '..apply(self.exprs[1])
	else
		return 'local '..commasep(self.exprs, apply)
	end
end

-- control

-- TODO either 'exprs' a node of its own, or flatten it into 'return'
ast._return = nodeclasstyped('return', _stmt)
function ast._return:init(...)
	self.exprs = {...}
end
function ast._return:serialize(apply)
	return 'return '..commasep(self.exprs, apply)
end

ast._break = nodeclasstyped('break', _stmt)
function ast._break:serialize(apply) return 'break' end

-- TODO 'args' a node of its own ?
ast._call = nodeclasstyped'call'
function ast._call:init(func, ...)
	self.func = func
	self.args = {...}
end
function ast._call:serialize(apply)
	if #self.args == 1
	and (ast._table:isa(self.args[1])
		or ast._string:isa(self.args[1])
	) then
		return apply(self.func)..apply(self.args[1])
	end
	return apply(self.func)..'('..commasep(self.args, apply)..')'
end

ast._nil = nodeclasstyped'nil'
ast._nil.const = true
function ast._nil:serialize(apply) return 'nil' end

ast._true = nodeclasstyped'boolean'
ast._true.const = true
ast._true.value = true
function ast._true:serialize(apply) return 'true' end

ast._false = nodeclasstyped'boolean'
ast._false.const = true
ast._false.value = false
function ast._false:serialize(apply) return 'false' end

ast._number = nodeclasstyped'number'
function ast._number:init(value) self.value = value end
function ast._number:serialize(apply) return self.value end

ast._string = nodeclasstyped'string'
function ast._string:init(value) self.value = value end
function ast._string:serialize(apply)
	-- use ext.tolua's string serializer
	return tolua(self.value)
end

ast._vararg = nodeclasstyped'vararg'
function ast._vararg:serialize(apply) return '...' end

-- TODO 'args' a node
ast._table = nodeclasstyped'table'	-- single-element assigns
function ast._table:init(args)
	self.args = table(assert(args))
end
function ast._table:serialize(apply)
	return '{'..self.args:mapi(function(arg)
		-- if it's an assign then wrap the vars[1] with []'s
		if ast._assign:isa(arg) then
			assert(#arg.vars == 1)
			assert(#arg.exprs == 1)
			return '[' .. apply(arg.vars[1]) .. '] = '..apply(arg.exprs[1])
		end
		return apply(arg)
	end):concat(',')..'}'
end

ast._var = nodeclasstyped'var'	-- variable, lhs of ast._assign's, similar to _arg
function ast._var:init(name, attrib)
	self.name = name
	self.attrib = attrib
end
function ast._var:serialize(apply)
	local s = self.name
	if self.attrib then
		-- the extra space is needed for assignments, otherwise lua5.4 `local x<const>=1` chokes while `local x<const> =1` works
		s = s .. '<'..self.attrib..'> '
	end
	return s
end

ast._par = nodeclasstyped'parenthesis'
function ast._par:init(expr)
	self.expr = expr
end
function ast._par:serialize(apply)
	return '('..apply(self.expr)..')'
end

local function isLuaName(s)
	return s:match'^[_%a][_%w]*$'
end

ast._index = nodeclasstyped'index'
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
function ast._index:serialize(apply)
-- TODO - if self.key is a string and has no funny chars the use a .$key instead of [$key]
	if ast._string:isa(self.key)
	and isLuaName(self.key.value)
	then
		return apply(self.expr)..'.'..self.key.value
	end
	return apply(self.expr)..'['..apply(self.key)..']'
end

-- this isn't the () call itself, this is just the : dereference
-- a:b(c) is _call(_indexself(_var'a', _var'b'), _var'c')
-- technically this is a string lookup, however it is only valid as a lua name, so I'm just passing the Lua string itself
ast._indexself = nodeclasstyped'indexself'
function ast._indexself:init(expr,key)
	self.expr = assert(expr)
	assert(isLuaName(key))
	self.key = assert(key)
end
function ast._indexself:serialize(apply)
	return apply(self.expr)..':'..self.key
end

ast._op = LuaNode:subclass()

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
	{'idiv', '//'},	-- 5.3+
	{'band', '&'},	-- 5.3+
	{'bxor', '~'},	-- 5.3+
	{'bor', '|'},	-- 5.3+
	{'shl', '<<'},	-- 5.3+
	{'shr', '>>'},	-- 5.3+
} do
	local name = info[1]
	local op = info[2]
	local cl = nodeclass({type = info[1], op = op}, ast._op)
	ast['_'..name] = cl
	-- TODO 'args' a node ... or just flatten it into this node ...
	function cl:init(...)
		self.args = {...}
	end
	function cl:serialize(apply)
		return table.mapi(self.args, apply):concat(' '..self.op..' ') -- spaces required for 'and' and 'or'
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
	function cl:serialize(apply)
		return ' '..self.op..' '..apply(self.arg)	-- spaces required for 'not'
	end
end

ast._goto = nodeclasstyped('goto', _stmt)
function ast._goto:init(name)
	self.name = name
end
function ast._goto:serialize(apply)
	return 'goto '..self.name
end

ast._label = nodeclasstyped('label', _stmt)
function ast._label:init(name)
	self.name = name
end
function ast._label:serialize(apply)
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
