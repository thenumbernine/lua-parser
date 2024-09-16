--[[
parser.base.ast returns the BaseAST root of all AST nodes

TODO ...
... but parser.lua.ast (and maybe soon parser.grammar.ast) return a collection-of-nodes, which are key'd to the token ... hmm ...
maybe for consistency I should have parser.lua.ast return the LuaAST, which is an BaseAST child, and parent of all Lua AST nodes ...
... and give that node a member htat holds a key/value map to all nodes per token ...
But using a namespace is definitely convenient, especially with all the member subclasses and methods that go in it (traverse, nodeclass, etc)
... though these can easily turn into member fields and member methods

tempting to replace the 'ast' namespace with just LuaAST itself, and keep the convention that keys beginning with `_` are subclasses...
--]]
local table = require 'ext.table'
local tolua = require 'ext.tolua'

local BaseAST = require 'parser.base.ast'


-- namespace table of all Lua AST nodes
-- TODO get rid of parser's dependency on this?  or somehow make the relation between parser rules and ast's closer, like generate the AST from the parser-rules?
-- another TODO how about just storing subclasses as `._type` , then the 'ast' usage outside this file can be just echanged with LuaASTNode itself, and the file can return a class, and lots of things can be simplified
local ast = {}

-- Lua-specific parent class.  root of all other ast node classes in this file.
local LuaAST = BaseAST:subclass()

-- assign to 'ast.node' to define it as the Lua ast's parent-most node class
ast.node = LuaAST

--[[
TODO ... how to make tostring traversal - or any traversal for that matter - modular
this needs to go with 'flatten'
and honestly if we do save all tokens, that's an easy case for in-order traversal and for easily reconstructing the syntax / prettyprint / tostring()
--]]

local function asttolua(x)
	if not x.toLua then
		error('asttolua called on non-ast object '..require 'ext.tolua'(x))
	end
	return x:toLua()
end

-- TODO subsequent toLua() impls have (apply) as an arg, and this is modular for expanding off toLua impls
-- but maybe change those names to 'toLua_recurse' or something
-- and keep a sole external function that provides 'asttolua' as the apply() function
function LuaAST:toLua()
	-- :serialize() impl provided by child classes
	-- :serialize() should call traversal in-order of parsing (why I want to make it auto and assoc wth the parser and grammar and rule-generated ast node classes)
	-- that means serialize() itself should never call serialize() but only call the apply() function passed into it (for modularity's sake)
	-- it might mean i should capture all nodes too, even those that are fixed, like keywords and symbols, for the sake of reassmbling the syntax
	return self:toLua_recursive(asttolua)
end
-- why distinguish toLua(apply) and serialize(apply)?
-- The need for this design pops up more in subclasses.
-- serialize(apply) is used by all language-serializations
-- toLua_recursive(apply) is for Lua-specific serialization (to-be-subclassed)
-- toLua() is the external API
function LuaAST:toLua_recursive(apply)
	return self:serialize(apply)
end

-- lua is the default serialization ... but change this function to change that
function LuaAST:__tostring()
	return self:toLua()
end


function LuaAST:exec(...)
	local code = self:toLua()
	local f, msg = load(code, ...)
	if not f then
		print(require 'template.showcode'(code))
		error(msg)
	end
	return f
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

ast.exec = LuaAST.exec

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
	if not LuaAST:isa(node) then return node end
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

LuaAST.traverse = traverse
ast.traverse = traverse

function LuaAST.copy(n)
	local newn = {}
	setmetatable(newn, getmetatable(n))
	for i=1,#n do
		newn[i] = LuaAST.copy(n[i])
	end
	for _,field in ipairs(fields) do
		local name = field[1]
		local howmuch = field[2]
		local value = n[name]
		if value then
			if howmuch == 'one' then
				if type(value) == 'table' then
					newn[name] = LuaAST.copy(value)
				else
					newn[name] = value
				end
			elseif howmuch == 'many' then
				local newmany = {}
				for k,v in ipairs(value) do
					if type(v) == 'table' then
						newmany[k] = LuaAST.copy(v)
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
ast.copy = LuaAST.copy

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
function LuaAST.flatten(f, varmap)
	f = LuaAST.copy(f)
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
					retexprs[i] = LuaAST.copy(e)
					traverseRecurse(retexprs[i], function(v)
						if ast._arg:isa(v) then
							return LuaAST.copy(n.args[i])
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
ast.flatten = LuaAST.flatten

-- TODO something more flexible than this
ast.spaceseparator = '\n'

local function spacesep(stmts, apply)
	return table.mapi(stmts, apply):concat(ast.spaceseparator)
end

local function commasep(exprs, apply)
	return table.mapi(exprs, apply):concat','
end


local function nodeclass(type, parent, args)
	parent = parent or LuaAST
	local cl = parent:subclass(args)
	cl.type = type
	ast['_'..type] = cl
	return cl
end
ast.nodeclass = nodeclass

-- helper function
local function isLuaName(s)
	return s:match'^[_%a][_%w]*$'
end
function ast.keyIsName(key, parser)
	return ast._string:isa(key)
	-- if key is a string and has no funny chars
	and isLuaName(key.value)
	and (
		-- ... and if we don't have a .parser assigned (as is the case of some dynamic ast manipulation ... *cough* vec-lua *cough* ...)
		not parser
		-- ... or if we do have a parser and this name isn't a keyword in the parser's tokenizer
		or not parser.t.keywords[key.value]
	)
end

-- generic global stmt collection
local _block = nodeclass'block'
function _block:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _block:serialize(apply)
	return spacesep(self, apply)
end

--statements

local _stmt = nodeclass'stmt'

-- TODO 'vars' and 'exprs' should be nodes themselves ...
local _assign = nodeclass('assign', _stmt)
function _assign:init(vars, exprs)
	self.vars = table(vars)
	self.exprs = table(exprs)
end
function _assign:serialize(apply)
	return commasep(self.vars, apply)..'='..commasep(self.exprs, apply)
end

-- should we impose construction constraints _do(_block(...))
-- or should we infer?  _do(...) = {type = 'do', block = {type = 'block, ...}}
-- or should we do neither?  _do(...) = {type = 'do', ...}
-- neither for now
-- but that means _do and _block are identical ...
local _do = nodeclass('do', _stmt)
function _do:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _do:serialize(apply)
	return 'do '..spacesep(self, apply)..' end'
end

local _while = nodeclass('while', _stmt)
-- TODO just make self[1] into the cond ...
function _while:init(cond, ...)
	self.cond = cond
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _while:serialize(apply)
	return 'while '..apply(self.cond)..' do '..spacesep(self, apply)..' end'
end

local _repeat = nodeclass('repeat', _stmt)
-- TODO just make self[1] into the cond ...
function _repeat:init(cond, ...)
	self.cond = cond
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _repeat:serialize(apply)
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
local _if = nodeclass('if', _stmt)
-- TODO maybe just assert the node types and store them as-is in self[i]
function _if:init(cond,...)
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
function _if:serialize(apply)
	local s = 'if '..apply(self.cond)..' then '..spacesep(self, apply)
	for _,ei in ipairs(self.elseifs) do
		s = s .. apply(ei)
	end
	if self.elsestmt then s = s .. apply(self.elsestmt) end
	s = s .. ' end'
	return s
end

-- aux for _if
local _elseif = nodeclass('elseif', _stmt)
-- TODO just make self[1] into the cond ...
function _elseif:init(cond,...)
	self.cond = cond
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _elseif:serialize(apply)
	return ' elseif '..apply(self.cond)..' then '..spacesep(self, apply)
end

-- aux for _if
local _else = nodeclass('else', _stmt)
function _else:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _else:serialize(apply)
	return ' else '..spacesep(self, apply)
end

local _foreq = nodeclass('foreq', _stmt)
-- step is optional
-- TODO just make self[1..4] into the var, min, max, step ...
-- ... this means we can possibly have a nil child mid-sequence ...
-- .. hmm ...
-- ... which is better:
-- *) requiring table.max for integer iteration instead of ipairs
-- *) or using fields instead of integer indexes?
function _foreq:init(var,min,max,step,...)
	self.var = var
	self.min = min
	self.max = max
	self.step = step
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _foreq:serialize(apply)
	local s = 'for '..apply(self.var)..' = '..apply(self.min)..','..apply(self.max)
	if self.step then s = s..','..apply(self.step) end
	s = s .. ' do '..spacesep(self, apply)..' end'
	return s
end

-- TODO 'vars' should be a node itself
local _forin = nodeclass('forin', _stmt)
function _forin:init(vars, iterexprs, ...)
	self.vars = vars
	self.iterexprs = iterexprs
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _forin:serialize(apply)
	return 'for '..commasep(self.vars, apply)..' in '..commasep(self.iterexprs, apply)..' do '..spacesep(self, apply)..' end'
end

local _function = nodeclass('function', _stmt)
-- name is optional
-- TODO make 'args' a node
function _function:init(name, args, ...)
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
function _function:serialize(apply)
	local s = 'function '
	if self.name then s = s .. apply(self.name) end
	s = s .. '('
		.. table.mapi(self.args, apply):concat','
		.. ') ' .. spacesep(self, apply) .. ' end'
	return s
end

-- aux for _function
local _arg = nodeclass'arg'
-- TODO just self[1] ?
function _arg:init(index)
	self.index = index
end
-- params need to know what function they're in
-- so they can reference the function's arg names
function _arg:serialize(apply)
	return 'arg'..self.index
end

-- _local can be an assignment of multi vars to muli exprs
--  or can optionally be a declaration of multi vars with no statements
-- so it will take the form of assignments
-- but it can also be a single function declaration with no equals symbol ...
-- the parser has to accept functions and variables as separate conditions
--  I'm tempted to make them separate symbols here too ...
-- exprs is a table containing: 1) a single function 2) a single assign statement 3) a list of variables
local _local = nodeclass('local', _stmt)
-- TODO just self[1] instead of self.exprs[i]
function _local:init(exprs)
	if ast._function:isa(exprs[1]) or ast._assign:isa(exprs[1]) then
		assert(#exprs == 1, "local functions or local assignments must be the only child")
	end
	self.exprs = table(assert(exprs))
end
function _local:serialize(apply)
	if ast._function:isa(self.exprs[1]) or ast._assign:isa(self.exprs[1]) then
		return 'local '..apply(self.exprs[1])
	else
		return 'local '..commasep(self.exprs, apply)
	end
end

-- control

local _return = nodeclass('return', _stmt)
-- TODO either 'exprs' a node of its own, or flatten it into 'return'
function _return:init(...)
	self.exprs = {...}
end
function _return:serialize(apply)
	return 'return '..commasep(self.exprs, apply)
end

local _break = nodeclass('break', _stmt)
function _break:serialize(apply) return 'break' end

local _call = nodeclass'call'
-- TODO 'args' a node of its own ?  or store it in self[i] ?
function _call:init(func, ...)
	self.func = func
	self.args = {...}
end
function _call:serialize(apply)
	if #self.args == 1
	and (ast._table:isa(self.args[1])
		or ast._string:isa(self.args[1])
	) then
		return apply(self.func)..apply(self.args[1])
	end
	return apply(self.func)..'('..commasep(self.args, apply)..')'
end

local _nil = nodeclass'nil'
_nil.const = true
function _nil:serialize(apply) return 'nil' end

local _boolean = nodeclass'boolean'

local _true = nodeclass('true', _boolean)
_true.const = true
_true.value = true
function _true:serialize(apply) return 'true' end

local _false = nodeclass('false', _boolean)
_false.const = true
_false.value = false
function _false:serialize(apply) return 'false' end

local _number = nodeclass'number'
-- TODO just self[1] instead of self.value ?
-- but this breaks convention with _boolean having .value as its static member value.
-- I could circumvent this with _boolean subclass [1] holding the value ...
function _number:init(value) self.value = value end
function _number:serialize(apply) return self.value end

local _string = nodeclass'string'
-- TODO just self[1] instead of self.value
function _string:init(value) self.value = value end
function _string:serialize(apply)
	-- use ext.tolua's string serializer
	return tolua(self.value)
end

local _vararg = nodeclass'vararg'
function _vararg:serialize(apply) return '...' end

-- TODO 'args' a node, or flatten into self[i] ?
local _table = nodeclass'table'	-- single-element assigns
function _table:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _table:serialize(apply)
	return '{'..table.mapi(self, function(arg)
		-- if it's an assign then wrap the vars[1] with []'s
		if ast._assign:isa(arg) then
			assert(#arg.vars == 1)
			assert(#arg.exprs == 1)
			-- TODO if it's a string and name and not a keyword then use our shorthand
			-- but for this , I should put the Lua keywords in somewhere that both the AST and Tokenizer can see them
			-- and the Tokenizer builds separate lists depending on the version (so I guess a table per version?)
			return (ast.keyIsName(arg.vars[1], self.parser)
					and arg.vars[1].value
					or '[' .. apply(arg.vars[1]) .. ']'
				)..'='..apply(arg.exprs[1])

		end
		return apply(arg)
	end):concat(',')..'}'
end

-- OK here is the classic example of the benefits of fields over integers:
-- extensibility.
-- attrib was added later
-- as we add/remove fields, that means reordering indexes, and that means a break in compat
-- one workaround to merging the two is just named functions and integer-indexed children
-- another is a per-child traversal routine (like :serialize())
local _var = nodeclass'var'	-- variable, lhs of ast._assign's, similar to _arg
function _var:init(name, attrib)
	self.name = name
	self.attrib = attrib
end
function _var:serialize(apply)
	local s = self.name
	if self.attrib then
		-- the extra space is needed for assignments, otherwise lua5.4 `local x<const>=1` chokes while `local x<const> =1` works
		s = s .. '<'..self.attrib..'> '
	end
	return s
end

local _par = nodeclass'par'
ast._par = _par
ast._parenthesis = nil
function _par:init(expr)
	self.expr = expr
end
function _par:serialize(apply)
	return '('..apply(self.expr)..')'
end

local _index = nodeclass'index'
function _index:init(expr,key)
	self.expr = expr
	-- helper add wrappers to some types:
	-- TODO or not?
	if type(key) == 'string' then
		key = ast._string(key)
	elseif type(key) == 'number' then
		key = ast._number(key)
	end
	self.key = key
end
function _index:serialize(apply)
	if ast.keyIsName(self.key, self.parser) then
		-- the use a .$key instead of [$key]
		return apply(self.expr)..'.'..self.key.value
	end
	return apply(self.expr)..'['..apply(self.key)..']'
end

-- this isn't the () call itself, this is just the : dereference
-- a:b(c) is _call(_indexself(_var'a', _var'b'), _var'c')
-- technically this is a string lookup, however it is only valid as a lua name, so I'm just passing the Lua string itself
local _indexself = nodeclass'indexself'
function _indexself:init(expr,key)
	self.expr = assert(expr)
	assert(isLuaName(key))
	-- TODO compat with _index?  always wrap?  do this before passing in key?
	--key = ast._string(key)
	self.key = assert(key)
end
function _indexself:serialize(apply)
	return apply(self.expr)..':'..self.key
end

local _op = nodeclass'op'
-- TODO 'args' a node ... or just flatten it into this node ...
function _op:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _op:serialize(apply)
	return table.mapi(self, apply):concat(' '..self.op..' ') -- spaces required for 'and' and 'or'
end

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
	local op = info[2]
	local cl = nodeclass(info[1], _op)
	cl.op = op
end

for _,info in ipairs{
	{'unm','-'},
	{'not','not'},
	{'len','#'},
	{'bnot','~'},		-- 5.3+
} do
	local op = info[2]
	local cl = nodeclass(info[1], _op)
	cl.op = op
	function cl:init(...)
		for i=1,select('#', ...) do
			self[i] = select(i, ...)
		end
	end
	function cl:serialize(apply)
		return ' '..self.op..' '..apply(self[1])	-- spaces required for 'not'
	end
end

local _goto = nodeclass('goto', _stmt)
function _goto:init(name)
	self.name = name
end
function _goto:serialize(apply)
	return 'goto '..self.name
end

local _label = nodeclass('label', _stmt)
function _label:init(name)
	self.name = name
end
function _label:serialize(apply)
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
