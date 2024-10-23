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
local assert = require 'ext.assert'
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

-- TODO subsequent toLua() impls have (consume) as an arg, and this is modular for expanding off toLua impls
-- but maybe change those names to 'toLua_recurse' or something
-- and keep a sole external function that provides consume()
function LuaAST:serializeRecursiveMember(field)
	local s = ''
	-- :serialize() impl provided by child classes
	-- :serialize() should call traversal in-order of parsing (why I want to make it auto and assoc wth the parser and grammar and rule-generated ast node classes)
	-- that means serialize() itself should never call serialize() but only call the consume() function passed into it (for modularity's sake)
	-- it might mean i should capture all nodes too, even those that are fixed, like keywords and symbols, for the sake of reassmbling the syntax
	local consume
	local lastspan
	consume = function(x)
		if type(x) == 'number' then
			x = tostring(x)
		end
		if type(x) == 'string' then
			-- here's our only string join

			-- TODO here if you want ... pad lines and cols until we match the original location (or exceed it)
			-- to do that, track appended strings to have a running line/col counter just like we do in parser
			-- to do that, separate teh updatelinecol() in the parser to work outside datareader

			-- if we have a name coming in, only insert a space if we were already at a name
			local namelhs = s:sub(-1):match'[_%w]'
			local namerhs = x:sub(1,1):match'[_%w]'
			if namelhs and namerhs then
				s = s .. ' '
			elseif not namelhs and not namerhs then
				-- TODO here for minification if you want
				-- if we have a symbol coming in, only insert a space if we were already at a symbol and the two together would make a different valid symbol
				-- you'll need to search back the # of the max length of any symbol ...
				s = s .. ' '
			end

			s = s .. x
		elseif type(x) == 'table' then
			lastspan = x.span
			assert.is(x, BaseAST)
			assert.index(x, field)
			x[field](x, consume)
		else
			error('here with unknown type '..type(x))
		end
	end
	self[field](self, consume)
	return s
end

function LuaAST:toLua()
	return self:serializeRecursiveMember'toLua_recursive'
end

-- why distinguish toLua() and serialize(consume)?
-- The need for this design pops up more in subclasses.
-- serialize(consume) is used by all language-serializations
-- toLua_recursive(consume) is for Lua-specific serialization (to-be-subclassed)
-- I'm not sure if this is better than just using a fully separate table of serialization functions per node ...
-- toLua() is the external API
function LuaAST:toLua_recursive(consume)
	return self:serialize(consume)
end

-- ok maybe it's not such a good idea to use tostring and serialization for the same purpose ...
LuaAST.__tostring = string.nametostring

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
			local funcname = n.func:toLua()	-- in case it's a var ... ?
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
						-- _arg is not used by parser - externally used only - I should move flatten somewhere else ... 
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

local function consumeconcat(consume, t, sep)
	for i,x in ipairs(t) do
		consume(x)
		if sep and i < #t then
			consume(sep)
		end
	end
end

local function spacesep(stmts, consume)
	consumeconcat(consume, stmts)
end

local function commasep(exprs, consume)
	consumeconcat(consume, exprs, ',')
end

local function nodeclass(type, parent, args)
	parent = parent or LuaAST
	local cl = parent:subclass(args)
	cl.type = type
	cl.__name = type
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
function _block:serialize(consume)
	spacesep(self, consume)
end

--statements

local _stmt = nodeclass'stmt'

-- TODO 'vars' and 'exprs' should be nodes themselves ...
local _assign = nodeclass('assign', _stmt)
function _assign:init(vars, exprs)
	self.vars = table(vars)
	self.exprs = table(exprs)
end
function _assign:serialize(consume)
	commasep(self.vars, consume)
	consume'='
	commasep(self.exprs, consume)
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
function _do:serialize(consume)
	consume'do'
	spacesep(self, consume)
	consume'end'
end

local _while = nodeclass('while', _stmt)
-- TODO just make self[1] into the cond ...
function _while:init(cond, ...)
	self.cond = cond
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _while:serialize(consume)
	consume'while'
	consume(self.cond)
	consume'do'
	spacesep(self, consume)
	consume'end'
end

local _repeat = nodeclass('repeat', _stmt)
-- TODO just make self[1] into the cond ...
function _repeat:init(cond, ...)
	self.cond = cond
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _repeat:serialize(consume)
	consume'repeat'
	spacesep(self, consume)
	consume'until'
	consume(self.cond)
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
function _if:serialize(consume)
	consume'if'
	consume(self.cond)
	consume'then'
	spacesep(self, consume)
	for _,ei in ipairs(self.elseifs) do
		consume(ei)
	end
	if self.elsestmt then
		consume(self.elsestmt)
	end
	consume'end'
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
function _elseif:serialize(consume)
	consume'elseif'
	consume(self.cond)
	consume'then'
	spacesep(self, consume)
end

-- aux for _if
local _else = nodeclass('else', _stmt)
function _else:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _else:serialize(consume)
	consume'else'
	spacesep(self, consume)
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
function _foreq:serialize(consume)
	consume'for'
	consume(self.var)
	consume'='
	consume(self.min)
	consume','
	consume(self.max)
	if self.step then
		consume','
		consume(self.step)
	end
	consume'do'
	spacesep(self, consume)
	consume'end'
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
function _forin:serialize(consume)
	consume'for'
	commasep(self.vars, consume)
	consume'in'
	commasep(self.iterexprs, consume)
	consume'do'
	spacesep(self, consume)
	consume'end'
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
function _function:serialize(consume)
	consume'function'
	if self.name then
		consume(self.name)
	end
	consume'('
	commasep(self.args, consume)
	consume')'
	spacesep(self, consume)
	consume'end'
end

-- aux for _function
-- not used by parser - externally used only - I should get rid of it
local _arg = nodeclass'arg'
-- TODO just self[1] ?
function _arg:init(index)
	self.index = index
end
-- params need to know what function they're in
-- so they can reference the function's arg names
function _arg:serialize(consume)
	consume('arg'..self.index)
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
function _local:serialize(consume)
	if ast._function:isa(self.exprs[1]) or ast._assign:isa(self.exprs[1]) then
		consume'local'
		consume(self.exprs[1])
	else
		consume'local'
		commasep(self.exprs, consume)
	end
end

-- control

local _return = nodeclass('return', _stmt)
-- TODO either 'exprs' a node of its own, or flatten it into 'return'
function _return:init(...)
	self.exprs = {...}
end
function _return:serialize(consume)
	consume'return'
	commasep(self.exprs, consume)
end

local _break = nodeclass('break', _stmt)
function _break:serialize(consume) consume'break' end

local _call = nodeclass'call'
-- TODO 'args' a node of its own ?  or store it in self[i] ?
function _call:init(func, ...)
	self.func = func
	self.args = {...}
end
function _call:serialize(consume)
	if #self.args == 1
	and (ast._table:isa(self.args[1])
		or ast._string:isa(self.args[1])
	) then
		consume(self.func)
		consume(self.args[1])
	else
		consume(self.func)
		consume'('
		commasep(self.args, consume)
		consume')'
	end
end

local _nil = nodeclass'nil'
_nil.const = true
function _nil:serialize(consume) consume'nil' end

local _boolean = nodeclass'boolean'

local _true = nodeclass('true', _boolean)
_true.const = true
_true.value = true
function _true:serialize(consume) consume'true' end

local _false = nodeclass('false', _boolean)
_false.const = true
_false.value = false
function _false:serialize(consume) consume'false' end

local _number = nodeclass'number'
-- TODO just self[1] instead of self.value ?
-- but this breaks convention with _boolean having .value as its static member value.
-- I could circumvent this with _boolean subclass [1] holding the value ...
function _number:init(value) self.value = value end
function _number:serialize(consume) consume(tostring(self.value)) end

local _string = nodeclass'string'
-- TODO just self[1] instead of self.value
function _string:init(value) self.value = value end
function _string:serialize(consume)
	-- use ext.tolua's string serializer
	consume(tolua(self.value))
end

local _vararg = nodeclass'vararg'
function _vararg:serialize(consume) consume'...' end

-- TODO 'args' a node, or flatten into self[i] ?
local _table = nodeclass'table'	-- single-element assigns
function _table:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _table:serialize(consume)
	consume'{'
	for i,arg in ipairs(self) do
		-- if it's an assign then wrap the vars[1] with []'s
		if ast._assign:isa(arg) then
			assert.len(arg.vars, 1)
			assert.len(arg.exprs, 1)
			-- TODO if it's a string and name and not a keyword then use our shorthand
			-- but for this , I should put the Lua keywords in somewhere that both the AST and Tokenizer can see them
			-- and the Tokenizer builds separate lists depending on the version (so I guess a table per version?)
			if ast.keyIsName(arg.vars[1], self.parser) then
				consume(arg.vars[1].value)
			else
				consume'['
				consume(arg.vars[1])
				consume']'
			end
			consume'='
			consume(arg.exprs[1])
		else
			consume(arg)
		end
		if i < #self then
			consume','
		end
	end
	consume'}'
end

-- OK here is the classic example of the benefits of fields over integers:
-- extensibility.
-- attrib was added later
-- as we add/remove fields, that means reordering indexes, and that means a break in compat
-- one workaround to merging the two is just named functions and integer-indexed children
-- another is a per-child traversal routine (like :serialize())
local _var = nodeclass'var'	-- variable, lhs of ast._assign's
function _var:init(name, attrib)
	self.name = name
	self.attrib = attrib
end
function _var:serialize(consume)
	consume(self.name)
	if self.attrib then
		-- the extra space is needed for assignments, otherwise lua5.4 `local x<const>=1` chokes while `local x<const> =1` works
		consume'<'
		consume(self.attrib)
		consume'>'
	end
end

local _par = nodeclass'par'
ast._par = _par
ast._parenthesis = nil
function _par:init(expr)
	self.expr = expr
end
function _par:serialize(consume)
	consume'('
	consume(self.expr)
	consume')'
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
function _index:serialize(consume)
	if ast.keyIsName(self.key, self.parser) then
		-- the use a .$key instead of [$key]
		consume(self.expr)
		consume'.'
		consume(self.key.value)
	else
		consume(self.expr)
		consume'['
		consume(self.key)
		consume']'
	end
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
function _indexself:serialize(consume)
	consume(self.expr)
	consume':'
	consume(self.key)
end

local _op = nodeclass'op'
-- TODO 'args' a node ... or just flatten it into this node ...
function _op:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _op:serialize(consume)
	for i,x in ipairs(self) do
		consume(x)
		if i < #self then
			-- spaces required for 'and' and 'or'
			consume(self.op)
		end
	end
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
	function cl:serialize(consume)
		consume(self.op)
		consume(self[1])	-- spaces required for 'not'
	end
end

local _goto = nodeclass('goto', _stmt)
function _goto:init(name)
	self.name = name
end
function _goto:serialize(consume)
	consume'goto'
	consume(self.name)
end

local _label = nodeclass('label', _stmt)
function _label:init(name)
	self.name = name
end
function _label:serialize(consume)
	consume'::'
	consume(self.name)
	consume'::'
end

return ast
