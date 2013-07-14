local class = require 'ext.class'

module('ast', package.seeall)

function spacesep(stmts)
	return table.concat(table.map(stmts, tostring), ' ')
end

function commasep(exprs)
	return table.concat(table.map(exprs, tostring), ',')
end

-- generic global stmt collection
_block = class{type='block'}
function _block:init(...)
	self.stmts = {...}
end
function _block:__tostring()
	return spacesep(self.stmts)
end

--statements

_assign = class{type='assign'}
function _assign:init(...)
	local args = {...}
	local h = #args/2
	assert(h == math.floor(h))
	local vars = table()
	local exprs = table()
	for i=1,h do
		vars:insert(args[i])
		exprs:insert(args[i+h])
	end
	self.vars = vars
	self.exprs = exprs
end
function _assign:__tostring()
	return commasep(self.vars)..' = '..commasep(self.exprs)
end

-- should we impose construction constraints _do(_block(...))
-- or should we infer?  _do(...) = {type='do', block={type='block, ...}}
-- or should we do neither?  _do(...) = {type='do', ...}
-- neither for now
_do = class{type='do'}
function _do:init(...)
	self.stmts = {...}
end
function _do:__tostring()
	return 'do '..spacesep(self.stmts)..' end'
end

_while = class{type='while'}
function _while:init(cond, ...)
	self.cond=cond
	self.stmts={...}
end
function _while:__tostring()
	return 'while '..tostring(self.cond)..' do '..spacesep(self.stmts)..' end'
end

_repeat = class{type='repeat'}
function _repeat:init(cond, ...)
	self.cond = cond
	self.stmts = {...}
end
function _repeat:__tostring()
	return 'repeat '..spacesep(self.stmts)..' until '..tostring(self.cond)
end

--[[
_if(_eq(a,b),
		_assign(a,2),
	__elseif(...),
	__elseif(...),
	__else(...))
--]]
_if = class{type='if'}
function _if:init(cond,...)
	local stmts = table()
	local elseifs = table()
	local elsestmt
	for _,stmt in ipairs{...} do
		if stmt.type == 'elseif' then
			elseifs:insert(stmt)
		elseif stmt.type == 'else' then
			assert(not elsestmt)
			elsestmt = stmt -- and remove
		else
			if laststmt then
				assert(laststmt.type ~= 'elseif' and laststmt.type ~= 'else', "got a bad stmt in an if after an else "..laststmt.type)
			end
			stmts:insert(stmt)
		end
		laststmt = stmt
	end
	self.cond = cond
	self.stmts = stmts
	self.elseifs = elseifs
	self.elsestmt = elsestmt
end
function _if:__tostring()
	local s = 'if '..tostring(self.cond)..' then '..spacesep(self.stmts)
	for _,ei in ipairs(self.elseifs) do
		s = s .. tostring(ei)
	end
	if self.elsestmt then s = s .. tostring(self.elsestmt) end
	s = s .. ' end'
	return s
end

-- aux for _if
_elseif = class{type='elseif'}
function _elseif:init(cond,...)
	self.cond = cond
	self.stmts={...}
end
function _elseif:__tostring()
	return 'elseif '..tostring(self.cond)..' then '..spacesep(self.stmts)
end

-- aux for _if
_else = class{type='else'}
function _else:init(...)
	self.stmts={...}
end
function _else:__tostring()
	return 'else '..spacesep(self.stmts)
end

_foreq = class{type='foreq'}
-- step is optional
function _foreq:init(var,min,max,step,...)
	self.var=var
	self.min=min
	self.max=max
	self.step=step
	self.stmts={...}
end
function _foreq:__tostring()
	local s = 'for '..tostring(self.var)..' = '..tostring(self.min)..','..tostring(self.max)
	if self.step then s = s..','..tostring(self.step) end
	s = s .. ' do '..spacesep(self.stmts)..' end'
	return s
end

_forin = class{type='forin'}
function _forin:init(vars,func,...)
	self.vars=vars
	self.func=func
	self.stmts={...}
end
function _forin:__tostring()
	return 'for '..commasep(self.vars)..' in '..tostring(self.func)..' do '..spacesep(self.stmts)..' end'
end

_function = class{type='function'}
-- name is optional
function _function:init(name, args, ...)
	-- prep args...
	for i=1,#args do
		args[i].index = i
		args[i].param = true
	end
	self.name=name
	self.args=args
	self.stmts={...}
end
function _function:__tostring()
	local s = 'function '
	if self.name then s = s .. self.name end
	s = s .. '(' .. commasep(table.map(self.args, tostring))
		.. ') ' .. spacesep(self.stmts) .. ' end'
	return s
end

-- aux for _function
_arg = class{type='arg'}
function _arg:init(index)
	self.index = index
end
-- params need to know what function they're in
-- so they can reference the function's arg names
function _arg:__tostring()
	return 'arg'..self.index
end

_local = class{type='local'}
function _local:init(stmt)
	self.stmt=stmt
end
function _local:__tostring()
	return 'local '..tostring(self.stmt)
end

-- control

_return = class{type='return'}
function _return:init(...)
	self.exprs={...}
end
function _return:__tostring()
	return 'return '..commasep(self.exprs)
end

_break = class{type='break'}

-- TODO - don't allow 'func' to just be a string
-- don't allow strings for any of this, in fact
_call = class{type='call'}
function _call:init(func, ...)
	self.func=func
	self.args={...}
end
function _call:__tostring()
	return tostring(self.func)..'('..commasep(self.args)..')'
end

-- please don't change these
_nil = class{type='nil', const=true}
_true = class{type='boolean', const=true, value=true}
_false = class{type='boolean', const=true, value=false}

_number = class{type='number'}
function _number:init(value) self.value = value end
function _number:__tostring() return self.value end

_string = class{type='string'}
function _string:init(value) self.value = value end
function _string:__tostring() return ('%q'):format(self.value) end

_vararg = class{type='vararg'}
function _vararg:__tostring() return '...' end
_table = class{type='table'}

_var = class{type='var'}	-- variable, lhs of _assign's, similar to _arg
function _var:init(name)
	self.name = name
end
function _var:__tostring()
	return self.name
end

_par = class{type='parenthesis'}
function _par:init(expr)
	self.expr = expr
end
function _par:__tostring()
	return '('..tostring(self.expr)..')'
end

_index = class{type='index'}
function _index:init(expr,key)
	self.expr = expr
	self.key = key
end
function _index:__tostring()
-- TODO - if self.key is a string and has no funny chars the use a .$key instead of [$key]
	return tostring(self.expr)..'['..tostring(self.key)..']'
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
} do
	local name = info[1]
	local op = info[2]
	local cl = class{type='binop', op=op}
	_G['_'..name] = cl
	function cl:init(...)
		self.args = {...}
	end
	function cl:__tostring()
		return table.concat(
			table.map(self.args, tostring),
			' '..self.op..' ')
	end
end

for _,info in ipairs{
	{'unm','-'},
	{'not','not'},
	{'length','#'},
} do
	local name = info[1]
	local op = info[2]
	local cl = class{type='unop', op=op}
	_G['_'..name] = cl
	function cl:init(arg)
		self.arg = arg
	end
	function cl:__tostring()
		return self.op..' '..tostring(self.arg)
	end
end


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
	{'stmts', 'many'},
	{'elseifs', 'many'},
	{'elsestmt', 'many'},
	{'name', 'field'},
	{'index', 'field'},
	{'value', 'field'},
}

function exec(n)
	return assert(loadstring(tostring(n)))
end

--[[
I need to fix this up better to handle short-circuiting, replacing, removing, etc...
parent is the parent-first traversal method
child is the child-first traversal
return what value of the callbacks you want 
returning a new node at the parent callback will not traverse its subsequent new children added to the tree
--]]
function traverse(n, parent, child)
	if parent then
		local ret = parent(n)
		if ret ~= n then
			return ret
		end
	end
	if type(n) == 'table' then
		for _,field in ipairs(fields) do
			local name = field[1]
			local howmuch = field[2]
			if n[name] then
				if howmuch == 'one' then
					n[name] = traverse(n[name], parent, child)
				elseif howmuch == 'many' then
					local value = n[name]
					for i=#value,1,-1 do
						value[i] = traverse(value[i], parent, child)
					end
				elseif howmuch == 'field' then
				else
					error("unknown howmuch "..howmuch)
				end
			end
		end
	end
	if child then
		n = child(n)
	end
	return n
end

function copy(n)
	local newn = {}
	setmetatable(newn, getmetatable(n))
	for _,field in ipairs(fields) do
		local name = field[1]
		local howmuch = field[2]
		local value = n[name]
		if value then
			if howmuch == 'one' then
				if type(value) == 'table' then
					newn[name] = copy(value)
				else
					newn[name] = value
				end
			elseif howmuch == 'many' then
				local newmany = {}
				for k,v in ipairs(value) do
					if type(v) == 'table' then
						newmany[k] = copy(v)
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
function flatten(f, varmap)
	f = copy(f)
	traverse(f, function(n)
		if type(n) == 'table'
		and n.type == 'call'
		then
			local funcname = tostring(n.func)	-- in case it's a var ... ?
			assert(funcname, "can't flatten a function with anonymous calls")
			local f = varmap[funcname]
			if f
			and #f.stmts == 1
			and f.stmts[1].type == 'return'
			then
				local retexprs = {}
				for i,e in ipairs(f.stmts[1].exprs) do
					retexprs[i] = copy(e)
					traverse(retexprs[i], function(v)
						if type(v) == 'table'
						and v.type == 'arg'
						then
							return copy(n.args[i])
						end
					end)
					retexprs[i] = _par(retexprs[i])
				end
				return _block(unpack(retexprs))	-- TODO exprlist, and redo assign to be based on vars and exprs
			end
		end
		return n
	end)
	return f
end


--[[
function building and eventually reconstructing and inlining

TODO full-on AST:

f = ast._function(
		'vec3.add',
		{'a','b'},
		-- rest is stmts
		ast._return(
			ast._call('vec3',
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
	traverse(f, function(n)
		if n.type == 'add' then n.type = 'sub' end
	end
	
	-- then we do a tree-descent with replace rule:
	traverse(f, function(n)
		if n.type == 'param' and n.param == 2 then
			n.param = 1
		end			
	end)
	-- and that's how dot() becomes lenSq()
	
	-- inline a function
	traverse(f, function(n)
		if n.type == 'call' then
			local inline = clonetree(n.func.body)
			-- first scope/local declare args
			local vars = {}
			for i=#n.args,1,-1 do
				local arg = n.args[i]
				local v = var()
				table.insert(vars, v)
				table.insert(inline.stmts, 1, localassign(v, arg))			-- names are all anonymous, just specify the var and assignment value.	 local() for declaring vars only, assign() for assigning them only, and localassign() for both?  or should i nest the two cmds?
			end
			-- then convert the body
			traverse(inline, function(v)
				if v.type == 'param' then
					return vars[v.param]
				end
			end)
			return inline
		end
	end)
	
	--[=[
	make a list ... check it twice ...
	
	block		- statement block
		.stmts	- array of stmt objects
		.tostring = table.concat(stmts, ' ')
	
statements:
	assign		- assignment operation
		.vars	- array of var objects
		.exprs	- array of expressions
		.tostring = table.concat(map(vars, 'name'), ',')..'='..table.concat(exprs, ',')

	do			- do / end block wrapper
		.stmts	- array of stmt objects
		.tostring = 'do '..table.concat(stmts, ' ')..' end'
		
	while
		.cond	- condition expression
		.stmts	- statements to execute
		.tostring = 'while '..cond..' do '..table.concat(tostring, ' ')..' end'
		
	repeat
		.cond	- condition expression
		.stmts	- statements to execute
		.tostring = 'repeat '..table.concat(tostring, ' ')..' until '..cond
	
	-- this could be prettier if we just had 'else' as a var, and did a special-case reinterpret for else->if's 
	-- but it would also have more nodes...
	if
		.cond		- condition expression
		.stmts		- statements if this condition option works
		.elseifs	- array of 'elseif' condition options
			.cond	- condition expression
			.stmts	- statements if this condition option works
		.else		- statements to execute if all other condition options fail
		.tostring = 'if '..cond..' then '..table.concat(stmts, ' ')..
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
		-stmts: array of stmt objects
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
