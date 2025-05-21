local table = require 'ext.table'
local assert = require 'ext.assert'
local Parser = require 'parser.base.parser'

local ast = require 'parser.lua.ast'

local LuaTokenizer = require 'parser.lua.tokenizer'

local LuaParser = Parser:subclass()

-- save the namespace here, for Parser:setData()
LuaParser.ast = ast

-- static function
function LuaParser.parse(data, source, ...)
	local parser = LuaParser(nil, nil, ...)
	local result = table.pack(parser:setData(data, source))
	if not result[1] then return result:unpack() end
	return parser.tree
end

-- TODO instead of version and useluajit, how about parseFlags, and enable/disable them depending on the version
function LuaParser:init(data, version, source, useluajit)
	self.version = version or _VERSION:match'^Lua (.*)$'
	if useluajit == nil then
		-- I could test for _G.jit's presence, but what if luajit is compiled with jit off but still has LL language feature on ...
		-- TODO unified load shim layer , esp for lua 5.1 ...
		-- TODO TODO if langfix's load has been replaced then this will segfault...
		-- we are detecting LL / ULL suffix, but using load to do so causes some recursion problems (since in some cases I've already overridden load() via ext.load and parser.load_xform ...)
		--local _load = loadstring or load
		--useluajit = _load'return 1LL'
		-- ... so instead, for now just assume jit's presence implies luajit implies LL / ULL for parsing
		useluajit = not not _G.jit
	end
	self.useluajit = not not useluajit

	-- TODO between this and parser.grammar, make a table-based way to specify the rules
	-- TODO TODO a token DAG from the grammar would be nice ...
	-- [[ what to name this ...
	self.parseExprPrecedenceRulesAndClassNames = table{
		{
			name = 'or',
			rules = {
				{token='or', className='_or'},
			},
		},
		{
			name = 'and',
			rules = {
				{token='and', className='_and'},
			},
		},
		{
			name = 'cmp',
			rules = {
				{token='<', className='_lt'},
				{token='>', className='_gt'},
				{token='<=', className='_le'},
				{token='>=', className='_ge'},
				{token='~=', className='_ne'},
				{token='==', className='_eq'},
			},
		},
	}:append(
		self.version < '5.3' and nil or table{
		{
			name = 'bor',
			rules = {
				{token='|', className='_bor'},
			},
		},
		{
			name = 'bxor',
			rules = {
				{token='~', className='_bxor'},
			},
		},
		{
			name = 'band',
			rules = {
				{token='&', className='_band'},
			},
		},
		{
			name = 'shift',
			rules = {
				{token='<<', className='_shl'},
				{token='>>', className='_shr'},
			},
		},
	}):append{
		{
			name = 'concat',
			rules = {
				{token='..', className='_concat'},
			},
		},
		{
			name = 'addsub',	-- arithmetic
			rules = {
				{token='+', className='_add'},
				{token='-', className='_sub'},
			},
		},
		{
			name = 'muldivmod',	-- geometric
			rules = {
				{token='*', className='_mul'},
				{token='/', className='_div'},
				{token='%', className='_mod'},
				-- if version < 5.3 then the // symbol won't be added to the tokenizer anyways...
				{token='//', className='_idiv'},
			},
		},
		{
			name = 'unary',
			unaryLHS = true,
			rules = {
				{token='not', className='_not'},
				{token='#', className='_len'},	-- only a 5.1+ token
				{token='-', className='_unm'},
				{token='~', className='_bnot'},	-- only a 5.3+ token
			},
		},
		{
			name = 'pow',
			rules = {
				{token='^', className='_pow', nextLevel='unary'},
			},
		},
	}
	--]]

	if data then
		-- can't return from init so gotta error ...
		assert(self:setData(data, source))
	end
end

function LuaParser:setData(data, source)
	self.gotos = {}		-- keep track of all gotos
	self.labels = {}	-- keep track of all labels
	self.blockStack = table()
	self.functionStack = table{'function-vararg'}

	local result = table.pack(LuaParser.super.setData(self, data))
	if not result[1] then
		return result:unpack()
	end

	-- last verify that all gotos went to all labels
	for _,g in pairs(self.gotos) do
		if not self.labels[g.name] then
			return false, "line "..g.span.to.line..": no visible label '"..g.name.."' for <goto>"
		end
	end
	return true
end

function LuaParser:buildTokenizer(data)
	return LuaTokenizer(data, self.version, self.useluajit)
end

-- default entry point for parsing data sources
function LuaParser:parseTree()
	return self:parse_chunk()
end

function LuaParser:parse_chunk()
	local from = self:getloc()
	local stmts = table()
	if self.version >= '5.2' or self.useluajit then
		-- preceding ;'s allowed
		while self:canbe(';', 'symbol') do end
	end
	repeat
		local stmt = self:parse_stat()
		if not stmt then break end
		stmts:insert(stmt)
		self:canbe(';', 'symbol')
	until false
	local laststat = self:parse_retstat()
	if laststat then
		stmts:insert(laststat)
		self:canbe(';', 'symbol')
	end
	return self:node('_block', table.unpack(stmts))
		:setspan{from = from, to = self:getloc()}
end

function LuaParser:parse_block(blockName)
	if blockName then self.blockStack:insert(blockName) end
	local chunk = self:parse_chunk()
	if blockName then assert.eq(self.blockStack:remove(), blockName) end
	return chunk
end

function LuaParser:parse_stat()
	local from = self:getloc()
	if self:canbe('local', 'keyword') then
		local ffrom = self:getloc()
		if self:canbe('function', 'keyword') then
			local namevar = self:parse_var()
			if not namevar then error{msg="expected name"} end
			return self:node('_local', {
				self:makeFunction(
					namevar,
					table.unpack((assert(self:parse_funcbody(), {msg="expected function body"})))
				):setspan{from = ffrom , to = self:getloc()}
			}):setspan{from = from , to = self:getloc()}
		else
			local afrom = self:getloc()
			local namelist = assert(self:parse_attnamelist(), {msg="expected attr name list"})
			if self:canbe('=', 'symbol') then
				local explist = assert(self:parse_explist(), {msg="expected expr list"})
				local assign = self:node('_assign', namelist, explist)
					:setspan{from = ffrom, to = self:getloc()}
				return self:node('_local', {assign})
					:setspan{from = from, to = self:getloc()}
			else
				return self:node('_local', namelist)
					:setspan{from = from, to = self:getloc()}
			end
		end
	elseif self:canbe('function', 'keyword') then
		local funcname = self:parse_funcname()
		return self:makeFunction(funcname, table.unpack((assert(self:parse_funcbody(), {msg="expected function body"}))))
			:setspan{from = from , to = self:getloc()}
	elseif self:canbe('for', 'keyword') then
		local namelist = assert(self:parse_namelist(), {msg="expected name list"})
		if self:canbe('=', 'symbol') then
			assert.eq(#namelist, 1, {msg="expected only one name in for loop"})
			local explist = assert(self:parse_explist(), {msg="expected exp list"})
			assert.ge(#explist, 2, {msg="bad for loop"})
			assert.le(#explist, 3, {msg="bad for loop"})
			local doloc = self:getloc()
			self:mustbe('do', 'keyword')
			local block = assert(self:parse_block'for =', {msg="for loop expected block"})
			self:mustbe('end', 'keyword', 'do', doloc)
			return self:node('_foreq', namelist[1], explist[1], explist[2], explist[3], table.unpack(block))
				:setspan{from = from, to = self:getloc()}
		elseif self:canbe('in', 'keyword') then
			local explist = assert(self:parse_explist(), {msg="expected expr list"})
			local doloc = self:getloc()
			self:mustbe('do', 'keyword')
			local block = assert(self:parse_block'for in', {msg="expected block"})
			self:mustbe('end', 'keyword', 'do', doloc)
			return self:node('_forin', namelist, explist, table.unpack(block))
				:setspan{from = from, to = self:getloc()}
		else
			error{msg="'=' or 'in' expected"}
		end
	elseif self:canbe('if', 'keyword') then
		local cond = assert(self:parse_exp(), {msg="unexpected symbol"})
		self:mustbe('then', 'keyword')
		local block = self:parse_block()
		local stmts = table(block)
		-- ...and add elseifs and else to this
		local efrom = self:getloc()
		while self:canbe('elseif', 'keyword') do
			local cond = assert(self:parse_exp(), {msg='unexpected symbol'})
			self:mustbe('then', 'keyword')
			stmts:insert(
				self:node('_elseif', cond, table.unpack((assert(self:parse_block(), {msg='expected block'}))))
					:setspan{from = efrom, to = self:getloc()}
			)
			efrom = self:getloc()
		end
		if self:canbe('else', 'keyword') then
			stmts:insert(
				self:node('_else', table.unpack((assert(self:parse_block(), {msg='expected block'}))))
					:setspan{from = efrom, to = self:getloc()}
			)
		end
		self:mustbe('end', 'keyword', 'if', from)
		return self:node('_if', cond, table.unpack(stmts))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('repeat', 'keyword') then
		local block = assert(self:parse_block'repeat', {msg='expected block'})
		self:mustbe('until', 'keyword')
		return self:node(
			'_repeat',
			(assert(self:parse_exp(), {msg='unexpected symbol'})),
			table.unpack(block)
		):setspan{from = from, to = self:getloc()}
	elseif self:canbe('while', 'keyword') then
		local cond = assert(self:parse_exp(), {msg='unexpected symbol'})
		local doloc = self:getloc()
		self:mustbe('do', 'keyword')
		local block = assert(self:parse_block'while', {msg='expected block'})
		self:mustbe('end', 'keyword', 'do', doloc)
		return self:node('_while', cond, table.unpack(block))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('do', 'keyword') then
		local block = assert(self:parse_block(), {msg='expected block'})
		self:mustbe('end', 'keyword', 'do', from)
		return self:node('_do', table.unpack(block))
			:setspan{from = from, to = self:getloc()}
	elseif self.version >= '5.2' then
		if self:canbe('goto', 'keyword') then
			local name = self:mustbe(nil, 'name')
			local g = self:node('_goto', name)
				:setspan{from = from, to = self:getloc()}
			self.gotos[name] = g
			return g
		-- lua5.2+ break is a statement, so you can have multiple breaks in a row with no syntax error
		elseif self:canbe('break', 'keyword') then
			return self:parse_break()
				:setspan{from = from, to = self:getloc()}
		elseif self:canbe('::', 'symbol') then
			local name = self:mustbe(nil, 'name')
			local l = self:node('_label', name)
			self.labels[name] = true
			self:mustbe('::', 'symbol')
			return l:setspan{from = from, to = self:getloc()}
		end
	end

	-- now we handle functioncall and varlist = explist rules

	--[[
	stat ::= varlist `=` explist | functioncall
	varlist ::= var {`,` var}
	var ::= Name | prefixexp `[` exp `]` | prefixexp `.` Name
	prefixexp ::= var | functioncall | `(` exp `)`
	functioncall ::= prefixexp args | prefixexp `:` Name args
		right now prefixexp is designed to process trailing args ...
		... so just use it and complain if the wrapping ast is not a _call
	likewise with var, complain if it is a call
	--]]

	local prefixexp = self:parse_prefixexp()
	if prefixexp then
		if self.ast._call:isa(prefixexp) then 	-- function call
			return prefixexp
		else	-- varlist assignment
			local vars = table{prefixexp}
			while self:canbe(',', 'symbol') do
				local var = assert(self:parse_prefixexp(), {msg='expected expr'})
				assert.ne(var.type, 'call', {msg="syntax error"})
				vars:insert(var)
			end
			return self:parse_assign(vars, from)
		end
	end
end

function LuaParser:parse_assign(vars, from)
	self:mustbe('=', 'symbol')
	return self:node('_assign', vars, (assert(self:parse_explist(), {msg='expected expr'})))
		:setspan{from = from, to = self:getloc()}
end

-- 'laststat' in 5.1, 'retstat' in 5.2+
function LuaParser:parse_retstat()
	local from = self:getloc()
	-- lua5.2+ break is a statement, so you can have multiple breaks in a row with no syntax error
	-- that means only handle 'break' here in 5.1
	if self.version <= '5.1' and self:canbe('break', 'keyword') then
		return self:parse_break()
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe('return', 'keyword') then
		local explist = self:parse_explist() or {}
		return self:node('_return', table.unpack(explist))
			:setspan{from = from, to = self:getloc()}
	end
end

-- verify we're in a loop, then return the break

function LuaParser:parse_break()
	local from = self:getloc()
	if not ({['while']=1, ['repeat']=1, ['for =']=1, ['for in']=1})[self.blockStack:last()] then
		error{msg="break not inside loop"}
	end
	return self:node('_break')
		:setspan{from = from, to = self:getloc()}
end


function LuaParser:parse_funcname()
	local from = self:getloc()
	local name = self:parse_var()
	if not name then return end
	while self:canbe('.', 'symbol') do
		local sfrom = self.t:getloc()
		name = self:node('_index',
			name,
			self:node('_string', self:mustbe(nil, 'name'))
				:setspan{from = sfrom, to = self:getloc()}
		):setspan{from = from, to = self:getloc()}
	end
	if self:canbe(':', 'symbol') then
		name = self:node('_indexself', name, self:mustbe(nil, 'name'))
			:setspan{from = from, to = self:getloc()}
	end
	return name
end

-- parses a varialbe name, without attribs, and returns it in a '_var' node
function LuaParser:parse_var()
	local from = self:getloc()
	local name = self:canbe(nil, 'name')
	if not name then return end
	return self:node('_var', name)
		:setspan{from=from, to=self:getloc()}
end

function LuaParser:parse_namelist()
	local var = self:parse_var()
	if not var then return end
	local names = table{var}
	while self:canbe(',', 'symbol') do
		names:insert((assert(self:parse_var(), {msg="expected name"})))
	end
	return names
end

-- same as above but with optional attributes

function LuaParser:parse_attnamelist()
	local from = self:getloc()
	local name = self:canbe(nil, 'name')
	if not name then return end
	local attrib = self:parse_attrib()
	local names = table{
		self:node('_var', name, attrib)
			:setspan{from = from, to = self:getloc()}
	}
	while self:canbe(',', 'symbol') do
		from = self:getloc()
		local name = self:mustbe(nil, 'name')
		local attrib = self:parse_attrib()
		names:insert(
			self:node('_var', name, attrib)
				:setspan{from = from, to = self:getloc()}
		)
	end
	return names
end

function LuaParser:parse_attrib()
	if self.version < '5.4' then return end
	local attrib
	if self:canbe('<', 'symbol') then
		attrib = self:mustbe(nil, 'name')
		self:mustbe('>', 'symbol')
	end
	return attrib
end

function LuaParser:parse_explist()
	local exp = self:parse_exp()
	if not exp then return end
	local exps = table{exp}
	while self:canbe(',', 'symbol') do
		exps:insert((assert(self:parse_exp(), {msg='unexpected symbol'})))
	end
	return exps
end

--[[
exp ::= nil | false | true | Numeral | LiteralString | `...` | function | prefixexp | tableconstructor | exp binop exp | unop exp
... splitting this into two ...
exp ::= [unop] subexp {binop [unop] subexp}
subexp ::= nil | false | true | Numeral | LiteralString | `...` | function | prefixexp | tableconstructor
--]]

function LuaParser:parse_exp()
	return self:parse_expr_precedenceTable(1)
end

function LuaParser:parse_subexp()
	local tableconstructor = self:parse_tableconstructor()
	if tableconstructor then return tableconstructor end

	local prefixexp = self:parse_prefixexp()
	if prefixexp then return prefixexp end

	local functiondef = self:parse_functiondef()
	if functiondef then return functiondef end

	local from = self:getloc()
	if self:canbe('...', 'symbol') then
		if self.version == '5.0' then error{msg="unexpected symbol near '...'"} end
		assert.eq(self.functionStack:last(), 'function-vararg', {msg='unexpected symbol'})
		return self:node('_vararg')
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe(nil, 'string') then
		return self:node('_string', self.lasttoken)
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe(nil, 'number') then
		return self:node('_number', self.lasttoken)
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe('true', 'keyword') then
		return self:node('_true')
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe('false', 'keyword') then
		return self:node('_false')
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe('nil', 'keyword') then
		return self:node('_nil')
			:setspan{from = from, to = self:getloc()}
	end
end

--[[
prefixexp ::= var | functioncall | `(` exp `)`

functioncall ::= prefixexp args | prefixexp `:` Name args
combine...
prefixexp ::= var | prefixexp args | prefixexp `:` Name args | `(` exp `)`
var ::= Name | prefixexp `[` exp `]` | prefixexp `.` Name
combine ...
prefixexp ::= Name | prefixexp `[` exp `]` | prefixexp `.` Name | prefixexp args | prefixexp `:` Name args | `(` exp `)`
simplify ...
prefixexp ::= (Name {'[' exp ']' | `.` Name | [`:` Name] args} | `(` exp `)`) {args}
--]]

function LuaParser:parse_prefixexp()
	local prefixexp
	local from = self:getloc()

	if self:canbe('(', 'symbol') then
		local exp = assert(self:parse_exp(), {msg='unexpected symbol'})
		self:mustbe(')', 'symbol')
		prefixexp = self:node('_par', exp)
			:setspan{from = from, to = self:getloc()}
	else
		prefixexp = self:parse_var()
		if not prefixexp then return end
	end

	while true do
		if self:canbe('[', 'symbol') then
			prefixexp = self:node('_index', prefixexp, (assert(self:parse_exp(), {msg='unexpected symbol'})))
			self:mustbe(']', 'symbol')
			prefixexp:setspan{from = from, to = self:getloc()}
		elseif self:canbe('.', 'symbol') then
			local sfrom = self:getloc()
			prefixexp = self:node('_index',
				prefixexp,
				self:node('_string', self:mustbe(nil, 'name'))
					:setspan{from = sfrom, to = self:getloc()}
			)
			:setspan{from = from, to = self:getloc()}
		elseif self:canbe(':', 'symbol') then
			prefixexp = self:node('_indexself',
				prefixexp,
				self:mustbe(nil, 'name')
			):setspan{from = from, to = self:getloc()}
			local args = self:parse_args()
			if not args then error{msg="function arguments expected"} end
			prefixexp = self:node('_call', prefixexp, table.unpack(args))
				:setspan{from = from, to = self:getloc()}
		else
			local args = self:parse_args()
			if not args then break end

			prefixexp = self:node('_call', prefixexp, table.unpack(args))
				:setspan{from = from, to = self:getloc()}
		end
	end

	return prefixexp
end

-- returns nil on fail to match, like all functions
-- produces error on syntax error
-- returns a table of the args -- particularly an empty table if no args were found

function LuaParser:parse_args()
	local from = self:getloc()
	if self:canbe(nil, 'string') then
		return {
			self:node('_string', self.lasttoken)
				:setspan{from = from, to = self:getloc()}
		}
	end

	local tableconstructor = self:parse_tableconstructor()
	if tableconstructor then return {tableconstructor} end

	if self:canbe('(', 'symbol') then
		local explist = self:parse_explist()
		self:mustbe(')', 'symbol')
		return explist or {}
	end
end
-- helper which also includes the line and col in the function object

function LuaParser:makeFunction(...)
	return self:node('_function', ...) -- no :setspan(), this is done by the caller
end
-- 'function' in the 5.1 syntax

function LuaParser:parse_functiondef()
	local from = self:getloc()
	if not self:canbe('function', 'keyword') then return end
	return self:makeFunction(nil, table.unpack((assert(self:parse_funcbody(), {msg='expected function body'}))))
		:setspan{from = from, to = self:getloc()}
end
-- returns a table of ... first element is a table of args, rest of elements are the body statements

function LuaParser:parse_funcbody()
	local funcloc = self:getloc()
	if not self:canbe('(', 'symbol') then return end
	local args = self:parse_parlist() or table()
	local lastArg = args:last()
	local functionType = self.ast._vararg:isa(lastArg) and 'function-vararg' or 'function'
	self:mustbe(')', 'symbol')
	self.functionStack:insert(functionType)
	local block = self:parse_block(functionType)
	assert.eq(self.functionStack:remove(), functionType)
	self:mustbe('end', 'keyword', 'function', funcloc)
	return table{args, table.unpack(block)}
end

function LuaParser:parse_parlist()	-- matches namelist() with ... as a terminator
	local from = self:getloc()
	if self:canbe('...', 'symbol') then
		return table{
			self:node('_vararg')
				:setspan{from = from, to = self:getloc()}
		}
	end

	local namevar = self:parse_var()
	if not namevar then return end
	local names = table{namevar}
	while self:canbe(',', 'symbol') do
		from = self:getloc()
		if self:canbe('...', 'symbol') then
			names:insert(
				self:node('_vararg')
					:setspan{from = from, to = self:getloc()}
			)
			return names
		end
		local namevar = self:parse_var()
		if not namevar then error{msg="expected name"} end
		names:insert(namevar)
	end
	return names
end

function LuaParser:parse_tableconstructor()
	local from = self:getloc()
	if not self:canbe('{', 'symbol') then return end
	if self.version == '5.0' then
		-- despite what the 5.0 syntax says, it looks like the 5.0 parser will parse and ignore a leading semicolon as valid: {; 1, 2, 3, 4}
		self:canbe(';', 'symbol')
	end
	local fields = self:parse_fieldlist()
	self:mustbe('}', 'symbol')
	--[[ ok design flaw I didn't foresee when trying to unify all the AST as indexed children (like my symmath project)
	-- if this _table's children are too big then you can't unpack it into the ctor args...
	local result = self:node('_table', table.unpack(fields or {}))
	--]]
	-- [[ ... so instead, manually insert them...
	-- but a later TODO might be to go back to accepting a table-of-children.
	local result = self:node'_table'
	if fields then
		for i,field in ipairs(fields) do
			result[i] = field
		end
	end
	--]]
	result:setspan{from = from, to = self:getloc()}
	return result
end

function LuaParser:parse_fieldlist()
	local field = self:parse_field()
	if not field then return end
	local fields = table{field}
	while self:parse_fieldsep() do
		local field = self:parse_field()
		if not field then break end
		fields:insert(field)
	end
	self:parse_fieldsep()
	return fields
end

function LuaParser:parse_field()
	local from = self:getloc()
	if self:canbe('[', 'symbol') then
		local keyexp = assert(self:parse_exp(), {msg='unexpected symbol'})
		self:mustbe(']', 'symbol')
		self:mustbe('=', 'symbol')
		local valexp = self:parse_exp()
		if not valexp then error{msg="expected expression but found "..tostring(self.t.token)} end
		return self:node('_assign', {keyexp}, {valexp})
			:setspan{from = from, to = self:getloc()}
	end

	-- this will be Name or exp
	-- in the case that it is a Name then check for = exp
	local exp = self:parse_exp()
	if not exp then return end

	if self.ast._var:isa(exp) and self:canbe('=', 'symbol') then
		return self:node('_assign',
			{
				self:node('_string', exp.name):setspan(exp.span)
			}, {
				(assert(self:parse_exp(), {msg='unexpected symbol'}))
			}
		):setspan{from = from, to = self:getloc()}
	else
		return exp
	end
end

function LuaParser:parse_fieldsep()
	return self:canbe(',', 'symbol') or self:canbe(';', 'symbol')
end

return LuaParser
