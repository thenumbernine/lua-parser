local table = require 'ext.table'
local asserteq = require 'ext.assert'.eq
local assertne = require 'ext.assert'.ne
local assertlt = require 'ext.assert'.lt
local assertge = require 'ext.assert'.ge
local assertle = require 'ext.assert'.le
local assertindex = require 'ext.assert'.index
local Parser = require 'parser.base.parser'

local ast = require 'parser.lua.ast'

local LuaTokenizer = require 'parser.lua.tokenizer'

local LuaParser = Parser:subclass()

-- save the namespace here, for Parser:setData()
LuaParser.ast = ast

-- static function
function LuaParser.parse(...)
	return LuaParser(...).tree
end

function LuaParser:init(data, version, source)
	self.version = version or _VERSION:match'^Lua (.*)$'
	if data then
		self:setData(data, source)
	end
end

-- TODO I don't need all these, just :getloc()
function LuaParser:getloc()
	local loc = self.t:getloc()
	loc.source = self.source
	return loc
end

function LuaParser:setData(data, source)
	self.gotos = {}		-- keep track of all gotos
	self.labels = {}	-- keep track of all labels
	self.source = source
	self.blockStack = table()
	self.functionStack = table{'function-vararg'}

	LuaParser.super.setData(self, data)

	-- last verify that all gotos went to all labels
	for _,g in pairs(self.gotos) do
		assert(self.labels[g.name], "no visible label '"..g.name.."' for <goto> at line "..g.line)
	end
end

function LuaParser:buildTokenizer(data)
	return LuaTokenizer(data, self.version)
end

-- default entry point for parsing data sources
function LuaParser:parseTree()
	return self:chunk()
end

function LuaParser:chunk()
	local from = self:getloc()
	local stmts = table()
	repeat
		local stmt = self:stat()
		if not stmt then break end
		stmts:insert(stmt)
		if self.version == '5.1' then
			self:canbe(';', 'symbol')
		end
	until false
	local laststat = self:retstat()
	if laststat then
		stmts:insert(laststat)
		if self.version == '5.1' then
			self:canbe(';', 'symbol')
		end
	end
	return ast._block(table.unpack(stmts))
		:setspan{from = from, to = self:getloc()}
end

function LuaParser:block(blockName)
	if blockName then self.blockStack:insert(blockName) end
	local chunk = self:chunk()
	if blockName then asserteq(self.blockStack:remove(), blockName) end
	return chunk
end

function LuaParser:stat()
	if self.version >= '5.2' then
		repeat until not self:canbe(';', 'symbol')
	end
	local from = self:getloc()
	if self:canbe('local', 'keyword') then
		local ffrom = self:getloc()
		if self:canbe('function', 'keyword') then
			local name = self:mustbe(nil, 'name')
			return ast._local{
				self:makeFunction(name, table.unpack(assert(self:funcbody())))
					:setspan{from = ffrom , to = self:getloc()}
				}:setspan{from = from , to = self:getloc()}
		else
			local afrom = self:getloc()
			local namelist = assert(self:attnamelist())
			if self:canbe('=', 'symbol') then
				local explist = assert(self:explist())
				local assign = ast._assign(namelist, explist)
					:setspan{from = from, to = self:getloc()}
				return ast._local{assign}
					:setspan{from = from, to = self:getloc()}
			else
				return ast._local(namelist)
					:setspan{from = from, to = self:getloc()}
			end
		end
	elseif self:canbe('function', 'keyword') then
		local funcname = self:funcname()
		return self:makeFunction(funcname, table.unpack(assert(self:funcbody())))
			:setspan{from = from , to = self:getloc()}
	elseif self:canbe('for', 'keyword') then
		local namelist = assert(self:namelist())
		if self:canbe('=', 'symbol') then
			asserteq(#namelist, 1)
			local explist = assert(self:explist())
			assertge(#explist, 2)
			assertle(#explist, 3)
			self:mustbe('do', 'keyword')
			local block = assert(self:block'for =')
			self:mustbe('end', 'keyword')
			return ast._foreq(namelist[1], explist[1], explist[2], explist[3], table.unpack(block))
				:setspan{from = from, to = self:getloc()}
		elseif self:canbe('in', 'keyword') then
			local explist = assert(self:explist())
			self:mustbe('do', 'keyword')
			local block = assert(self:block'for in')
			self:mustbe('end', 'keyword')
			return ast._forin(namelist, explist, table.unpack(block))
				:setspan{from = from, to = self:getloc()}
		else
			error("'=' or 'in' expected")
		end
	elseif self:canbe('if', 'keyword') then
		local cond = assert(self:exp(), "unexpected symbol")
		self:mustbe('then', 'keyword')
		local block = self:block()
		local stmts = table(block)
		-- ...and add elseifs and else to this
		local efrom = self:getloc()
		while self:canbe('elseif', 'keyword') do
			local cond = assert(self:exp())
			self:mustbe('then', 'keyword')
			stmts:insert(
				ast._elseif(cond, table.unpack(assert(self:block())))
					:setspan{from = efrom, to = self:getloc()}
			)
			efrom = self:getloc()
		end
		if self:canbe('else', 'keyword') then
			stmts:insert(
				ast._else(table.unpack(assert(self:block())))
					:setspan{from = efrom, to = self:getloc()}
			)
		end
		self:mustbe('end', 'keyword')
		return ast._if(cond, table.unpack(stmts))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('repeat', 'keyword') then
		local block = assert(self:block'repeat')
		self:mustbe('until', 'keyword')
		return ast._repeat(assert(self:exp()), table.unpack(block))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('while', 'keyword') then
		local cond = assert(self:exp())
		self:mustbe('do', 'keyword')
		local block = assert(self:block'while')
		self:mustbe('end', 'keyword')
		return ast._while(cond, table.unpack(block))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('do', 'keyword') then
		local block = assert(self:block())
		self:mustbe('end', 'keyword')
		return ast._do(table.unpack(block))
			:setspan{from = from, to = self:getloc()}
	elseif self.version >= '5.2' then
		if self:canbe('goto', 'keyword') then
			local name = self:mustbe(nil, 'name')
			local g = ast._goto(name)
				:setspan{from = from, to = self:getloc()}
			g.line, g.col = self.t:getlinecol()
			self.gotos[name] = g
			return g
		-- lua5.2+ break is a statement, so you can have multiple breaks in a row with no syntax error
		elseif self:canbe('break', 'keyword') then
			return self:_break()
				:setspan{from = from, to = self:getloc()}
		elseif self:canbe('::', 'symbol') then
			local name = self:mustbe(nil, 'name')
			local l = ast._label(name)
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

	local prefixexp = self:prefixexp()
	if prefixexp then
		if prefixexp.type == 'call' then 	-- function call
			return prefixexp
		else	-- varlist assignment
			local vars = table{prefixexp}
			while self:canbe(',', 'symbol') do
				local var = assert(self:prefixexp())
				assertne(var.type, 'call', "syntax error")
				vars:insert(var)
			end
			self:mustbe('=', 'symbol')
			return ast._assign(vars, assert(self:explist()))
				:setspan{from = from, to = self:getloc()}
		end
	end
end
-- 'laststat' in 5.1, 'retstat' in 5.2+

function LuaParser:retstat()
	local from = self:getloc()
	-- lua5.2+ break is a statement, so you can have multiple breaks in a row with no syntax error
	-- that means only handle 'break' here in 5.1
	if self.version == '5.1' and self:canbe('break', 'keyword') then
		return self:_break()
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe('return', 'keyword') then
		local explist = self:explist() or {}
		if self.version >= '5.2' then
			self:canbe(';', 'symbol')
		end
		return ast._return(table.unpack(explist))
			:setspan{from = from, to = self:getloc()}
	end
end

-- verify we're in a loop, then return the break

function LuaParser:_break()
	local from = self:getloc()
	if not ({['while']=1, ['repeat']=1, ['for =']=1, ['for in']=1})[self.blockStack:last()] then
		error("break not inside loop")
	end
	return ast._break()
		:setspan{from = from, to = self:getloc()}
end


function LuaParser:funcname()
	if not self:canbe(nil, 'name') then return end
	local from = self:getloc()
	local name = ast._var(self.lasttoken)
		:setspan{from = from, to = self:getloc()}
	while self:canbe('.', 'symbol') do
		local sfrom = self.t:getloc()
		name = ast._index(
			name,
			ast._string(self:mustbe(nil, 'name'))
				:setspan{from = sfrom, to = self:getloc()}
		):setspan{from = from, to = self:getloc()}
	end
	if self:canbe(':', 'symbol') then
		name = ast._indexself(name, self:mustbe(nil, 'name'))
			:setspan{from = from, to = self:getloc()}
	end
	return name
end

function LuaParser:namelist()
	local from = self:getloc()
	local name = self:canbe(nil, 'name')
	if not name then return end
	local names = table{
		ast._var(name)
			:setspan{from = from, to = self:getloc()}
	}
	while self:canbe(',', 'symbol') do
		from = self:getloc()
		names:insert(
			ast._var((self:mustbe(nil, 'name')))
				:setspan{from = from, to = self:getloc()}
		)
	end
	return names
end
-- same as above but with optional attributes

function LuaParser:attnamelist()
	local from = self:getloc()
	local name = self:canbe(nil, 'name')
	if not name then return end
	local attrib = self:attrib()
	local names = table{
		ast._var(name, attrib)
			:setspan{from = from, to = self:getloc()}
	}
	while self:canbe(',', 'symbol') do
		from = self:getloc()
		local name = self:mustbe(nil, 'name')
		local attrib = self:attrib()
		names:insert(
			ast._var(name, attrib)
				:setspan{from = from, to = self:getloc()}
		)
	end
	return names
end

function LuaParser:attrib()
	if self.version < '5.4' then return end
	local attrib
	if self:canbe('<', 'symbol') then
		attrib = self:mustbe(nil, 'name')
		self:mustbe('>', 'symbol')
	end
	return attrib
end

function LuaParser:explist()
	local exp = self:exp()
	if not exp then return end
	local exps = table{exp}
	while self:canbe(',', 'symbol') do
		exps:insert(assert(self:exp()))
	end
	return exps
end

--[[
exp ::= nil | false | true | Numeral | LiteralString | `...` | function | prefixexp | tableconstructor | exp binop exp | unop exp
... splitting this into two ...
exp ::= [unop] subexp {binop [unop] subexp}
subexp ::= nil | false | true | Numeral | LiteralString | `...` | function | prefixexp | tableconstructor
--]]

function LuaParser:exp()
	return self:exp_or()
end

function LuaParser:exp_or()
	local a = self:exp_and()
	if not a then return end
	if self:canbe('or', 'keyword') then
		a = ast._or(a,assert(self:exp_or()))
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end

function LuaParser:exp_and()
	local a = self:exp_cmp()
	if not a then return end
	if self:canbe('and', 'keyword') then
		a = ast._and(a, assert(self:exp_and()))
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end

function LuaParser:exp_cmp()
	local a
	if self.version >= '5.3' then
		a = self:exp_bor()
	else
		a = self:exp_concat()
	end
	if not a then return end
	if self:canbe('<', 'symbol')
	or self:canbe('>', 'symbol')
	or self:canbe('<=', 'symbol')
	or self:canbe('>=', 'symbol')
	or self:canbe('~=', 'symbol')
	or self:canbe('==', 'symbol')
	then
		local classForSymbol = {
			['<'] = ast._lt,
			['>'] = ast._gt,
			['<='] = ast._le,
			['>='] = ast._ge,
			['~='] = ast._ne,
			['=='] = ast._eq,
		}
		a = assertindex(classForSymbol, self.lasttoken)(a, assert(self:exp_cmp()))
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end
-- BEGIN 5.3+ ONLY:

function LuaParser:exp_bor()
	local a = self:exp_bxor()
	if not a then return end
	if self:canbe('|', 'symbol') then
		a = ast._bor(a, assert(self:exp_bor()))
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end

function LuaParser:exp_bxor()
	local a = self:exp_band()
	if not a then return end
	if self:canbe('~', 'symbol') then
		a = ast._bxor(a, assert(self:exp_bxor()))
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end

function LuaParser:exp_band()
	local a = self:exp_shift()
	if not a then return end
	if self:canbe('&', 'symbol') then
		a = ast._band(a, assert(self:exp_band()))
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end

function LuaParser:exp_shift()
	local a = self:exp_concat()
	if not a then return end
	if self:canbe('<<', 'symbol')
	or self:canbe('>>', 'symbol')
	then
		local classForSymbol = {
			['<<'] = ast._shl,
			['>>'] = ast._shr,
		}
		local cl = assertindex(classForSymbol, self.lasttoken)
		local b = assert(self:exp_shift())
		a = cl(a, b)
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end
-- END 5.3+ ONLY:

function LuaParser:exp_concat()
	local a = self:exp_addsub()
	if not a then return end
	if self:canbe('..', 'symbol') then
		a = ast._concat(a, assert(self:exp_concat()))
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end

function LuaParser:exp_addsub()
	local a = self:exp_muldivmod()
	if not a then return end
	if self:canbe('+', 'symbol')
	or self:canbe('-', 'symbol')
	then
		local classForSymbol = {
			['+'] = ast._add,
			['-'] = ast._sub,
		}
		local cl = assertindex(classForSymbol, self.lasttoken)
		local b = assert(self:exp_addsub())
		a = cl(a, b)
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end

function LuaParser:exp_muldivmod()
	local a = self:exp_unary()
	if not a then return end
	if self:canbe('*', 'symbol')
	or self:canbe('/', 'symbol')
	or self:canbe('%', 'symbol')
	or (self.version >= '5.3' and self:canbe('//', 'symbol'))
	then
		local classForSymbol = {
			['*'] = ast._mul,
			['/'] = ast._div,
			['%'] = ast._mod,
			['//'] = ast._idiv,
		}
		a = assertindex(classForSymbol, self.lasttoken)(a, assert(self:exp_muldivmod()))
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end

function LuaParser:exp_unary()
	local from = self:getloc()
	if self:canbe('not', 'keyword') then
		return ast._not(assert(self:exp_unary()))
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe('#', 'symbol') then
		return ast._len(assert(self:exp_unary()))
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe('-', 'symbol') then
		return ast._unm(assert(self:exp_unary()))
			:setspan{from = from, to = self:getloc()}
	end
	if self.version >= '5.3' then
		if self:canbe('~', 'symbol') then
			return ast._bnot(assert(self:exp_unary()))
				:setspan{from = from, to = self:getloc()}
		end
	end
	return self:exp_pow()
end

function LuaParser:exp_pow()
	local a = self:subexp()
	if not a then return end
	if self:canbe('^', 'symbol') then
		a = ast._pow(a, assert(self:exp_unary()))
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end

function LuaParser:subexp()
	local tableconstructor = self:tableconstructor()
	if tableconstructor then return tableconstructor end

	local prefixexp = self:prefixexp()
	if prefixexp then return prefixexp end

	local functiondef = self:functiondef()
	if functiondef then return functiondef end

	local from = self:getloc()
	if self:canbe('...', 'symbol') then
		asserteq(self.functionStack:last(), 'function-vararg')
		return ast._vararg()
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe(nil, 'string') then
		return ast._string(self.lasttoken)
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe(nil, 'number') then
		return ast._number(self.lasttoken)
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe('true', 'keyword') then
		return ast._true()
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe('false', 'keyword') then
		return ast._false()
			:setspan{from = from, to = self:getloc()}
	end
	if self:canbe('nil', 'keyword') then
		return ast._nil()
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

function LuaParser:prefixexp()
	local prefixexp
	local from = self:getloc()

	if self:canbe('(', 'symbol') then
		local exp = assert(self:exp())
		self:mustbe(')', 'symbol')
		prefixexp = ast._par(exp)
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe(nil, 'name') then
		prefixexp = ast._var(self.lasttoken)
			:setspan{from = from, to = self:getloc()}
	else
		return
	end

	while true do
		if self:canbe('[', 'symbol') then
			prefixexp = ast._index(prefixexp, assert(self:exp()))
			self:mustbe(']', 'symbol')
			prefixexp:setspan{from = from, to = self:getloc()}
		elseif self:canbe('.', 'symbol') then
			local sfrom = self:getloc()
			prefixexp = ast._index(
				prefixexp,
				ast._string(self:mustbe(nil, 'name'))
					:setspan{from = sfrom, to = self:getloc()}
			)
			:setspan{from = from, to = self:getloc()}
		elseif self:canbe(':', 'symbol') then
			prefixexp = ast._indexself(
				prefixexp,
				self:mustbe(nil, 'name')
			):setspan{from = from, to = self:getloc()}
			local args = self:args()
			if not args then error"function arguments expected" end
			prefixexp = ast._call(prefixexp, table.unpack(args))
				:setspan{from = from, to = self:getloc()}
		else
			local args = self:args()
			if not args then break end

			prefixexp = ast._call(prefixexp, table.unpack(args))
				:setspan{from = from, to = self:getloc()}
		end
	end

	return prefixexp
end

-- returns nil on fail to match, like all functions
-- produces error on syntax error
-- returns a table of the args -- particularly an empty table if no args were found

function LuaParser:args()
	local from = self:getloc()
	if self:canbe(nil, 'string') then
		return {
			ast._string(self.lasttoken)
				:setspan{from = from, to = self:getloc()}
			}
	end

	local tableconstructor = self:tableconstructor()
	if tableconstructor then return {tableconstructor} end

	if self:canbe('(', 'symbol') then
		local explist = self:explist()
		self:mustbe(')', 'symbol')
		return explist or {}
	end
end
-- helper which also includes the line and col in the function object

function LuaParser:makeFunction(...)
	local f = ast._function(...) -- no :setspan(), this is done by the caller
	f.line, f.col = self.t:getlinecol()
	f.source = self.source
	return f
end
-- 'function' in the 5.1 syntax

function LuaParser:functiondef()
	local from = self:getloc()
	if not self:canbe('function', 'keyword') then return end
	return self:makeFunction(nil, table.unpack(assert(self:funcbody())))
		:setspan{from = from, to = self:getloc()}
end
-- returns a table of ... first element is a table of args, rest of elements are the body statements

function LuaParser:funcbody()
	if not self:canbe('(', 'symbol') then return end
	local args = self:parlist() or table()
	local lastArg = args:last()
	local functionType = lastArg and lastArg.type == 'vararg' and 'function-vararg' or 'function'
	self:mustbe(')', 'symbol')
	self.functionStack:insert(functionType)
	local block = self:block(functionType)
	asserteq(self.functionStack:remove(), functionType)
	self:mustbe('end', 'keyword')
	return table{args, table.unpack(block)}
end

function LuaParser:parlist()	-- matches namelist() with ... as a terminator
	local from = self:getloc()
	if self:canbe('...', 'symbol') then
		return table{
			ast._vararg()
				:setspan{from = from, to = self:getloc()}
		}
	end
	local name = self:canbe(nil, 'name')
	if not name then return end
	local names = table{
		ast._var(name)
			:setspan{from = from, to = self:getloc()}
	}
	while self:canbe(',', 'symbol') do
		from = self:getloc()
		if self:canbe('...', 'symbol') then
			names:insert(
				ast._vararg()
					:setspan{from = from, to = self:getloc()}
			)
			return names
		end
		names:insert(
			ast._var((self:mustbe(nil, 'name')))
				:setspan{from = from, to = self:getloc()}
		)
	end
	return names
end

function LuaParser:tableconstructor()
	local from = self:getloc()
	if not self:canbe('{', 'symbol') then return end
	local fields = self:fieldlist()
	self:mustbe('}', 'symbol')
	return ast._table(fields or {})
		:setspan{from = from, to = self:getloc()}
end

function LuaParser:fieldlist()
	local field = self:field()
	if not field then return end
	local fields = table{field}
	while self:fieldsep() do
		local field = self:field()
		if not field then break end
		fields:insert(field)
	end
	self:fieldsep()
	return fields
end

function LuaParser:field()
	local from = self:getloc()
	if self:canbe('[', 'symbol') then
		local keyexp = assert(self:exp())
		self:mustbe(']', 'symbol')
		self:mustbe('=', 'symbol')
		local valexp = self:exp()
		if not valexp then error("expected expression but found "..self.t.token) end
		return ast._assign({keyexp}, {valexp})
			:setspan{from = from, to = self:getloc()}
	end

	-- this will be Name or exp
	-- in the case that it is a Name then check for = exp
	local exp = self:exp()
	if not exp then return end

	if exp.type == 'var' and self:canbe('=', 'symbol') then
		return ast._assign(
			{
				ast._string(exp.name):setspan(exp.span)
			}, {
				assert(self:exp())
			}
		):setspan{from = from, to = self:getloc()}
	else
		return exp
	end
end

function LuaParser:fieldsep()
	return self:canbe(',', 'symbol') or self:canbe(';', 'symbol')
end

return LuaParser
