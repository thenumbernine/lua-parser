local table = require 'ext.table'
local class = require 'ext.class'
local string = require 'ext.string'

local ast = require 'parser.ast'

local DataReader = class()
function DataReader:init(data)
	self.data = data
	self.index = 1
	self.line = 1
	self.col = 1
	-- skip past initial #'s
	if self.data:sub(1,1) == '#' then
		self.index = self.data:find('\n')+1
		self.line = self.line + 1
	end
end
function DataReader:done()
	return self.index > #self.data
end
function DataReader:seekto(pattern)
	local from, to = self.data:find(pattern, self.index)
	if not from then
		from = #self.data+1
		to = from
	end
	local skipped = self.data:sub(self.index, from - 1)
	self.index = from
	self.lasttoken = self.data:sub(from, to)
	local newlines = #skipped:gsub('[^\n]','')
	if newlines > 0 then
		self.line = self.line + newlines
		self.col = #skipped:match('[^\n]*$') + 1
	else
		self.col = self.col + #skipped
	end
	return self.lasttoken
end
function DataReader:seekpast(pattern)
	local from, to = self.data:find(pattern, self.index)
	if not from then return end
	local skipped = self.data:sub(self.index, to)
	self.index = to + 1
	self.lasttoken = self.data:sub(from, to)
	local newlines = #skipped:gsub('[^\n]','')
	if newlines > 0 then
		self.line = self.line + newlines
		self.col = #skipped:match('[^\n]*$') + 1
	else
		self.col = self.col + #skipped
	end
	return self.lasttoken
end
function DataReader:canbe(pattern)
	return self:seekpast('^'..pattern)
end
function DataReader:mustbe(pattern, msg)
	if not self:canbe(pattern) then error(msg or "expected "..pattern) end
	return self.lasttoken
end
function DataReader:readblock()
	if not self:canbe('%[=*%[') then return end
	local eq = assert(self.lasttoken:match('^%[(=*)%[$'))
	local start = self.index
	if not self:seekpast('%]'..eq..'%]') then
		error("expected closing block")
	end
	self.lasttoken = self.data:sub(start, self.index - #self.lasttoken - 1)
	return self.lasttoken
end

local Tokenizer = class()

Tokenizer.symbols = table()
for w in ([[... .. == ~= <= >= + - * / % ^ # < > = ( ) { } [ ] ; : , .]]):gmatch('%S+') do
	Tokenizer.symbols:insert(w)
end

Tokenizer.keywords = {}
for w in ([[and break do else elseif end false for function if in local nil not or repeat return then true until while]]):gmatch('%S+') do
	Tokenizer.keywords[w] = true
end

function Tokenizer:init(data, version)
	self.version = assert(version)
	if self.version >= '5.2' then
		self.symbols:insert'::'	-- for labels .. make sure you insert it before ::
		self.keywords['goto'] = true
	end
	if self.version >= '5.2' then
		self.symbols:insert'//'
		self.symbols:insert'~'
		self.symbols:insert'&'
		self.symbols:insert'|'
		self.symbols:insert'<<'
		self.symbols:insert'>>'
	end
	self.r = DataReader(data)
	self.gettokenthread = coroutine.create(function()
		local r = self.r

		while not r:done() do
			-- whitespace
			while r:canbe'%s+' do
--print('read space ['..(r.index-#r.lasttoken)..','..r.index..']: '..r.lasttoken)
			end
			if r:done() then break end
			-- comment
			if r:canbe'%-%-' then
local start = r.index - #r.lasttoken
				-- read block comment if it exists
				if not r:readblock() then
					-- read line otherwise
					r:seekto'\n'
				end
				local commentstr = r.data:sub(start, r.index-1)
				-- TODO how to insert comments into the AST?  should they be their own nodes?
				-- should all whitespace be its own node, so the original code text can be reconstructed exactly?
				--coroutine.yield(commentstr, 'comment')

--print('read comment ['..start..','..(r.index-1)..']:'..commentstr)

			-- block string
			elseif r:readblock() then
--print('read multi-line string ['..(r.index-#r.lasttoken)..','..r.index..']: '..r.lasttoken)
				coroutine.yield(r.lasttoken, 'string')
			-- other string
			elseif r:canbe'["\']' then
--print('read quote string ['..(r.index-#r.lasttoken)..','..r.index..']: '..r.lasttoken)
local start = r.index-#r.lasttoken
				local quote = r.lasttoken
				local s = table()
				while true do
					r:seekpast'.'
					if r.lasttoken == quote then break end
					if r:done() then error("unfinished string") end
					if r.lasttoken == '\\' then
						local esc = r:canbe'.'
						local escapeCodes = {a='\a', b='\b', f='\f', n='\n', r='\r', t='\t', v='\v', ['\\']='\\', ['"']='"', ["'"]="'", ['0']='\0'}
						local escapeCode = escapeCodes[esc]
						if escapeCode then
							s:insert(escapeCode)
						elseif esc:match('%d') then
							-- can read up to three
							if r:canbe'%d' then esc = esc .. r.lasttoken end
							if r:canbe'%d' then esc = esc .. r.lasttoken end
							s:insert(string.char(tonumber(esc)))
						end
					else
						s:insert(r.lasttoken)
					end
				end
--print('read quote string ['..start..','..(r.index-#r.lasttoken)..']: '..r.data:sub(start, r.index-#r.lasttoken))
				coroutine.yield(s:concat(), 'string')
			elseif r:canbe'[%a_][%w_]*' then	-- name
--print('read name ['..(r.index-#r.lasttoken)..', '..r.index..']: '..r.lasttoken)
				coroutine.yield(r.lasttoken, self.keywords[r.lasttoken] and 'keyword' or 'name')
			elseif r.data:match('^[%.%d]', r.index) -- if it's a decimal or a number...
			and (r.data:match('^%d', r.index)	-- then, if it's a number it's good
			or r.data:match('^%.%d', r.index))	-- or if it's a decimal then if it has a number following it then it's good ...
			then 								-- otherwise I want it to continue to the next 'else'
				-- lua doesn't consider the - to be a part of the number literal
				-- instead, it parses it as a unary - and then possibly optimizes it into the literal during ast optimization
--local start = r.index
				if r:canbe'0[xX]' then

					-- if version is 5.2 then allow decimals in hex #'s, and use 'p's instead of 'e's for exponents
					if self.version >= '5.2' then
						-- TODO this looks like the float-parse code below (but with e+- <-> p+-) but meh I'm lazy so I just copied it.
						local token = r:canbe'[%.%da-fA-F]+'
						assert(#token:gsub('[^%.]','') < 2, 'malformed number')
						local n = table{'0x', token}
						if r:canbe'p' then
							n:insert(r.lasttoken)
							-- fun fact, while the hex float can include hex digits, its 'p+-' exponent must be in decimal.
							n:insert(r:mustbe('[%+%-]%d+', 'malformed number'))
						end
						coroutine.yield(n:concat(), 'number')
					else
						local token = r:canbe'[%da-fA-F]+'
						assert(token, 'malformed number')
						coroutine.yield('0x'..token, 'number')
					end
				else
					local token = r:canbe'[%.%d]+'
					assert(#token:gsub('[^%.]','') < 2, 'malformed number')
					local n = table{token}
					if r:canbe'e' then
						n:insert(r.lasttoken)
						n:insert(r:mustbe('[%+%-]%d+', 'malformed number'))
					end
					coroutine.yield(n:concat(), 'number')
				end
--print('read number ['..start..', '..r.index..']: '..r.data:sub(start, r.index-1))
			else
				-- see if it matches any symbols
				local found = false
				for _,symbol in ipairs(self.symbols) do
					if r:canbe(string.patescape(symbol)) then
--print('read symbol ['..(r.index-#r.lasttoken)..','..r.index..']: '..r.lasttoken)
						coroutine.yield(r.lasttoken, 'symbol')
						found = true
						break
					end
				end
				if not found then
					error("unknown symbol "..r.data:sub(r.index))
				end
			end
		end
	end)
end
-- separate this in case someone has to modify the tokenizer symbols and keywords before starting
function Tokenizer:start()
	-- arrange symbols from largest to smallest
	self.symbols:sort(function(a,b) return #a > #b end)
	self:consume()
	self:consume()
end
function Tokenizer:consume()
	self.token = self.nexttoken
	self.tokentype = self.nexttokentype
	if coroutine.status(self.gettokenthread) == 'dead' then
		self.nexttoken = nil
		self.nexttokentype = nil
		-- done = true
		return
	end
	local status, nexttoken, nexttokentype = coroutine.resume(self.gettokenthread)
	-- detect errors
	if not status then
		error(nexttoken..'\n'
			..debug.traceback(self.gettokenthread))
	end
	self.nexttoken = nexttoken
	self.nexttokentype = nexttokentype
end
function Tokenizer:getlinecol()
	return self.r.line, self.r.col
end
-- TODO a more efficient way of doing this
function Tokenizer:getpos()
	local sofar = self.r.data:sub(1,self.r.index)
	local lastline = sofar:match('[^\n]*$') or ''
	return 'line '..self.r.line
		..' col '..self.r.col
		..' code "'..lastline..'"'
end

local Parser = class()

-- static function
function Parser.parse(...)
	return Parser(...).tree
end

function Parser:init(data, version, source)
	self.version = version or _VERSION:match'^Lua (.*)$'
	if data then
		self:setData(data, source)
	end
end
function Parser:setData(data, source)
	assert(data, "expected data")
	data = tostring(data)
	self.gotos = {}		-- keep track of all gotos
	self.labels = {}	-- keep track of all labels
	local t = Tokenizer(data, self.version)
	t:start()
	self.t = t
	self.source = source

	self.blockStack = table()
	self.functionStack = table{'function-vararg'}

	self.tree = self:chunk()

	-- last verify that all gotos went to all labels
	for _,g in pairs(self.gotos) do
		assert(self.labels[g.name], "no visible label '"..g.name.."' for <goto> at line "..g.line)
	end

	-- now that we have the tree, build parents
	-- ... since I don't do that during construction ...
	ast.refreshparents(self.tree)

	if self.t.token then
		error("unexpected "..self.t.token)
	end
end
function Parser:canbe(token, tokentype)	-- token is optional
	assert(tokentype)
	if (not token or token == self.t.token)
	and tokentype == self.t.tokentype
	then
		self.lasttoken, self.lasttokentype = self.t.token, self.t.tokentype
		self.t:consume()
		return self.lasttoken, self.lasttokentype
	end
end
function Parser:mustbe(token, tokentype)
	local lasttoken, lasttokentype = self.t.token, self.t.tokentype
	self.lasttoken, self.lasttokentype = self:canbe(token, tokentype)
	if not self.lasttoken then
		error("expected token='"..token.."' tokentype="..tokentype.." but found token='"..tostring(lasttoken).."' type="..tostring(lasttokentype))
	end
	return self.lasttoken, self.lasttokentype
end
function Parser:chunk()
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
end
function Parser:block(blockName)
	if blockName then self.blockStack:insert(blockName) end
	local chunk = self:chunk()
	if blockName then assert(self.blockStack:remove() == blockName) end
	return chunk
end
function Parser:stat()
	if self.version >= '5.2' then
		repeat until not self:canbe(';', 'symbol')
	end
	if self:canbe('local', 'keyword') then
		if self:canbe('function', 'keyword') then
			local name = self:mustbe(nil, 'name')
			return ast._local{self:makeFunction(name, table.unpack(assert(self:funcbody())))}
		else
			local namelist = assert(self:attnamelist())
			if self:canbe('=', 'symbol') then
				local explist = assert(self:explist())
				local assign = ast._assign(namelist, explist)
				return ast._local{assign}
			else
				return ast._local(namelist)
			end
		end
	elseif self:canbe('function', 'keyword') then
		local funcname = self:funcname()
		return self:makeFunction(funcname, table.unpack(assert(self:funcbody())))
	elseif self:canbe('for', 'keyword') then
		local namelist = assert(self:namelist())
		if self:canbe('=', 'symbol') then
			assert(#namelist == 1)
			local explist = assert(self:explist())
			assert(#explist >= 2 and #explist <= 3)
			self:mustbe('do', 'keyword')
			local block = assert(self:block'for =')
			self:mustbe('end', 'keyword')
			return ast._foreq(namelist[1], explist[1], explist[2], explist[3], table.unpack(block))
		elseif self:canbe('in', 'keyword') then
			local explist = assert(self:explist())
			self:mustbe('do', 'keyword')
			local block = assert(self:block'for in')
			self:mustbe('end', 'keyword')
			return ast._forin(namelist, explist, table.unpack(block))
		else
			error("'=' or 'in' expected")
		end
	elseif self:canbe('if', 'keyword') then
		local cond = assert(self:exp(), "unexpected symbol")
		self:mustbe('then', 'keyword')
		local block = self:block()
		local stmts = table(block)
		-- ...and add elseifs and else to this
		while self:canbe('elseif', 'keyword') do
			local cond = assert(self:exp())
			self:mustbe('then', 'keyword')
			stmts:insert(ast._elseif(cond, table.unpack(assert(self:block()))))
		end
		if self:canbe('else', 'keyword') then
			stmts:insert(ast._else(table.unpack(assert(self:block()))))
		end
		self:mustbe('end', 'keyword')
		return ast._if(cond, table.unpack(stmts))
	elseif self:canbe('repeat', 'keyword') then
		local block = assert(self:block'repeat')
		self:mustbe('until', 'keyword')
		return ast._repeat(assert(self:exp()), table.unpack(block))
	elseif self:canbe('while', 'keyword') then
		local cond = assert(self:exp())
		self:mustbe('do', 'keyword')
		local block = assert(self:block'while')
		self:mustbe('end', 'keyword')
		return ast._while(cond, table.unpack(block))
	elseif self:canbe('do', 'keyword') then
		local block = assert(self:block())
		self:mustbe('end', 'keyword')
		return ast._do(table.unpack(block))
	elseif self.version >= '5.2' then
		if self:canbe('goto', 'keyword') then
			local name = self:mustbe(nil, 'name')
			local g = ast._goto(name)
			g.line, g.col = self.t:getlinecol()
			self.gotos[name] = g
			return g
		-- lua5.2+ break is a statement, so you can have multiple breaks in a row with no syntax error
		elseif self:canbe('break', 'keyword') then
			return self:_break()
		elseif self:canbe('::', 'symbol') then
			local name = self:mustbe(nil, 'name')
			local l = ast._label(name)
			self.labels[name] = true
			self:mustbe('::', 'symbol')
			return l
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
				assert(var.type ~= 'call', "syntax error")
				vars:insert(var)
			end
			self:mustbe('=', 'symbol')
			return ast._assign(vars, assert(self:explist()))
		end
	end
end
-- 'laststat' in 5.1, 'retstat' in 5.2+
function Parser:retstat()
	-- lua5.2+ break is a statement, so you can have multiple breaks in a row with no syntax error
	-- that means only handle 'break' here in 5.1
	if self.version == '5.1' and self:canbe('break', 'keyword') then
		return self:_break()
	end
	if self:canbe('return', 'keyword') then
		local explist = self:explist() or {}
		if self.version >= '5.2' then
			self:canbe(';', 'symbol')
		end
		return ast._return(table.unpack(explist))
	end
end

-- verify we're in a loop, then return the break
function Parser:_break()
	if not ({['while']=1, ['repeat']=1, ['for =']=1, ['for in']=1})[self.blockStack:last()] then
		error("break not inside loop")
	end
	return ast._break()
end

function Parser:funcname()
	if not self:canbe(nil, 'name') then return end
	local name = ast._var(self.lasttoken)
	while self:canbe('.', 'symbol') do
		name = ast._index(name, ast._string(self:mustbe(nil, 'name')))
	end
	if self:canbe(':', 'symbol') then
		name = ast._indexself(name, self:mustbe(nil, 'name'))
	end
	return name
end
function Parser:namelist()
	local name = self:canbe(nil, 'name')
	if not name then return end
	local names = table{ast._var(name)}
	while self:canbe(',', 'symbol') do
		names:insert(ast._var((self:mustbe(nil, 'name'))))
	end
	return names
end
-- same as above but with optional attributes
function Parser:attnamelist()
	local name = self:canbe(nil, 'name')
	if not name then return end
	local attrib = self:attrib()
	local names = table{ast._var(name, attrib)}
	while self:canbe(',', 'symbol') do
		local name = self:mustbe(nil, 'name')
		local attrib = self:attrib()
		names:insert(ast._var(name, attrib))
	end
	return names
end
function Parser:attrib()
	if self.version < '5.4' then return end
	local attrib
	if self:canbe('<', 'symbol') then
		attrib = self:mustbe(nil, 'name')
		self:mustbe('>', 'symbol')
	end
	return attrib
end
function Parser:explist()
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
function Parser:exp()
	return self:exp_or()
end
function Parser:exp_or()
	local a = self:exp_and()
	if not a then return end
	if self:canbe('or', 'keyword') then
		a = ast._or(a,assert(self:exp_or()))
	end
	return a
end
function Parser:exp_and()
	local a = self:exp_cmp()
	if not a then return end
	if self:canbe('and', 'keyword') then
		a = ast._and(a, assert(self:exp_and()))
	end
	return a
end
function Parser:exp_cmp()
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
		a = assert(classForSymbol[self.lasttoken])(a, assert(self:exp_cmp()))
	end
	return a
end
-- BEGIN 5.3+ ONLY:
function Parser:exp_bor()
	local a = self:exp_bxor()
	if not a then return end
	if self:canbe('|', 'symbol') then
		a = ast._bor(a, assert(self:exp_bor()))
	end
	return a
end
function Parser:exp_bxor()
	local a = self:exp_band()
	if not a then return end
	if self:canbe('~', 'symbol') then
		a = ast._bxor(a, assert(self:exp_bxor()))
	end
	return a
end
function Parser:exp_band()
	local a = self:exp_shift()
	if not a then return end
	if self:canbe('&', 'symbol') then
		a = ast._band(a, assert(self:exp_band()))
	end
	return a
end
function Parser:exp_shift()
	local a = self:exp_concat()
	if not a then return end
	if self:canbe('<<', 'symbol')
	or self:canbe('>>', 'symbol')
	then
		local classForSymbol = {
			['<<'] = ast._shl,
			['>>'] = ast._shr,
		}
		local cl = assert(classForSymbol[self.lasttoken])
		local b = assert(self:exp_shift())
		a = cl(a, b)
	end
	return a
end
-- END 5.3+ ONLY:
function Parser:exp_concat()
	local a = self:exp_addsub()
	if not a then return end
	if self:canbe('..', 'symbol') then
		a = ast._concat(a, assert(self:exp_concat()))
	end
	return a
end
function Parser:exp_addsub()
	local a = self:exp_muldivmod()
	if not a then return end
	if self:canbe('+', 'symbol')
	or self:canbe('-', 'symbol')
	then
		local classForSymbol = {
			['+'] = ast._add,
			['-'] = ast._sub,
		}
		local cl = assert(classForSymbol[self.lasttoken])
		local b = assert(self:exp_addsub())
		a = cl(a, b)
	end
	return a
end
function Parser:exp_muldivmod()
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
		a = assert(classForSymbol[self.lasttoken])(a, assert(self:exp_muldivmod()))
	end
	return a
end
function Parser:exp_unary()
	if self:canbe('not', 'keyword') then return ast._not(assert(self:exp_unary())) end
	if self:canbe('#', 'symbol') then return ast._len(assert(self:exp_unary())) end
	if self:canbe('-', 'symbol') then return ast._unm(assert(self:exp_unary())) end
	if self.version >= '5.3' then
		if self:canbe('~', 'symbol') then return ast._bnot(assert(self:exp_unary())) end
	end
	return self:exp_pow()
end
function Parser:exp_pow()
	local a = self:subexp()
	if not a then return end
	if self:canbe('^', 'symbol') then
		a = ast._pow(a, assert(self:exp_unary()))
	end
	return a
end
function Parser:subexp()
	local tableconstructor = self:tableconstructor()
	if tableconstructor then return tableconstructor end

	local prefixexp = self:prefixexp()
	if prefixexp then return prefixexp end

	local functiondef = self:functiondef()
	if functiondef then return functiondef end

	if self:canbe('...', 'symbol') then
		assert(self.functionStack:last() == 'function-vararg')
		return ast._vararg()
	end
	if self:canbe(nil, 'string') then return ast._string(self.lasttoken) end
	if self:canbe(nil, 'number') then return ast._number(self.lasttoken) end
	if self:canbe('true', 'keyword') then return ast._true() end
	if self:canbe('false', 'keyword') then return ast._false() end
	if self:canbe('nil', 'keyword') then return ast._nil() end
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
function Parser:prefixexp()
	local prefixexp

	if self:canbe('(', 'symbol') then
		local exp = assert(self:exp())
		self:mustbe(')', 'symbol')
		prefixexp = ast._par(exp)
	elseif self:canbe(nil, 'name') then
		prefixexp = ast._var(self.lasttoken)
	else
		return
	end

	while true do
		if self:canbe('[', 'symbol') then
			prefixexp = ast._index(prefixexp, assert(self:exp()))
			self:mustbe(']', 'symbol')
		elseif self:canbe('.', 'symbol') then
			prefixexp = ast._index(prefixexp, ast._string(self:mustbe(nil, 'name')))
		elseif self:canbe(':', 'symbol') then
			prefixexp = ast._indexself(prefixexp, self:mustbe(nil, 'name'))
			local args = self:args()
			if not args then error"function arguments expected" end
			prefixexp = ast._call(prefixexp, table.unpack(args))
		else
			local args = self:args()
			if not args then break end

			prefixexp = ast._call(prefixexp, table.unpack(args))
		end
	end

	return prefixexp
end

-- returns nil on fail to match, like all functions
-- produces error on syntax error
-- returns a table of the args -- particularly an empty table if no args were found
function Parser:args()
	if self:canbe(nil, 'string') then return {ast._string(self.lasttoken)} end

	local tableconstructor = self:tableconstructor()
	if tableconstructor then return {tableconstructor} end

	if self:canbe('(', 'symbol') then
		local explist = self:explist()
		self:mustbe(')', 'symbol')
		return explist or {}
	end
end
-- helper which also includes the line and col in the function object
function Parser:makeFunction(...)
	local f = ast._function(...)
	f.line, f.col = self.t:getlinecol()
	f.source = self.source
	return f
end
-- 'function' in the 5.1 syntax
function Parser:functiondef()
	if not self:canbe('function', 'keyword') then return end
	return self:makeFunction(nil, table.unpack(assert(self:funcbody())))
end
-- returns a table of ... first element is a table of args, rest of elements are the body statements
function Parser:funcbody()
	if not self:canbe('(', 'symbol') then return end
	local args = self:parlist() or table()
	local lastArg = args:last()
	local functionType = lastArg and lastArg.type == 'vararg' and 'function-vararg' or 'function'
	self:mustbe(')', 'symbol')
	self.functionStack:insert(functionType)
	local block = self:block(functionType)
	assert(self.functionStack:remove() == functionType)
	self:mustbe('end', 'keyword')
	return table{args, table.unpack(block)}
end
function Parser:parlist()	-- matches namelist() with ... as a terminator
	if self:canbe('...', 'symbol') then return table{ast._vararg()} end
	local name = self:canbe(nil, 'name')
	if not name then return end
	local names = table{ast._var(name)}
	while self:canbe(',', 'symbol') do
		if self:canbe('...', 'symbol') then
			names:insert(ast._vararg())
			return names
		end
		names:insert(ast._var((self:mustbe(nil, 'name'))))
	end
	return names
end
function Parser:tableconstructor()
	if not self:canbe('{', 'symbol') then return end
	local fields = self:fieldlist()
	self:mustbe('}', 'symbol')
	return ast._table(fields or {})
end
function Parser:fieldlist()
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
function Parser:field()
	if self:canbe('[', 'symbol') then
		local keyexp = assert(self:exp())
		self:mustbe(']', 'symbol')
		self:mustbe('=', 'symbol')
		local valexp = self:exp()
		if not valexp then error("expected expression but found "..self.t.token) end
		return ast._assign({keyexp}, {valexp})
	end

	-- this will be Name or exp
	-- in the case that it is a Name then check for = exp
	local exp = self:exp()
	if not exp then return end

	if exp.type == 'var' and self:canbe('=', 'symbol') then
		return ast._assign({ast._string(exp.name)}, {assert(self:exp())})
	else
		return exp
	end
end
function Parser:fieldsep()
	return self:canbe(',', 'symbol') or self:canbe(';', 'symbol')
end

return Parser
