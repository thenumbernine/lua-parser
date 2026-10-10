local table = require 'ext.table'
local string = require 'ext.string'
local class = require 'ext.class'
local assert = require 'ext.assert'
local DataReader = require 'parser.base.datareader'

local Tokenizer = class()

function Tokenizer:initSymbolsAndKeywords(...)
end

function Tokenizer:init(data, ...)
	-- TODO move what this does to just the subclass initialization
	self.symbols = table(self.symbols):setmetatable(nil)
	self.keywords = table(self.keywords):setmetatable(nil)
	self:initSymbolsAndKeywords(...)

	self.r = DataReader(data)
end

function Tokenizer:parseNextToken()
	local r = self.r

	local ws
	repeat
		ws = self:skipWhiteSpaces()
			or self:parseComment()
	until not ws

	if r:done() then return true end

	-- "tokenData" is because strings parse-and-convert
	-- whereas all other tokens don't need to convert so they don't need any tokenData data besides the range in the input stream
	local tokenFrom, tokenTo, tokenType, tokenData

	tokenFrom, tokenTo, tokenType, tokenData = self:parseString()
	if tokenFrom then return tokenFrom, tokenTo, tokenType, tokenData end

	-- [=[
	tokenFrom, tokenTo, tokenType, tokenData = self:parseNumber()
	if tokenFrom then return tokenFrom, tokenTo, tokenType, tokenData end
	--]=]

	tokenFrom, tokenTo, tokenType, tokenData = self:parseNameOrSymbol()
	if tokenFrom then return tokenFrom, tokenTo, tokenType, tokenData end

	error("MSG:unknown token "..r.data:sub(r.index, r.index+20)..(r.index+20 > #r.data and '...' or ''))
end

function Tokenizer:skipWhiteSpaces()
	return self.r:canbe'^%s+'
--DEBUG(@5): print('read space ['..(r.index-(r.lastTokenTo-r.lastTokenFrom+1))..','..r.index..']: '..r:getlasttoken())
end

-- Lua-specific comments (tho changing the comment symbol is easy ...)
Tokenizer.singleLineComment = '^'..string.patescape'--'
function Tokenizer:parseComment()
	local r = self.r

	-- TODO try block comments first
	if self:parseBlockComment() then return true end

	if r:canbe(self.singleLineComment) then
--DEBUG(@5):local start = r.index - (r.lastTokenTo-r.lastTokenFrom+1)
		-- read line
		if not r:seekpast'\n' then
			r:seekpast'$'
		end
--DEBUG(@5):local commentstr = r.data:sub(start, r.index-1)
		-- TODO how to insert comments into the AST?  should they be their own nodes?
		-- should all whitespace be its own node, so the original code text can be reconstructed exactly?
		--coroutine.yield(commentstr, 'comment')
--DEBUG(@5):print('read comment ['..start..','..(r.index-1)..']:'..commentstr)
		return true
	end
end

-- parse a string
function Tokenizer:parseString()
	return self:parseQuoteString()
end

local _0Byte = ('0'):byte()
local _9Byte = ('9'):byte()
local aByte = ('a'):byte()
local fByte = ('f'):byte()
local AByte = ('A'):byte()
local FByte = ('F'):byte()

local function asciiByteToDec(b)
	if b >= _0Byte and b <= _9Byte then return b - _0Byte end
	error"shouldn't get here"
end

local function asciiByteToHex(b)
	if b >= _0Byte and b <= _9Byte then return b - _0Byte end
	if b >= aByte and b <= fByte then return b - aByte + 10 end
	if b >= AByte and b <= FByte then return b - AByte + 10 end
	error"shouldn't get here"
end

-- TODO this is a very lua function though it's in parser/base/ and not parser/lua/ ...
-- '' or "" single-line quote-strings with escape-codes
local backslashByte = ('\\'):byte()
local xByte = ('x'):byte()
local uByte = ('u'):byte()
local escapeCodes = {
	[('a'):byte()]='\a',
	[('b'):byte()]='\b',
	[('f'):byte()]='\f',
	[('n'):byte()]='\n',
	[('r'):byte()]='\r',
	[('t'):byte()]='\t',
	[('v'):byte()]='\v',
	[('\\'):byte()]='\\',
	[('"'):byte()]='"',
	[("'"):byte()]="'",
	[('0'):byte()]='\0',
	[('\r'):byte()]='\n',
	[('\n'):byte()]='\n'
}
function Tokenizer:parseQuoteString()
	local r = self.r
	if r:canbe'^["\']' then
--DEBUG(@5): print('read quote string ['..(r.index-(r.lastTokenTo-r.lastTokenFrom+1))..','..r.index..']: '..r:getlasttoken())
--DEBUG(@5): local start = r.index-(r.lastTokenTo-r.lastTokenFrom+1)
		local quoteFrom, quoteTo = r.lastTokenFrom, r.lastTokenTo
		local s = ''
		while true do
			r:seekpast'.'
			if r:subsetsMatch(r.lastTokenFrom, r.lastTokenTo, quoteFrom, quoteTo) then break end
			if r:done() then error("MSG:unfinished string") end
			if r.lastTokenFrom == r.lastTokenTo
			and r.data:byte(r.lastTokenFrom) == backslashByte
			then
				local escByte = r:canbe'^.' and r.data:byte(r.lastTokenFrom)
				local escapeCode = escapeCodes[escByte]
				if escapeCode then
					s = s..escapeCode
				elseif escByte == xByte and self.version >= '5.2' then
					local code = 0
					r:mustbe'^%x'
					code = code * 16 + asciiByteToHex(r.data:byte(r.lastTokenFrom))
					r:mustbe'^%x'
					code = code * 16 + asciiByteToHex(r.data:byte(r.lastTokenFrom))
					s = s..string.char(code)
				elseif escByte == uByte and self.version >= '5.3' then
					r:mustbe'^{'
					local code = 0
					while true do
						if not r:canbe'^%x' then break end
						code = code * 16 + asciiByteToHex(r.data:byte(r.lastTokenFrom))
					end
					r:mustbe'^}'

					-- hmm, needs bit library or bit operations, which should only be present in version >= 5.3 anyways so ...
					local bit = bit or bit32 or require 'bit'
					if code < 0x80 then
						s = s..string.char(code)	-- 0xxxxxxx
					elseif code < 0x800 then
						s = s
							.. string.char(bit.bor(0xc0, bit.band(0x1f, bit.rshift(code, 6))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, code)))
					elseif code < 0x10000 then
						s = s
							.. string.char(bit.bor(0xe0, bit.band(0x0f, bit.rshift(code, 12))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, bit.rshift(code, 6))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, code)))
					else
						s = s
							.. string.char(bit.bor(0xf0, bit.band(0x07, bit.rshift(code, 18))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, bit.rshift(code, 12))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, bit.rshift(code, 6))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, code)))
					end
				elseif escByte >= _0Byte and escByte <= _9Byte then
					local code = asciiByteToDec(r.data:byte(r.lastTokenFrom))
					-- can read up to three
					if r:canbe'^%d' then
						code = code * 10 + asciiByteToDec(r.data:byte(r.lastTokenFrom))
						if r:canbe'^%d' then
							code = code * 10 + asciiByteToDec(r.data:byte(r.lastTokenFrom))
						end
					end
					s = s..string.char(code)
				else
					if self.version >= '5.2' then
						-- lua5.1 doesn't care about bad escape codes
						error("MSG:invalid escape sequence "..esc)
					end
				end
			else
				s = s..r:getlasttoken()
			end
		end
--DEBUG(@5): print('read quote string ['..start..','..(r.index-(r.lastTokenTo-r.lastTokenFrom+1))..']: '..r.data:sub(start, r.index-(r.lastTokenTo-r.lastTokenFrom+1)))
		return 1, #s, 'string', s
	end
end

-- [=[
function Tokenizer:parseNumber()
	local r = self.r
	if r.data:find('^[%.%d]', r.index) 		-- if it's a decimal or a number...
	and (
		r.data:find('^%d', r.index)			-- then, if it's a number it's good
		or r.data:find('^%.%d', r.index)	-- or if it's a decimal then if it has a number following it then it's good ...
	) then 									-- otherwise I want it to continue to the next 'else'
		-- lua doesn't consider the - to be a part of the number literal
		-- instead, it parses it as a unary - and then possibly optimizes it into the literal during ast optimization
--DEBUG(@5): local start = r.index
		if r:canbe'^0[xX]' then
			return self:parseHexNumber()
		else
			return self:parseDecNumber()
		end
--DEBUG(@5): print('read number ['..start..', '..r.index..']: '..r.data:sub(start, r.index-1))
	end
end

function Tokenizer:parseHexNumber()
	-- save here to include 0x
	local from = r.lastTokenFrom

	local r = self.r
	r:mustbe('^%x+', 'malformed number')

	return from, r.lastTokenTo, 'number'
end

function Tokenizer:parseDecNumber()
	local r = self.r
	if not r:canbe'^[%.%d]+' then return end
	local from = r.lastTokenFrom
	r:ensureZeroOrOneDot(from, r.lastTokenTo)
	if r:canbe'^[eE]' then
		r:canbe'^[+-]'
		r:mustbe('^%d+', 'malformed number')
	end
	return from, r.lastTokenTo, 'number'
end
--]=]

function Tokenizer:parseNameOrSymbol()
	local r = self.r
	-- see if it matches any symbols
--DEBUG:assert.eq(#self.symbols, #self.symbolsPatescape)
	local t = self.tokenTree
	local lastValidLoc
	local lastValidType
	for i=r.index,math.huge do
		local b = r.data:byte(i)
		local n = t[b]
		if not n then
			if not lastValidLoc then
				return
			end
			-- r:canbe/r:seekpast implementation:
			local from = r.index
			local to = lastValidLoc
			r.index = to+1
			r:updatelinecol()
			r:setlasttoken(from, to, from, from-1)

			return from, to, lastValidType
		else
			local possibleEnd = n[true]
			if possibleEnd then
				lastValidType = possibleEnd
				lastValidLoc = i
			end
		end
		t = n
	end
end

-- separate this in case someone has to modify the tokenizer symbols and keywords before starting
function Tokenizer:start()
	self.tokenTree = {}


	-- i'm too lazy to write another parser.
	-- this is a table of objects per character rule.
	-- each has list of characters for this character rule,
	--  'maybe' or 'multiple' flags apply
	local newnodes
	local function addrule(args)
		local sofar = args.sofar or ''
		local tokentype = args.type
		local rules = args.rules
		local t = args.tree
		if #rules == 0 then
			if t[true] and t[true] ~= tokentype then
				error("got overlapping token at "..sofar..': '..t[true]..' vs '..tokentype)
			end
			t[true] = tokentype
			return
		end
		local r = rules[1]
		local rest = table.sub(rules, 2)
		if r.maybe then
			local rnomaybe = {}
			for i=1,#r do rnomaybe[i] = r[i] end
			addrule{
				sofar = sofar..'?',
				type = tokentype,
				tree = t,
				rules = table{rnomaybe}:append(rest),
			}
			addrule{
				sofar = sofar..'?',
				type = tokentype,
				tree = t,
				rules = res,
			}
		elseif r.many then
			local manyNode
			for _,b in ipairs(r) do
				local n = t[b]
				if n then
					addrule{
						sofar = sofar..'*',
						type = tokentype,
						tree = n,
						rules = rest,
					}
				else
					if not manyNode then
						-- all previous keys
						manyNode = {}
						for _,b in ipairs(r) do
							manyNode[b] = manyNode
						end
						addrule{
							sofar = sofar..'*',
							type = tokentype,
							tree = manyNode,
							rules = rest,
						}
					end
					t[b] = manyNode
				end
			end
		else
			for _,b in ipairs(r) do
				local n = t[b]
				if not n then
					n = {}
					newnodes:insert(n)
					t[b] = n
				else
					-- TODO verify there's no cyclic edges, otherwise we'll have to break them apart
					-- only store one previous cycles
					assert.ne(t, n)
				end
				addrule{
					sofar = sofar..string.char(b),
					type = tokentype,
					tree = n,
					rules = rest,
				}
			end
		end
	end


	local nodesForTypes = {}
	for _,info in ipairs{
		{type='keyword', strs=table.keys(self.keywords)},
		{type='symbol', strs=table.keys(self.symbols)},
	} do
		newnodes = table()
		nodesForTypes[info.type] = newnodes
		for _,s in pairs(info.strs) do
			addrule{
				type = info.type,
				tree = self.tokenTree,
				rules = string.split(s)
					:mapi(function(c) return {c:byte()} end),
			}
		end
	end


	--	names
	--	[_%a][_%w]*
	--[=[
	local lcase = range(('a'):byte(), ('z'):byte())
	local ucase = range(('a'):byte(), ('z'):byte())
	local alpha = table():append(lcase, ucase)
	local alphanum = table(alpha):append(range(('0'):byte(), ('9'):byte()))
	addrule{
		type = 'name',
		tree = self.tokenTree,
		rules = table{
			table(alpha, {maybe=true}),
			table(alphanum, {many=true})
		},
	}
	--]=]
	-- [=[
	-- fill in the tokenTree to handle names
	local name1 = table{(('_'):byte())}
	for i=('a'):byte(),('z'):byte() do
		name1:insert(i)
		name1:insert(i + ('A'):byte() - ('a'):byte())
	end
	local name2 = table(name1)
	for i=0,9 do
		name2:insert(('0'):byte() + i)
	end

	-- add self-transitions so we can process name characters as long as they go
	local nameNode = {[true] = 'name'}
	for _,b in ipairs(name2) do
		nameNode[b] = nameNode
	end

	-- add first character of name transitions to table
	for _,b in ipairs(name1) do
		self.tokenTree[b] = self.tokenTree[b] or nameNode
	end

	-- now for keyword nodes, add remaining name transitions
	-- TODO instead of this, how about automata combine?
	for _,n in ipairs(nodesForTypes.keyword) do
		n[true] = n[true] or 'name'
		for _,b in ipairs(name2) do
			n[b] = n[b] or nameNode
		end
	end
	--]=]


--[[
decimal numbers?

%d+
%d+[lL][lL]
%d+[uU][lL][lL]

%d+[Ee][+-]%d+
%d*%.%d+
%d+%.%d*[Ee][+-]%d+

hex numbers

0x%x+[lL][lL]
0x%x+[uU][lL][lL]

0x%x*%.%x+[Pp][+-]%d+
0x%x+%.%x*[Pp][+-]%d+

combined:

%d*%.%d+
%d*%d+
%d*%d+[lL][lL]				luajit
%d*%d+[uU][lL][lL]			luajit
%d*%d+[Ee][+-]%d+
%d*%d+%.%d*[Ee][+-]%d+

0x%x*%.%x+[Pp][+-]%d+		>= 5.2
0x%x*%x+[lL][lL]			luajit
0x%x*%x+[uU][lL][lL]		luajit
0x%x*%x+%.%x*[Pp][+-]%d+	>= 5.2

--]]
--[=[
	local lByte = ('l'):byte()
	local LByte = ('L'):byte()
	local uByte = ('u'):byte()
	local UByte = ('U'):byte()
	-- %d+
	local decNode = {
		[true] = 'number',
	}

	-- %d+[uU]?[lL][lL]
	local numberTerm = {[true] = 'number'}
	local LL = {[lByte] = numberTerm, [LByte] = numberTerm}
	local ULL = {[lByte] = LL, [LByte] = LL}
	if self.useluajit then
		decNode[uByte] = ULL
		decNode[UByte] = ULL
		decNode[lByte] = LL
		decNode[LByte] = LL
	end
	for b=('0'):byte(),('9'):byte() do
		decNode[b] = decNode
	end

	-- %d+[eE][+-]
	local decExpNext = {[true] = 'number'}
	for i=('0'):byte(),('9'):byte() do
		decExpNext[i] = decExpNext
	end
	local decExp1st = {}
	for i=('0'):byte(),('9'):byte() do
		decExp1st[i] = decExpNext
	end
	local decEPMExp = {
		[('+'):byte()] = decExp1st,
		[('-'):byte()] = decExp1st,
	}
	decNode[('e'):byte()] = decEPMExp
	decNode[('E'):byte()] = decEPMExp

	-- %d+%.%d*[Ee][+-]%d+
	local decDecNode = {[true] = 'number'}
	for b=('0'):byte(),('9'):byte() do
		decDecNode[b] = decDecNode
	end
	decDecNode[('e'):byte()] = decEPMExp
	decDecNode[('E'):byte()] = decEPMExp
	decNode[('.'):byte()] = decDecNode

	for b=('1'):byte(),('9'):byte() do
		assert(not self.tokenTree[b])
		self.tokenTree[b] = decNode
	end

	-- 0x%x*%.%x+[Pp][+-]%d+
	-- 0x%x+%.%x*[Pp][+-]%d+
	local hexDecNode = {[true] = 'number'}
	for b=('0'):byte(),('9'):byte() do
		hexDecNode[b] = decDecNode
	end
	if self.version >= '5.2' then
		hexDecNode[('p'):byte()] = decEPMExp
		hexDecNode[('P'):byte()] = decEPMExp
	end

	local hexBytes = table():append(
		range(('0'):byte(), ('9'):byte()),
		range(('a'):byte(), ('f'):byte()),
		range(('A'):byte(), ('F'):byte())
	)

	local hexNode = {[true] = 'number'}
	for _,b in ipairs(hexBytes) do
		hexNode[b] = hexNode
	end
	if self.useluajit then
		hexNode[uByte] = ULL
		hexNode[UByte] = ULL
		hexNode[lByte] = LL
		hexNode[LByte] = LL
	end
	hexNode[('.'):byte()] = hexDecNode
	if self.version >= '5.2' then
		hexNode[('p'):byte()] = decEPMExp
		hexNode[('P'):byte()] = decEPMExp
	end

	local hex1stNode = {}
	for _,b in ipairs(hexBytes) do
		hex1stNode[b] = hexNode
	end
	hex1stNode[('.'):byte()] = hexDecNode

	local _0Node = {[true] = 'number'}
	_0Node[('x'):byte()] = hex1stNode
	_0Node[('X'):byte()] = hex1stNode
	for b=('1'):byte(),('9'):byte() do
		_0Node[b] = decNode
	end
	self.tokenTree[('0'):byte()] = _0Node

	-- %d*%.%d+
	for b=('0'):byte(),('9'):byte() do
		self.tokenTree[('.'):byte()][b] = decDecNode
	end
--]=]

	self:consume()
	self:consume()
end

function Tokenizer:gettoken()
	if not self.tokenFrom then return nil end
	if not self.token then
		self.token = (self.tokenData or self.r.data):sub(self.tokenFrom, self.tokenTo)
	end
	return self.token
end

function Tokenizer:consume()
	-- [[ TODO store these in an array somewhere, make the history adjustable
	-- then in all the get[prev][2]loc's just pass an index for how far back to search
	self.prev2index = self.previndex
	self.prev2tokenIndex = self.prevtokenIndex

	self.previndex = self.r.index
	self.prevtokenIndex = #self.r.tokenhistory/2-1
	--]]

	self.token = nil
	self.tokenFrom = self.nextTokenFrom
	self.tokenTo = self.nextTokenTo
	self.tokenType = self.nextTokenType
	self.tokenData = self.nextTokenData

	local nextTokenFrom, nextTokenTo, nextTokenType, nextTokenData = self:parseNextToken()
	-- detect errors
	if not nextTokenFrom then
		local err = nextTokenType
		--[[ enabling this to forward errors wasn't so foolproof...
		if type(err) == 'table' then
		--]]
			-- then repackage it and include our parser state
			error('MSG:'..err..' token='..tostring(self:gettoken())..' type='..tostring(self.tokenType)..' pos='..self:getpos())
		--[[ see above
		else
			-- internal error - just rethrow
			error(err)
		end
		--]]
	end

	-- change "done" to empty
	if nextTokenFrom == true then
		self.nextTokenFrom = nil
		self.nextTokenTo = nil
		self.nextTokenType = nil
		self.nextTokenData = nil
	else
		self.nextTokenFrom = nextTokenFrom
		self.nextTokenTo = nextTokenTo
		self.nextTokenType = nextTokenType
		self.nextTokenData = nextTokenData
	end
end

function Tokenizer:getpos()
	return 'line '..self.r.line
		..' col '..self.r.col
		..' code "'..self.r.data:sub(self.r.index):match'^[^\n]*'..'"'
end

function Tokenizer:getIndexLineCol(index)
	local line = 1
	local i = 1
	while i < index do
		local j = self.r.data:find('\n', i)
		if not j or j >= index then break end
		line = line + 1
		i = j+1
	end
	local col = index - i
	return line, col
end

-- return the index in the data reader
function Tokenizer:getloc()
	return self.prev2index
end

return Tokenizer
