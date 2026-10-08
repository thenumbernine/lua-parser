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
	self.symbols = table(self.symbols)
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

	tokenFrom, tokenTo, tokenType, tokenData = self:parseNumber()
	if tokenFrom then return tokenFrom, tokenTo, tokenType, tokenData end

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
	r:mustbe('^[%da-fA-F]+', 'malformed number')

	return r.data:sub(from, r.lastTokenTo), 'number'
end

function Tokenizer:parseDecNumber()
	local r = self.r
	if not r:canbe'^[%.%d]+' then return end
	local from = r.lastTokenFrom
	r:ensureZeroOrOneDot(from, r.lastTokenTo)
	if r:canbe'^[eE]' then
		r:canbe'^[%+%-]'
		r:mustbe('^%d+', 'malformed number')
	end
	return from, r.lastTokenTo, 'number'
end

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
	-- TODO provide tokenizer the AST namespace and have it build the tokens (and keywords?) here automatically
	self.symbols = self.symbols:mapi(function(v,k) return true, v end):keys()

	self.tokenTree = {}
	local nodesForTypes = {}
	for _,info in ipairs{
		{type='keyword', strs=table.keys(self.keywords)},
		{type='symbol', strs=self.symbols},
	} do
		nodesForTypes[info.type] = table()
		for _,s in pairs(info.strs) do
			local t = self.tokenTree
			for i=1,#s do
				local b = s:byte(i)
				local n = t[b]
				if not n then
					n = {}
					nodesForTypes[info.type]:insert(n)
					t[b] = n
				end
				t = n
			end
			-- terminator
			t[true] = info.type
		end
	end

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
	for _,n in ipairs(nodesForTypes.keyword) do
		n[true] = n[true] or 'name'
		for _,b in ipairs(name2) do
			n[b] = n[b] or nameNode
		end
	end

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
	self.tokenData = self.nextTokenData
	self.tokenType = self.nextTokenType

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

-- return the index in the data reader
function Tokenizer:getloc()
	return self.prev2index
end

return Tokenizer
