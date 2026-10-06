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

function Tokenizer:gettoken()
	local r = self.r

	local ws
	repeat
		ws = self:skipWhiteSpaces()
			or self:parseComment()
	until not ws

	if r:done() then return true end

	tk, tt = self:parseString()
	if tk then return tk, tt end

	tk, tt = self:parseName()
	if tk then return tk, tt end

	tk, tt = self:parseNumber()
	if tk then return tk, tt end

	tk, tt = self:parseSymbol()
	if tk then return tk, tt end

	error("MSG:unknown token "..r.data:sub(r.index, r.index+20)..(r.index+20 > #r.data and '...' or ''))
end

function Tokenizer:skipWhiteSpaces()
	local r = self.r
	return r:canbe'^%s+'
--DEBUG(@5): print('read space ['..(r.index-#r:getlasttoken())..','..r.index..']: '..r:getlasttoken())
end

-- Lua-specific comments (tho changing the comment symbol is easy ...)
Tokenizer.singleLineComment = '^'..string.patescape'--'
function Tokenizer:parseComment()
	local r = self.r

	-- TODO try block comments first
	if self:parseBlockComment() then return true end

	if r:canbe(self.singleLineComment) then
--DEBUG(@5):local start = r.index - #r:getlasttoken()
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

-- TODO this is a very lua function though it's in parser/base/ and not parser/lua/ ...
-- '' or "" single-line quote-strings with escape-codes
local backslashByte = ('\\'):byte()
local xByte = ('x'):byte()
local uByte = ('u'):byte()
local _0Byte = ('0'):byte()
local _9Byte = ('9'):byte()
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
--DEBUG(@5): print('read quote string ['..(r.index-#r:getlasttoken())..','..r.index..']: '..r:getlasttoken())
--DEBUG(@5): local start = r.index-#r:getlasttoken()
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
					r:mustbe'^%x'
					local esc =  r:getlasttoken()
					r:mustbe'^%x'
					esc = esc .. r:getlasttoken()
					s = s..string.char(tonumber(esc, 16))
				elseif escByte == uByte and self.version >= '5.3' then
					r:mustbe'^{'
					local code = 0
					while true do
						local ch = r:canbe'^%x' and r:getlasttoken()
						if not ch then break end
						code = code * 16 + tonumber(ch, 16)
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
					-- can read up to three
					local esc = r:getlasttoken()
					if r:canbe'^%d' then esc = esc .. r:getlasttoken() end
					if r:canbe'^%d' then esc = esc .. r:getlasttoken() end
					s = s..string.char(tonumber(esc))
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
--DEBUG(@5): print('read quote string ['..start..','..(r.index-#r:getlasttoken())..']: '..r.data:sub(start, r.index-#r:getlasttoken()))
		return s, 'string'
	end
end

-- C names
function Tokenizer:parseName()
	local r = self.r
	if r:canbe'^[%a_][%w_]*' then	-- name
--DEBUG(@5): print('read name ['..(r.index-#r:getlasttoken())..', '..r.index..']: '..r:getlasttoken())
		return r:getlasttoken(), self.keywords[r:getlasttoken()] and 'keyword' or 'name'
	end
end

function Tokenizer:parseNumber()
	local r = self.r
	if r.data:match('^[%.%d]', r.index) -- if it's a decimal or a number...
	and (r.data:match('^%d', r.index)	-- then, if it's a number it's good
	or r.data:match('^%.%d', r.index))	-- or if it's a decimal then if it has a number following it then it's good ...
	then 								-- otherwise I want it to continue to the next 'else'
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
	return r.data:sub(from, r.lastTokenTo), 'number'
end

function Tokenizer:parseSymbol()
	local r = self.r
	-- see if it matches any symbols
--DEBUG:assert.eq(#self.symbols, #self.symbolsPatescape)
	for _,symbolPatescape in ipairs(self.symbolsPatescape) do
		if r:canbe(symbolPatescape) then
--DEBUG(@5): print('read symbol ['..(r.index-#r:getlasttoken())..','..r.index..']: '..r:getlasttoken())
			return r:getlasttoken(), 'symbol'
		end
	end
end

-- separate this in case someone has to modify the tokenizer symbols and keywords before starting
function Tokenizer:start()
	-- TODO provide tokenizer the AST namespace and have it build the tokens (and keywords?) here automatically
	self.symbols = self.symbols:mapi(function(v,k) return true, v end):keys()
	-- arrange symbols from largest to smallest
	self.symbols:sort(function(a,b) return #a > #b end)
	self.symbolsPatescape = self.symbols:mapi(function(symbol) return '^'..string.patescape(symbol) end)
	self:consume()
	self:consume()
end

function Tokenizer:consume()
	-- [[ TODO store these in an array somewhere, make the history adjustable
	-- then in all the get[prev][2]loc's just pass an index for how far back to search
	self.prev2index = self.previndex
	self.prev2tokenIndex = self.prevtokenIndex

	self.previndex = self.r.index
	self.prevtokenIndex = #self.r.tokenhistory/2+1
	--]]

	self.token = self.nexttoken
	self.tokentype = self.nexttokentype

	local nexttoken, nexttokentype = self:gettoken()
	-- detect errors
	if not nexttoken then
		local err = nexttokentype
		--[[ enabling this to forward errors wasn't so foolproof...
		if type(err) == 'table' then
		--]]
			-- then repackage it and include our parser state
			error('MSG:'..err..' token='..tostring(self.token)..' type='..tostring(self.tokentype)..' pos='..self:getpos())
		--[[ see above
		else
			-- internal error - just rethrow
			error(err)
		end
		--]]
	end

	-- change "done" to empty
	if nexttoken == true then
		self.nexttoken = nil
		self.nexttokentype = nil
	else
		self.nexttoken = nexttoken
		self.nexttokentype = nexttokentype
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
