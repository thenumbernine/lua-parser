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
	self.gettokenthread = coroutine.create(function()
		local r = self.r

		while not r:done() do
			self:skipWhiteSpaces()
			if r:done() then break end

			if self:parseComment() then
			elseif self:parseString() then
			elseif self:parseName() then
			elseif self:parseNumber() then
			elseif self:parseSymbol() then
			else
				error("MSG:unknown token "..r.data:sub(r.index, r.index+20)..(r.index+20 > #r.data and '...' or ''))
			end
		end
	end)
end

function Tokenizer:skipWhiteSpaces()
	local r = self.r
	r:canbe'^%s+'
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
	if self:parseQuoteString() then return true end
end

-- TODO this is a very lua function though it's in parser/base/ and not parser/lua/ ...
-- '' or "" single-line quote-strings with escape-codes
local backslashByte = ('\\'):byte()
local escapeCodes = {a='\a', b='\b', f='\f', n='\n', r='\r', t='\t', v='\v', ['\\']='\\', ['"']='"', ["'"]="'", ['0']='\0', ['\r']='\n', ['\n']='\n'}
function Tokenizer:parseQuoteString()
	local r = self.r
	if r:canbe'^["\']' then
--DEBUG(@5): print('read quote string ['..(r.index-#r:getlasttoken())..','..r.index..']: '..r:getlasttoken())
--DEBUG(@5): local start = r.index-#r:getlasttoken()
		local quoteFrom, quoteTo = r.lastTokenFrom, r.lastTokenTo
		local s = table()
		while true do
			r:seekpast'.'
			if r:subsetsMatch(r.lastTokenFrom, r.lastTokenTo, quoteFrom, quoteTo) then break end
			if r:done() then error("MSG:unfinished string") end
			if r.lastTokenFrom == r.lastTokenTo
			and r.data:byte(r.lastTokenFrom) == backslashByte
			then
				local esc = r:canbe'^.' and r:getlasttoken()
				local escapeCode = escapeCodes[esc]
				if escapeCode then
					s:insert(escapeCode)
				elseif esc == 'x' and self.version >= '5.2' then
					r:mustbe'^%x'
					esc =  r:getlasttoken()
					r:mustbe'^%x'
					esc = esc .. r:getlasttoken()
					s:insert(string.char(tonumber(esc, 16)))
				elseif esc == 'u' and self.version >= '5.3' then
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
						s:insert(string.char(code))	-- 0xxxxxxx
					elseif code < 0x800 then
						s:insert(
							string.char(bit.bor(0xc0, bit.band(0x1f, bit.rshift(code, 6))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, code)))
						)
					elseif code < 0x10000 then
						s:insert(
							string.char(bit.bor(0xe0, bit.band(0x0f, bit.rshift(code, 12))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, bit.rshift(code, 6))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, code)))
						)
					else
						s:insert(
							string.char(bit.bor(0xf0, bit.band(0x07, bit.rshift(code, 18))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, bit.rshift(code, 12))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, bit.rshift(code, 6))))
							.. string.char(bit.bor(0x80, bit.band(0x3f, code)))
						)
					end
				elseif esc:match'%d' then
					-- can read up to three
					if r:canbe'^%d' then esc = esc .. r:getlasttoken() end
					if r:canbe'^%d' then esc = esc .. r:getlasttoken() end
					s:insert(string.char(tonumber(esc)))
				else
					if self.version >= '5.2' then
						-- lua5.1 doesn't care about bad escape codes
						error("MSG:invalid escape sequence "..esc)
					end
				end
			else
				s:insert(r:getlasttoken())
			end
		end
--DEBUG(@5): print('read quote string ['..start..','..(r.index-#r:getlasttoken())..']: '..r.data:sub(start, r.index-#r:getlasttoken()))
		coroutine.yield(s:concat(), 'string')
		return true
	end
end

-- C names
function Tokenizer:parseName()
	local r = self.r
	if r:canbe'^[%a_][%w_]*' then	-- name
--DEBUG(@5): print('read name ['..(r.index-#r:getlasttoken())..', '..r.index..']: '..r:getlasttoken())
		coroutine.yield(r:getlasttoken(), self.keywords[r:getlasttoken()] and 'keyword' or 'name')
		return true
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
			self:parseHexNumber()
		else
			self:parseDecNumber()
		end
--DEBUG(@5): print('read number ['..start..', '..r.index..']: '..r.data:sub(start, r.index-1))
		return true
	end
end

function Tokenizer:parseHexNumber()
	local r = self.r
	r:mustbe('^[%da-fA-F]+', 'malformed number')
	local token = r:getlasttoken()
	coroutine.yield('0x'..token, 'number')
end

function Tokenizer:parseDecNumber()
	local r = self.r
	local token = r:canbe'^[%.%d]+' and r:getlasttoken()
	assert.le(#token:gsub('[^%.]',''), 1, 'malformed number')
	local n = table{token}
	if r:canbe'^[eE]' then
		n:insert(r:getlasttoken())
		if r:canbe'^[%+%-]' then
			n:insert(r:getlasttoken())
		end
		r:mustbe('^%d+', 'malformed number')
		n:insert(r:getlasttoken())
	end
	coroutine.yield(n:concat(), 'number')
end

function Tokenizer:parseSymbol()
	local r = self.r
	-- see if it matches any symbols
--DEBUG:assert.eq(#self.symbols, #self.symbolsPatescape)
	for _,symbolPatescape in ipairs(self.symbolsPatescape) do
		if r:canbe(symbolPatescape) then
--DEBUG(@5): print('read symbol ['..(r.index-#r:getlasttoken())..','..r.index..']: '..r:getlasttoken())
			coroutine.yield(r:getlasttoken(), 'symbol')
			return true
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
	if coroutine.status(self.gettokenthread) == 'dead' then
		self.nexttoken = nil
		self.nexttokentype = nil
		-- done = true
		return
	end
	local status, nexttoken, nexttokentype = coroutine.resume(self.gettokenthread)
	-- detect errors
	if not status then
		local err = nexttoken
		--[[ enabling this to forward errors wasn't so foolproof...
		if type(err) == 'table' then
		--]]
			-- then repackage it and include our parser state
			error('MSG:'..err..' token='..tostring(self.token)..' type='..tostring(self.tokentype)..' pos='..self:getpos())
				--..'\n'..debug.traceback(self.gettokenthread))
		--[[ see above
		else
			-- internal error - just rethrow
			error(err)
		end
		--]]
	end
	self.nexttoken = nexttoken
	self.nexttokentype = nexttokentype
end

function Tokenizer:getpos()
	return 'line '..self.r.line
		..' col '..self.r.col
		..' code "'..self.r.data:sub(self.r.index):match'^[^\n]*'..'"'
end

-- return the span across
function Tokenizer:getloc()
	local r = self.r
	local line = self.r.line
	local col = self.r.col

	return {
		line = line,
		col = col,
		index = self.prev2index,
		tokenIndex = self.prev2tokenIndex,
	}
end

return Tokenizer
