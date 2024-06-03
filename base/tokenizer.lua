local table = require 'ext.table'
local string = require 'ext.string'
local class = require 'ext.class'
local assertle = require 'ext.assert'.le
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
				error("unknown token "..r.data:sub(r.index))
			end
		end
	end)
end

function Tokenizer:skipWhiteSpaces()
	local r = self.r
	while r:canbe'%s+' do
--DEBUG: print('read space ['..(r.index-#r.lasttoken)..','..r.index..']: '..r.lasttoken)
	end
end

-- Lua-specific comments (tho changing the comment symbol is easy ...)
Tokenizer.singleLineComment = string.patescape'--'
function Tokenizer:parseComment()
	local r = self.r
	if r:canbe(self.singleLineComment) then
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
--DEBUG: print('read comment ['..start..','..(r.index-1)..']:'..commentstr)
		return true
	end
end

function Tokenizer:parseString()
	if self:parseBlockString() then return true end
	if self:parseQuoteString() then return true end
end

-- Lua-specific block strings
function Tokenizer:parseBlockString()
	local r = self.r
	if r:readblock() then
--DEBUG: print('read multi-line string ['..(r.index-#r.lasttoken)..','..r.index..']: '..r.lasttoken)
		coroutine.yield(r.lasttoken, 'string')
		return true
	end
end

-- '' or "" single-line quote-strings with escape-codes
function Tokenizer:parseQuoteString()
	local r = self.r
	if r:canbe'["\']' then
--DEBUG: print('read quote string ['..(r.index-#r.lasttoken)..','..r.index..']: '..r.lasttoken)
--local start = r.index-#r.lasttoken
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
--DEBUG: print('read quote string ['..start..','..(r.index-#r.lasttoken)..']: '..r.data:sub(start, r.index-#r.lasttoken))
		coroutine.yield(s:concat(), 'string')
		return true
	end
end

-- C names
function Tokenizer:parseName()
	local r = self.r
	if r:canbe'[%a_][%w_]*' then	-- name
--DEBUG: print('read name ['..(r.index-#r.lasttoken)..', '..r.index..']: '..r.lasttoken)
		coroutine.yield(r.lasttoken, self.keywords[r.lasttoken] and 'keyword' or 'name')
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
--local start = r.index
		if r:canbe'0[xX]' then
			self:parseHexNumber()
		else
			self:parseDecNumber()
		end
--DEBUG: print('read number ['..start..', '..r.index..']: '..r.data:sub(start, r.index-1))
		return true
	end
end

function Tokenizer:parseHexNumber()
	local r = self.r
	local token = r:mustbe('[%da-fA-F]+', 'malformed number')
	coroutine.yield('0x'..token, 'number')
end

function Tokenizer:parseDecNumber()
	local r = self.r
	local token = r:canbe'[%.%d]+'
	assertle(#token:gsub('[^%.]',''), 1, 'malformed number')
	local n = table{token}
	if r:canbe'e' then
		n:insert(r.lasttoken)
		n:insert(r:mustbe('[%+%-]%d+', 'malformed number'))
	end
	coroutine.yield(n:concat(), 'number')
end

function Tokenizer:parseSymbol()
	local r = self.r
	-- see if it matches any symbols
	for _,symbol in ipairs(self.symbols) do
		if r:canbe(string.patescape(symbol)) then
--DEBUG: print('read symbol ['..(r.index-#r.lasttoken)..','..r.index..']: '..r.lasttoken)
			coroutine.yield(r.lasttoken, 'symbol')
			return true
		end
	end
end

-- separate this in case someone has to modify the tokenizer symbols and keywords before starting
function Tokenizer:start()
	-- arrange symbols from largest to smallest
	self.symbols:sort(function(a,b) return #a > #b end)
	self:consume()
	self:consume()
end

function Tokenizer:consume()
	-- [[ TODO store these in an array somewhere, make the history adjustable
	-- then in all the get[prev][2]loc's just pass an index for how far back to search
	self.prev2line = self.prevline
	self.prev2col = self.prevcol
	self.prev2index = self.previndex

	self.prevline = self.r.line
	self.prevcol = self.r.col
	self.previndex = self.r.index
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
		error(nexttoken..'\n'
			..debug.traceback(self.gettokenthread))
	end
	self.nexttoken = nexttoken
	self.nexttokentype = nexttokentype
end

function Tokenizer:getlinecol()
	return self.r.line, self.r.col
end

function Tokenizer:getpos()
	local sofar = self.r.data:sub(1,self.r.index)
	local lastline = sofar:match('[^\n]*$') or ''
	return 'line '..self.r.line
		..' col '..self.r.col
		..' code "'..lastline..'"'
end

function Tokenizer:getloc()
	return {line = self.prev2line, col = self.prev2col, index = self.prev2index}
end

return Tokenizer 
