local table = require 'ext.table'
local assert = require 'ext.assert'
local Tokenizer = require 'parser.base.tokenizer'

local LuaTokenizer = Tokenizer:subclass()

--[[
NOTICE this only needs to be initialized once per tokenizer, not per-data-source
however at the moment it does need to be initialized once-per-version (as the extra arg to Tokenizer)
maybe I should move it to static initialization and move version-based stuff to subclasses' static-init?

So why 'symbols' vs 'keywords' ?
'Keywords' consist of valid names (names like variables functions etc use)
while 'symbols' consist of everything else. (can symbols contain letters that names can use? at the moment they do not.)
For this reason, when parsing, keywords need separated spaces, while symbols do not (except for distinguishing between various-sized symbols, i.e. < < vs <<).
--]]
function LuaTokenizer:initSymbolsAndKeywords(version, useluajit)
	-- store later for parseHexNumber
	self.version = assert(version)
	self.useluajit = useluajit

	for w in ([[... .. == ~= <= >= + - * / ^ < > = ( ) { } [ ] ; : , .]]):gmatch('%S+') do
		self.symbols:insert(w)
	end

	if version >= '5.1' then
		self.symbols:insert'#'
		self.symbols:insert'%'
	end

	for w in ([[and break do else elseif end false for function if in local nil not or repeat return then true until while]]):gmatch('%S+') do
		self.keywords[w] = true
	end

	-- TODO this will break because luajit doesn't care about versions
	-- if I use a load-test, the ext.load shim layer will break
	-- if I use a load('goto=true') test without ext.load then load() doens't accept strings for 5.1 when the goto isn't a keyword, so I might as well just test if load can load any string ...
	-- TODO separate language features from versions and put all the language options in a ctor table somewhere
	do--if version >= '5.2' then
		self.symbols:insert'::'	-- for labels .. make sure you insert it before ::
		self.keywords['goto'] = true
	end

	if version >= '5.3' then -- and not useluajit then ... setting this fixes some validation tests, but setting it breaks langfix+luajit ... TODO straighten out parser/version configuration
		self.symbols:insert'//'
		self.symbols:insert'~'
		self.symbols:insert'&'
		self.symbols:insert'|'
		self.symbols:insert'<<'
		self.symbols:insert'>>'
	end
end

function LuaTokenizer:init(...)
	LuaTokenizer.super.init(self, ...)

	-- skip past initial #'s
	local r = self.r
	if r.data:sub(1,1) == '#' then
		if not r:seekpast'\n' then
			r:seekpast'$'
		end
	end
end

function LuaTokenizer:parseBlockComment()
	local r = self.r
	-- look for --[====[
	if not r:canbe'%-%-%[=*%[' then return end
	self:readRestOfBlock(r.lasttoken)
	return true
end

function LuaTokenizer:parseString()
	-- try to parse block strings
	if self:parseBlockString() then return true end

	-- try for base's quote strings
	return LuaTokenizer.super.parseString(self)
end

-- Lua-specific block strings
function LuaTokenizer:parseBlockString()
	local r = self.r
	if not r:canbe'%[=*%[' then return end
	if self:readRestOfBlock(r.lasttoken) then
--DEBUG(@5): print('read multi-line string ['..(r.index-#r.lasttoken)..','..r.index..']: '..r.lasttoken)
		coroutine.yield(r.lasttoken, 'string')
		return true
	end
end

function LuaTokenizer:readRestOfBlock(startToken)
	local r = self.r

	local eq = assert(startToken:match('%[(=*)%[$'))
	-- skip whitespace?
	r:canbe'\n'	-- if the first character is a newline then skip it
	local start = r.index
	if not r:seekpast('%]'..eq..'%]') then
		error{msg="expected closing block"}
	end
	-- since we used seekpast, the string isn't being captured as a lasttoken ...
	--return r:setlasttoken(r.data:sub(start, r.index - #r.lasttoken - 1))
	-- ... so don't push it into the history here, just assign it.
	r.lasttoken = r.data:sub(start, r.index - #r.lasttoken - 1)
	return r.lasttoken
end


function LuaTokenizer:parseHexNumber(...)
	local r = self.r
	-- if version is 5.2 then allow decimals in hex #'s, and use 'p's instead of 'e's for exponents
	if self.version >= '5.2' then
		-- TODO this looks like the float-parse code below (but with e+- <-> p+-) but meh I'm lazy so I just copied it.
		local token = r:canbe'[%.%da-fA-F]+'
		local numdots = #token:gsub('[^%.]','')
		assert.le(numdots, 1, {msg='malformed number'})
		local n = table{'0x', token}
		if r:canbe'p' or r:canbe'P' then
			n:insert(r.lasttoken)
			-- fun fact, while the hex float can include hex digits, its 'p+-' exponent must be in decimal.
			n:insert(r:mustbe('[%+%-]%d+', 'malformed number'))
		elseif numdots == 0 and self.useluajit then
			if r:canbe'LL' then
				n:insert'LL'
			elseif r:canbe'ULL' then
				n:insert'ULL'
			end
		end
		coroutine.yield(n:concat(), 'number')
	else
		--return LuaTokenizer.super.parseHexNumber(self, ...)
		local token = r:mustbe('[%da-fA-F]+', 'malformed number')
		local n = table{'0x', token}
		if self.useluajit then
			if r:canbe'LL' then
				n:insert'LL'
			elseif r:canbe'ULL' then
				n:insert'ULL'
			end
		end
		coroutine.yield(n:concat(), 'number')
	end
end

function LuaTokenizer:parseDecNumber()
	local r = self.r
	local token = r:canbe'[%.%d]+'
	local numdots = #token:gsub('[^%.]','')
	assert.le(numdots, 1, {msg='malformed number'})
	local n = table{token}
	if r:canbe'e' or r:canbe'E' then
		n:insert(r.lasttoken)
		n:insert(r:mustbe('[%+%-]%d+', 'malformed number'))
	elseif numdots == 0 and self.useluajit then
		if r:canbe'LL' then
			n:insert'LL'
		elseif r:canbe'ULL' then
			n:insert'ULL'
		end
	end
	coroutine.yield(n:concat(), 'number')
end

return LuaTokenizer
