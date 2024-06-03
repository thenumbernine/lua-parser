local table = require 'ext.table'
local assertle = require 'ext.assert'.le
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
	
	for w in ([[... .. == ~= <= >= + - * / % ^ # < > = ( ) { } [ ] ; : , .]]):gmatch('%S+') do
		self.symbols:insert(w)
	end

	for w in ([[and break do else elseif end false for function if in local nil not or repeat return then true until while]]):gmatch('%S+') do
		self.keywords[w] = true
	end

	if version >= '5.2' then
		self.symbols:insert'::'	-- for labels .. make sure you insert it before ::
		self.keywords['goto'] = true
	end
	
	if version >= '5.3' then
		self.symbols:insert'//'
		self.symbols:insert'~'
		self.symbols:insert'&'
		self.symbols:insert'|'
		self.symbols:insert'<<'
		self.symbols:insert'>>'
	end
end

function LuaTokenizer:parseHexNumber(...)
	local r = self.r
	-- if version is 5.2 then allow decimals in hex #'s, and use 'p's instead of 'e's for exponents
	if self.version >= '5.2' then
		-- TODO this looks like the float-parse code below (but with e+- <-> p+-) but meh I'm lazy so I just copied it.
		local token = r:canbe'[%.%da-fA-F]+'
		local numdots = #token:gsub('[^%.]','')
		assertle(numdots, 1, 'malformed number')
		local n = table{'0x', token}
		if r:canbe'p' then
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
	assertle(numdots, 1, 'malformed number')
	local n = table{token}
	if r:canbe'e' then
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
