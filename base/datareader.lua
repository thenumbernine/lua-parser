--[[
TODO
store all tokens(term?) as we go in tokenhistory
then have Tokenizer keep track of the range in this array / forward it to be used as the span in AST
then the AST can look into this array, (maybe also keep track of which tokens are whitespace/comments)
... and reproduce the original file exactly as-is (if so desired).

TODO make sure *all* tokens are correctly stored in tokenhistory.  right now it doesn't reproduce source in 100% of cases. maybe just 99%.

TODO terminology ...
DataReader gets chars as input, turns them into ... collections-of-chars?
Tokenizer gets collections-of-chars as input, turns them into tokens
Parser gets tokens as input, turns them into AST nodes
--]]
local table = require 'ext.table'
local class = require 'ext.class'
local asserteq = require 'ext.assert'.eq

local DataReader = class()

-- At the moment this is 100% cosmetic.
-- In case someone doesn't want tracking all tokens done for whatever reason (slowdown, memory, etc)
-- enable/disable this to make token-tracking optional
DataReader.tracktokens = true

function DataReader:init(data)
	self.data = data
	self.index = 1

	-- keep track of all tokens as we parse them.
	self.tokenhistory = table()

	-- TODO this isn't robust against different OS file formats.  maybe switching back to determining line number offline / upon error encounter is better than trying to track it while we parse.
	self.line = 1
	self.col = 1

	-- skip past initial #'s
	if self.data:sub(1,1) == '#' then
		self:seekto'\n'
	end
end

function DataReader:done()
	return self.index > #self.data
end

function DataReader:updatelinecol(skipped)
	local newlines = #skipped:gsub('[^\n]','')
	if newlines > 0 then
		self.line = self.line + newlines
		self.col = #skipped:match('[^\n]*$') + 1
	else
		self.col = self.col + #skipped
	end
end

function DataReader:setlasttoken(lasttoken, skipped)
	self.lasttoken = lasttoken
	if self.tracktokens then
-- TODO depending on whether this is called from seekto() vs seekpast(), skipped might include lasttoken ...
		if skipped and #skipped > 0 then
--DEBUG(parser.base.datareader): print('SKIPPED', skipped)
			self.tokenhistory:insert(skipped)
		end
--DEBUG(parser.base.datareader): print('TOKEN', self.lasttoken)
		self.tokenhistory:insert(self.lasttoken)
--DEBUG(parser.base.datareader paranoid): local sofar = self.tokenhistory:concat()
--DEBUG(parser.base.datareader paranoid): asserteq(self.data:sub(1,#sofar), sofar)
	end
	return self.lasttoken
end

function DataReader:seekto(pattern)
	local from, to = self.data:find(pattern, self.index)
	if not from then
		from = #self.data+1
		to = from
	end
	local skipped = self.data:sub(self.index, from - 1)
	self.index = from
	self:setlasttoken(self.data:sub(from, to), skipped)
	return self:updatelinecol(skipped)
end

function DataReader:seekpast(pattern)
	local from, to = self.data:find(pattern, self.index)
	if not from then return end
	local skipped = self.data:sub(self.index, from - 1)
	self.index = to + 1
	-- TODO hmm if we're using 'skipped' in both places to update the line/col count, and if it is proceeding the token here, but preceding it above, then will it have accurate info?  yes so long as we're always starting at self.index ? for the sake of line counting ... however it will sometimes include the next token and sometimes not ...
	self:updatelinecol(skipped)
	return self:setlasttoken(self.data:sub(from, to), skipped)
end

function DataReader:canbe(pattern)
	return self:seekpast('^'..pattern)
end

function DataReader:mustbe(pattern, msg)
	if not self:canbe(pattern) then error(msg or "expected "..pattern) end
	return self.lasttoken
end

-- TODO this one is specific to Lua languages ... I could move it into tokenizer ...
function DataReader:readblock()
	if not self:canbe('%[=*%[') then return end
	local eq = assert(self.lasttoken:match('^%[(=*)%[$'))
	self:canbe'\n'	-- if the first character is a newline then skip it
	local start = self.index
	if not self:seekpast('%]'..eq..'%]') then
		error("expected closing block")
	end
	-- since we used seekpast, the string isn't being captured as a lasttoken ...
	--return self:setlasttoken(self.data:sub(start, self.index - #self.lasttoken - 1))
	-- ... so don't push it into the history here, just assign it.
	self.lasttoken = self.data:sub(start, self.index - #self.lasttoken - 1)
	return self.lasttoken
end

return DataReader
