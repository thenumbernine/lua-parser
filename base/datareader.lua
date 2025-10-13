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
local assert = require 'ext.assert'

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
end

function DataReader:done()
	return self.index > #self.data
end

local slashNByte = ('\n'):byte()
function DataReader:updatelinecol()
	if not self.lastUpdateLineColIndex then
		self.lastUpdateLineColIndex = 1
	else
		assert.ge(self.index, self.lastUpdateLineColIndex)
	end
	for i=self.lastUpdateLineColIndex,self.index do
		if self.data:byte(i,i) == slashNByte then
			self.col = 1
			self.line = self.line + 1
		else
			self.col = self.col + 1
		end
	end
	self.lastUpdateLineColIndex = self.index+1
end

function DataReader:setlasttoken(lasttoken, skipped)
	self.lasttoken = lasttoken
	if self.tracktokens then
		if skipped and #skipped > 0 then
--DEBUG(@5): print('SKIPPED', require 'ext.tolua'(skipped))
			self.tokenhistory:insert(skipped)
		end
--DEBUG(@5): print('TOKEN', require 'ext.tolua'(self.lasttoken))
		self.tokenhistory:insert(self.lasttoken)
--DEBUG(paranoid): local sofar = self.tokenhistory:concat()
--DEBUG(paranoid): assert.eq(self.data:sub(1,#sofar), sofar, "source vs tokenhistory")
	end
	return self.lasttoken
end

function DataReader:seekpast(pattern)
--DEBUG(@5): print('DataReader:seekpast', require 'ext.tolua'(pattern))
	local from, to = self.data:find(pattern, self.index)
	if not from then return end
	local skipped = self.data:sub(self.index, from - 1)
	self.index = to + 1
	self:updatelinecol()
	return self:setlasttoken(self.data:sub(from, to), skipped)
end

function DataReader:canbe(pattern)
--DEBUG(@5): print('DataReader:canbe', require 'ext.tolua'(pattern))
	return self:seekpast('^'..pattern)
end

function DataReader:mustbe(pattern, msg)
--DEBUG(@5): print('DataReader:mustbe', require 'ext.tolua'(pattern))
	if not self:canbe(pattern) then error("MSG:expected "..pattern) end
	return self.lasttoken
end

return DataReader
