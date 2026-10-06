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
	-- this holds an even number of numbers, each pair is a from-to index in self.data
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

-- try to use sparingly to cut down on string-allocs
function DataReader:getlasttoken()
	return (self.data:sub(self.lastTokenFrom, self.lastTokenTo))
end

function DataReader:subsetsMatch(from1, to1, from2, to2)
	local lenMinusOne = to1 - from1
	if lenMinusOne ~= to2 - from2 then return false end
	for i=0,lenMinusOne do
		if self.data:byte(from1+i) ~= self.data:byte(from2+i) then return false end
	end
	return true
end

function DataReader:setlasttoken(lastTokenFrom, lastTokenTo, skippedFrom, skippedTo)
	self.lastTokenFrom = lastTokenFrom
	self.lastTokenTo = lastTokenTo
	if self.tracktokens then
		if skippedFrom and skippedTo > skippedFrom then
--DEBUG(@5): print('SKIPPED', require 'ext.tolua'(self.data:sub(skippedFrom, skippedTo)))
			self.tokenhistory:insert(skippedFrom)
			self.tokenhistory:insert(skippedTo)
		end
--DEBUG(@5): print('TOKEN', require 'ext.tolua'(self.data:sub(lastTokenFrom, lastTokenTo)))
		self.tokenhistory:insert(lastTokenFrom)
		self.tokenhistory:insert(lastTokenTo)
	end
end

function DataReader:seekpast(pattern)
--DEBUG(@5): print('DataReader:seekpast', require 'ext.tolua'(pattern))
	local from, to = self.data:find(pattern, self.index)
	if not from then return end
	local skippedFrom = self.index
	local skippedTo = from - 1
	--local skipped = self.data:sub(self.index, from - 1)
	self.index = to + 1
	self:updatelinecol()
	self:setlasttoken(from, to, skippedFrom, skippedTo)
	return true
end

function DataReader:canbe(pattern)
--DEBUG(@5): print('DataReader:canbe', require 'ext.tolua'(pattern))
--DEBUG: assert.eq(pattern:sub(1,1), '^')
	return self:seekpast(pattern)
end

function DataReader:mustbe(pattern, msg)
--DEBUG(@5): print('DataReader:mustbe', require 'ext.tolua'(pattern))
	if not self:canbe(pattern) then error("MSG:expected "..pattern) end
	return true
end

function DataReader:ensureZeroOrOneDot(from, to)
	local numdots = 0
	while true do
		from = self.data:find('.', from, true)
		if not from or from > to then break end
		numdots = numdots + 1
		if numdots > 1 then
			error'MSG:malformed number'
		end
		from = from + 1
	end
	return numdots
end

return DataReader
