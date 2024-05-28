local class = require 'ext.class'

local DataReader = class()

function DataReader:init(data)
	self.data = data
	self.index = 1
	
	-- TODO this isn't robust against different OS file formats.  maybe switching back to determining line number offline / upon error encounter is better than trying to track it while we parse.
	self.line = 1
	self.col = 1
	
	-- skip past initial #'s
	if self.data:sub(1,1) == '#' then
		self.index = self.data:find('\n')+1
		self.line = self.line + 1
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

function DataReader:seekto(pattern)
	local from, to = self.data:find(pattern, self.index)
	if not from then
		from = #self.data+1
		to = from
	end
	local skipped = self.data:sub(self.index, from - 1)
	self.index = from
	self.lasttoken = self.data:sub(from, to)
	self:updatelinecol(skipped)
	return self.lasttoken
end

function DataReader:seekpast(pattern)
	local from, to = self.data:find(pattern, self.index)
	if not from then return end
	local skipped = self.data:sub(self.index, to)
	self.index = to + 1
	self.lasttoken = self.data:sub(from, to)
	self:updatelinecol(skipped)
	return self.lasttoken
end

function DataReader:canbe(pattern)
	return self:seekpast('^'..pattern)
end

function DataReader:mustbe(pattern, msg)
	if not self:canbe(pattern) then error(msg or "expected "..pattern) end
	return self.lasttoken
end

function DataReader:readblock()
	if not self:canbe('%[=*%[') then return end
	local eq = assert(self.lasttoken:match('^%[(=*)%[$'))
	local start = self.index
	if not self:seekpast('%]'..eq..'%]') then
		error("expected closing block")
	end
	self.lasttoken = self.data:sub(start, self.index - #self.lasttoken - 1)
	return self.lasttoken
end

return DataReader
