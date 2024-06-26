local table = require 'ext.table'
local string = require 'ext.string'
local class = require 'ext.class'

local BaseAST = class()

BaseAST.__concat = string.concat

function BaseAST:setspan(span)
	self.span = span
	return self
end

-- returns ancestors as a table, including self
function BaseAST:ancestors()
	local n = self
	local t = table()
	repeat
		t:insert(n)
		n = n.parent
	until not n
	return t
end

-- TODO move traverse flatten etc here once the fields problem is sorted out

return BaseAST
