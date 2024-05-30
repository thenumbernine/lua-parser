local table = require 'ext.table'
local class = require 'ext.class'

local ASTNode = class()

-- returns ancestors as a table, including self
function ASTNode:ancestors()
	local n = self
	local t = table()
	repeat
		t:insert(n)
		n = n.parent
	until not n
	return t
end

-- TODO move traverse flatten etc here once the fields problem is sorted out

return ASTNode
