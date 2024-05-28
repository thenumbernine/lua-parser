--[[
TODO rename ast.lua => luaast.lua or lua/ast.lua or something
and rename astbase.lua => ast.lua
--]]
local class = require 'ext.class'

local node = class()

-- returns ancestors as a table, including self
function node:ancestors()
	local n = self
	local t = table()
	repeat
		t:insert(n)
		n = n.parent
	until not n
	return t
end

-- TODO move traverse flatten etc here once the fields problem is sorted out

return node