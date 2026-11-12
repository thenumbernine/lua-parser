local table = require 'ext.table'
local string = require 'ext.string'
local class = require 'ext.class'

local BaseAST = class()

-- this is too relaxed, since concat maps to tostring maps to toLua, and I want toLua only called from external, and toLua_recursive from internal
--BaseAST.__concat = string.concat

-- spanSep = where the previous separator token (whitespace/comment) starts
-- spanFrom = where the token starts
-- spanTo = where the token ends
function BaseAST:setspan(spanSep, spanFrom, spanTo)
--DEBUG:assert(spanSep)
--DEBUG:assert(spanFrom)
--DEBUG:assert(spanTo)
	self.spanSep = spanSep
	self.spanFrom = spanFrom
	self.spanTo = spanTo

--DEBUG:local sepLine, sepCol = self.parser.t:getIndexLineCol(self.spanSep)
--DEBUG:local fromLine, fromCol = self.parser.t:getIndexLineCol(self.spanFrom)
--DEBUG:local toLine, toCol = self.parser.t:getIndexLineCol(self.spanTo)
--DEBUG:print('node '..self.type..' '
--DEBUG:	..sepLine..':'..sepCol
--DEBUG:	..' from index '
--DEBUG:	..fromLine..':'..fromCol
--DEBUG:	..' to index '
--DEBUG:	..toLine..':'..toCol
--DEBUG:	..' sep '..require 'ext.tolua'(self.parser.t.r.data:sub(self.spanSep, self.spanFrom-1))
--DEBUG:	..' token '..require 'ext.tolua'(self.parser.t.r.data:sub(self.spanFrom, self.spanTo))
--DEBUG:)
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
