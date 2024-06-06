-- parser.load_xorm uses ext.load to modify the load(), loadfile() and require() functions
local showcode = require 'template.showcode'
local LuaParser = require 'parser.lua.parser'

local callbacks = setmetatable({}, {__index=table})

require 'ext.load'.xforms:insert(function(d, source)
--DEBUG(parser.load_xform): print()
--DEBUG(parser.load_xform): print(debug.traceback())
--DEBUG(parser.load_xform): print'!!! BEFORE PARSE !!!'
--DEBUG(parser.load_xform): print('parser.load_xform source: '..source)
--DEBUG(parser.load_xform): print(showcode(d))
--DEBUG(parser.load_xform): print()

	local parser
	local result, code = xpcall(function()
		parser = LuaParser()
		parser:setData(d, source)
		local tree = parser.tree
		for _,cb in ipairs(callbacks) do
			cb(tree)
		end
		local result = tostring(tree)
		return result
	end, function(err)
		return
			(d and ('parser.load_xform code:\n'..showcode(d)) or '')
			..tostring(source)
			-- TODO move this into LuaParser itself's error generation
			..(parser and (' at '..parser.t:getpos()..'\n') or '')
			..err..'\n'
			..debug.traceback()
	end)
	if not result then error(code) end

--DEBUG(parser.load_xform): print()
--DEBUG(parser.load_xform): print(debug.traceback())
--DEBUG(parser.load_xform): print'!!! AFTER PARSE !!!'
--DEBUG(parser.load_xform): print('parser.load_xform source: '..source)
--DEBUG(parser.load_xform): print(showcode(code))
--DEBUG(parser.load_xform): print()

	return code
end)

return callbacks
