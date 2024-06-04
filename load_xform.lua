-- parser.load_xorm uses ext.load to modify the load(), loadfile() and require() functions
local showcode = require 'template.showcode'
local LuaParser = require 'parser.lua.parser'

local callbacks = setmetatable({}, {__index=table})

require 'ext.load'.xforms:insert(function(d, source)
--DEBUG:print()
--DEBUG:print(debug.traceback())
--DEBUG:print'!!! BEFORE PARSE !!!'
--DEBUG:print('parser.load_xform source: '..source)
--DEBUG:print(showcode(d))
--DEBUG:print()

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

--DEBUG:print()
--DEBUG:print(debug.traceback())
--DEBUG:print'!!! AFTER PARSE !!!'
--DEBUG:print('parser.load_xform source: '..source)
--DEBUG:print(showcode(code))
--DEBUG:print()

	return code
end)

return callbacks
