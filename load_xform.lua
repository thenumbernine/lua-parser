-- parser.load_xorm uses ext.load to modify the load(), loadfile() and require() functions
--DEBUG: local showcode = require 'template.showcode'
local LuaParser = require 'parser.lua.parser'

local callbacks = setmetatable({}, {__index=table})

local state = require 'ext.load'()
callbacks.state = state

state.xforms:insert(function(d, source)
--DEBUG: print()
--DEBUG: print(debug.traceback())
--DEBUG: print'!!! BEFORE PARSE !!!'
--DEBUG: print('parser.load_xform source: '..source)
--DEBUG: print(showcode(d))
--DEBUG: print()

	local parser = LuaParser()
	local success, msg = parser:setData(d, source)
	if not success then
		if parser.t then
			msg = parser.t:getpos()..': '..msg
		end
		return nil, msg
	end
	local tree = parser.tree
	for _,cb in ipairs(callbacks) do
		cb(tree)
	end
	local code = tree:toLua()

--DEBUG: print()
--DEBUG: print(debug.traceback())
--DEBUG: print'!!! AFTER PARSE !!!'
--DEBUG: print('parser.load_xform source: '..source)
--DEBUG: print(showcode(code))
--DEBUG: print()

	return code
end)

return callbacks
