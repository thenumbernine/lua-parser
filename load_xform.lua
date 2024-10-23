-- parser.load_xorm uses ext.load to modify the load(), loadfile() and require() functions
--DEBUG(parser.load_xform): local showcode = require 'template.showcode'
local LuaParser = require 'parser.lua.parser'

local callbacks = setmetatable({}, {__index=table})

local state = require 'ext.load'()
callbacks.state = state

state.xforms:insert(function(d, source)
--DEBUG(parser.load_xform): print()
--DEBUG(parser.load_xform): print(debug.traceback())
--DEBUG(parser.load_xform): print'!!! BEFORE PARSE !!!'
--DEBUG(parser.load_xform): print('parser.load_xform source: '..source)
--DEBUG(parser.load_xform): print(showcode(d))
--DEBUG(parser.load_xform): print()

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

--DEBUG(parser.load_xform): print()
--DEBUG(parser.load_xform): print(debug.traceback())
--DEBUG(parser.load_xform): print'!!! AFTER PARSE !!!'
--DEBUG(parser.load_xform): print('parser.load_xform source: '..source)
--DEBUG(parser.load_xform): print(showcode(code))
--DEBUG(parser.load_xform): print()

	return code
end)

return callbacks
