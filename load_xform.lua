--[[
parser.require only overrides the require statement, and is deprecated.
parser.load_xorm modifies the load(), and loadfile() and require() to go along with it.
--]]
local showcode = require 'template.showcode'
local LuaParser = require 'parser.lua.parser'

local callbacks = setmetatable({}, {__index=table})

require 'ext.load'.xforms:insert(function(d, source)
	local parser
	local result, code = xpcall(function()
		parser = LuaParser()
		parser:setData(d, source)
		local tree = parser.tree
		for _,cb in ipairs(callbacks) do
			cb(tree)
		end
		local result = tostring(tree)
--[[
print()
print(debug.traceback())
print('parser.load_xform source: '..source)
print(showcode(result))
print()
--]]
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
	return code
end)

return callbacks
