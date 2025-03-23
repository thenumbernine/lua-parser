#!/usr/bin/env lua
--[[
go through the validation tests and put in comments whether it passes or not, and on what versions
--]]
require 'ext'
local luas = table{
	'lua5.0',	-- lua 5.0.3
	'lua5.1',	-- lua 5.1.5
	'lua5.2',	-- lua 5.2.4 with LUA_COMPAT_ALL enabled
	'lua5.3',	-- lua 5.3.6 with LUA_COMPAT_5_2 enabled
	'lua5.4',	-- lua 5.4.7 with LUA_COMPAT_5_3 enabled
	'luajit',	-- luajit 2.1.x ... I think openresty variant ... with LUAJIT_ENABLE_LUA52COMPAT enabled
	-- (TODO luajit 2.0x vs 2.1x, vanilla vs openresty) 
}
local tmp = path'tmp.lua'
local lines = assert(path'validate.txt':read()):trim():split'\n'
local trimmedlines = lines:mapi(function(line)	-- trim comments
	return (line:match'^(.-)%-%-.*$' or line):trim()
end)
local maxline = trimmedlines:mapi(function(line) return #line end):sup()
for i,line in ipairs(lines) do
	-- TODO more comprehensive on with/without COMPAT flags enabled
	local verstats = {}
	for _,lua in ipairs(luas) do
		tmp:write(line)
		local results = table.pack(os.execute(lua..' -e "assert(loadfile\''..tmp..'\')" > /dev/null 2>&1'))	-- load, don't run
		local success = not not results[1]
		if not success and results[2] == 'signal' and results[3] ~= 1 then break end -- detect ctrl+c instead of syntax error ... this is not always picking it up
		--print()
		--print(results:unpack())
		verstats[lua] = success
		-- [[ check my old key for bugs/changes
		local version = lua:match'^lua(%d%.%d$)'
		if version then
			local expected = not line:match'FAIL_'..version
			assert.eq(expected, result)
		end
		--]]
	end
	local line = trimmedlines[i]	-- don't need comments so use the comment-less version
	print(line..(' '):rep(maxline - #line + 10)..'--\t'..luas:mapi(function(lua)
		return lua..'='..tostring(verstats[lua])
	end):concat'\t')
end
tmp:remove()
