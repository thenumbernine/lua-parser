#!/usr/bin/env lua
--[[
'./validate.lua' = runs through the validation tests and verifies that the parser produces the correct output for the currently-running version of lua
'./validate.lua all' = to test all versions at once (provided you have all built and with their names matching etc ...)
'./validate.lua makekey' = to regenerate the key to stdout
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
local lines = assert(path'validate-key.txt':read()):trim():split'\n'
local trimmedlines = lines:mapi(function(line)	-- trim comments
	return (line:match'^(.-)%-%-.*$' or line):trim()
end)
local maxline = trimmedlines:mapi(function(line) return #line end):sup()

-- which to test? current version or all?
local testluas
if cmdline.all then
	testluas = table(luas)
else
	local version = _VERSION:match'^Lua (.*)$'
	--if version == '5.1' and jit then version = '5.2' end	-- TODO more on luajit versions and COMPAT* builds and parser feature detection ...
	if jit then version = 'jit' end
	testluas = table{'lua'..version}
end

for i,line in ipairs(lines) do

	-- [[ if we're making the key ...
	if cmdline.makekey then
		-- TODO more comprehensive on with/without COMPAT flags enabled
		local verstats = {}
		for _,lua in ipairs(luas) do

			tmp:write(line)
			local results = table.pack(os.execute(lua..' -e "assert(loadfile\''..tmp..'\')" > /dev/null 2>&1'))	-- load, don't run
			local luaSuccess = not not results[1]

			if not luaSuccess and results[2] == 'signal' and results[3] ~= 1 then break end -- detect ctrl+c instead of syntax error ... this is not always picking it up

			--print()
			--print(results:unpack())
			verstats[lua] = luaSuccess
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
			return lua..'='..tostring(verstats[lua] and 1 or 0)
		end):concat'\t')

	else
	--]]
	-- [[ if we're testing the parser ...
		for _,testlua in ipairs(testluas) do
			-- TODO remove the 'lua' prefix and TODO make sure this is compat with whatever the parser version input is ...")

			-- determine 'version' to pass to the parser
			-- TODO more on luajit versions and COMPAT* builds and parser feature detection ...
			local version = testlua:match'^lua(.*)$'
			if version == '5.1' and jit then version = '5.2' end

			local keySuccess = assert(
				-- TODO if we don't have it then ... regenerate it from the bin ... ? and maybe even re-write it out?
				line:match('lua'..version..'=(%d)'), "couldn't find lua version "..version
			) ~= '0'

			local luaSuccess
			if cmdline.all then
				tmp:write(line)
				local results = table.pack(os.execute(testlua..' -e "assert(loadfile\''..tmp..'\')" > /dev/null 2>&1'))	-- load, don't run
				luaSuccess = not not results[1]
			else
				luaSuccess = not not (loadstring or load)(line)
			end

			local LuaParser = require 'parser.lua.parser'
			-- mannnn between parser.parse, Parser:init, Parser:setData, and parser/base/parser and parser/lua/parser, I need to clean up these function signatures
			local parser = LuaParser(nil, version, nil, testlua == 'luajit')
			local parseSuccess, errorString = parser:setData(line)
			parseSuccess = not not parseSuccess
			print('key results', keySuccess, 'parser results', parseSuccess, 'lua results', luaSuccess, 'line', line, 'version', version)
			if keySuccess ~= parseSuccess or parseSuccess ~= luaSuccess then
				error("parser failed to recreate same results.  error="..errorString)
			end
		end
	end
	--]]
end
tmp:remove()
