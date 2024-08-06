#!/usr/bin/env lua
local path = require 'ext.path'
local os = require 'ext.os'
local LuaParser = require 'parser'

-- TODO would be nice to remember who is executing you ... lua vs luajit vs whatever ...
local lua = 'lua'

local inceptionLevel = ... or 1
inceptionLevel = assert(tonumber(inceptionLevel), "expected number")
if inceptionLevel > 5 then 
	print('nobody can survive beyond 5 inception levels')
	return
end

local dstpath = path'inception'
dstpath = dstpath:abs()
dstpath:mkdir()

-- now parse and output a new Lua path in the dst folder ...
local function rewrite(src, dst)
	print(src..' => '..dst)
	dst:getdir():mkdir(true)
	assert(dst:write(tostring(LuaParser.parse((assert(src:read()))))))
end

-- find all lua files?  search the rockspec?
local srcpath = path'../..'
for _,info in ipairs{
	{dir='parser', files={'parser.lua', 'load_xform.lua'}},
	{dir='parser/base', files={'ast.lua', 'datareader.lua', 'parser.lua', 'tokenizer.lua'}},
	{dir='parser/lua', files={'ast.lua', 'parser.lua', 'tokenizer.lua'}},
	{dir='parser/grammar', files={'parser.lua', 'tokenizer.lua'}},
	{dir='parser/tests', files={'flatten.lua', 'lua_to_c.lua', 'lua_to_c_test.lua', 'minify_tests.lua', 'parse.lua', 'parsemyself.lua', 'spantest.lua'}},
} do
	for _,fn in ipairs(info.files) do
		rewrite(srcpath/info.dir/fn, dstpath/info.dir/fn)
	end
end

-- then chdir and run it again
dstpath'parser/tests':cd()
os.exec(
	-- if you want to only use reparsed content for the second parse ...
	--'LUA_PATH="'..dstpath..'/?.lua;'..dstpath..'/?/?.lua" && '..
	lua..' parsemyself.lua '..(inceptionLevel+1))
