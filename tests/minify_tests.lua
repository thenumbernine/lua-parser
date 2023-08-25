#!/usr/bin/env lua
--taken from the tests at https://github.com/stravant/LuaMinify
local parser = require 'parser'
local version = _VERSION:match'^Lua (.*)$'

-- This is due to my own luajit compiling with 5.2 compat...
-- But this begs the question, are there any flags testable when compiling luajit with LUAJIT_ENABLE_LUA52COMPAT ?
-- https://love2d.org/forums/viewtopic.php?t=83187&sid=d189c94812b6f7ed4ebf8b2253b39828
-- Seems you can test 'goto' for this
-- But load vs loadstring ... in 5.1 load() expects a function and loadstring() expects a string
-- While in 5.2 (or luajit + 5.2 compat?) load() accepts strings as well.
-- Which means I'll have to use loadstring() in the validation test ...
-- Mind you in Lua 5.3 that loadstring() no longer exists and this will produce an error.
if version == '5.1' then
	--[[
	if not pcall(loadstring'local goto=1') then
		version = '5.2'
	end
	--]]
	-- but then again, when it comes to luajit, the first test passes/fails between 2.0.5 and 2.1.0-beta3 (I think with/without LUAJIT_ENABLE_LUA52COMPAT...)
	-- but ::a:: goto a works in both my versions of LuaJIT.
	-- so instead I'll just upgrade all luajit versions:
	-- [[
	if jit then version = '5.2' end
	--]]
end

print(version)
for line in io.lines'minify_tests.txt' do
	local expected = not line:match('FAIL_'..version)
	local luaResults = not not (loadstring or load)(line)
	assert(expected == luaResults, "test failed: your Lua version doesn't match the baseline:\nline "..line.." expected "..tostring(expected).." but got "..tostring(luaResults))
	local parserResults, errorString = xpcall(function()
		return parser.parse(line)
	end, function(err)
		return err..'\n'..debug.traceback()
	end)
	print(expected, parserResults, line)
	if expected ~= parserResults then error("parser failed to recreate identical string.  error="..errorString) end
end
