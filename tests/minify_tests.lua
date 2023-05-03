#!/usr/bin/env lua
--taken from the tests at https://github.com/stravant/LuaMinify
local parser = require 'parser'
local version = _VERSION:match'^Lua (.*)$'
if version == '5.1' and jit then version = '5.2' end
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
