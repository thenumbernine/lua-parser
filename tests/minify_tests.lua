#!/usr/bin/env lua5.1
--taken from the tests at https://github.com/stravant/LuaMinify
local parser = require 'parser'
local version = assert(_VERSION:match('^Lua (.*)$'), "failed to find the lua version")
print('version',version)
for line in io.lines'minify_tests.txt' do
	local expected = not line:match('FAIL_'..version)
	local luaResults = not not (loadstring or load)(line)
	assert(expected == luaResults, "looks like your Lua version doesn't match the baseline:\nline "..line.." expected "..tostring(expected).." but got "..tostring(luaResults))
	local parserResults, errorString = xpcall(function() return parser.parse(line, version) end, function(err)
		return err..'\n'..debug.traceback()
	end)
	print(expected,parserResults,line)
	if expected ~= parserResults then print("parser failed to recreate identical string.  error="..errorString) end
end
