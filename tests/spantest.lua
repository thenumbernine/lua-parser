#!/usr/bin/env lua
local parser = require 'parser'
local tolua = require 'ext.tolua'

local code = [[local result = aa and bb]]
local tree = parser.parse(code)

-- TODO this but for every test in minify_tests.txt
-- then verify the :lua() serialized results match the source results
local function printspan(x, tab)
	tab = tab or ''
	if x.type then
		print(tab..'tostring():', tolua(tostring(x)))
		print(tab..'span substr:', tolua(code:sub(x.span.from.index, x.span.to.index)))
		print(tab..'type:', x.type)
	end
	for k,v in pairs(x) do
		if k == 'span' then
			print(tab..k..' = '..tostring(v.from.index)..'..'..tostring(v.to.index))
		elseif k ~= 'parent' then
			if type(v) == 'table' then
				print(tab..k)
				printspan(v, tab..'  ')
			else
				print(tab..k..' = '..tolua(v))
			end
		end
	end
end

printspan(tree)
