#!/usr/bin/env lua
require 'ext'
local LuaParser = require 'parser.lua.parser'

--[[
local code = [[local result = aa and bb]]
--]]
-- [[
local code = path'../lua/parser.lua':read()
--]]
local parser = LuaParser(code, nil, code)

local tree = parser.tree
local datareader = parser.t.r

-- TODO this but for every test in minify_tests.txt
-- then verify the :lua() serialized results match the source results
local function printspan(x, tab)
	tab = tab or ''
	if x.type then
		local reconstructed = tostring(x)
		print(tab..'tostring():', reconstructed)
		local fromIndexSpan = code:sub(x.span.from.index, x.span.to.index)
		print(tab..'span substr:', fromIndexSpan)
		local fromTokenSpan = datareader.tokenhistory:sub(x.span.from.tokenIndex, x.span.to.tokenIndex):concat()
		print(tab..'token range: '..x.span.from.tokenIndex..', '..x.span.to.tokenIndex)
		print(tab..'token substr:', fromTokenSpan)
		print(tab..'type:', x.type)

		--[[
		local reconstructedCode = load(reconstructed):dump()
		local fromIndexSpanCode = load(fromIndexSpan):dump()
		local fromTokenSpanCode = load(fromTokenSpan):dump()
		assert.eq(reconstructedCode:hexdump(), fromIndexSpanCode:hexdump())
		assert.eq(reconstructedCode:hexdump(), fromTokenSpanCode:hexdump())
		--]]
		--[[
		local function reduceString(s)
			-- remove comments too, those will be in tokenSpan text
			s = s:gsub('%-%-[^\n]*', '')
			repeat
				local start1, start2 = s:find('%-%-%[=*%[')
				if not start1 then break end
				local eq = s:sub(start1+3, start2-1)
				assert(eq:match'^=*$')
				local finish1, finish2 = s:find('%]'..eq..'%]', start2)
				if not finish1 then break end
				s = s:sub(1, start1-1)..s:sub(finish2+1)
			until false
			s = s:gsub('%s+', ''):gsub('["\']', "'")
			return s
		end
		reconstructed = reduceString(reconstructed)
		fromIndexSpan = reduceString(fromIndexSpan)
		fromTokenSpan = reduceString(fromTokenSpan)
		assert.eq(reconstructed, fromIndexSpan)
		assert.eq(reconstructed, fromTokenSpan)
		--]]
	end
	for k,v in pairs(x) do
		if k == 'span' then
			print(tab..k..' = index range '..tostring(v.from.index)..'..'..tostring(v.to.index)
				..', line/col range '..v.from.line..'/'..v.from.col..'..'..v.to.line..'/'..v.to.col)
		elseif k ~= 'parent'
		and k ~= 'span'
		and k ~= 'parser'
		then
			if type(v) == 'table' then
				print(tab..k)
				printspan(v, tab..'  ')
			else
				print(tab..k..' = '..tostring(v))--tolua(v))
			end
		end
	end
end

printspan(tree)
