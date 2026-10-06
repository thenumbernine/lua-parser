#!/usr/bin/env lua
require 'ext'
local LuaParser = require 'parser.lua.parser'

--[=[
local code = [[
local result = aa and bb
x = 1
y = 2
z = x + y
function h()
	print'hello world'
	return 42
end
]]
--]=]
-- [=[
local code = [[
function f() end
function g() end
function h() end
]]
--]=]
--[[
local code = path'../lua/parser.lua':read()
--]]
local parser = LuaParser(code, code)

local tree = parser.tree
local datareader = parser.t.r

-- TODO this but for every test in minify_tests.txt
-- then verify the :lua() serialized results match the source results
local function printspan(x, tab)
	tab = tab or ''
	if x.type then
		local reconstructed = x:toLua()
		print(tab..'tostring():', string.trim(reconstructed))
		local fromIndexSpan = code:sub(x.spanFrom.index, x.spanTo.index)
		print(tab..'span substr:', tolua(fromIndexSpan))
		local fromTokenSpan = datareader.data:sub(datareader.tokenhistory[1+2*x.spanFrom.tokenIndex], datareader.tokenhistory[2+2*x.spanTo.tokenIndex])
		print(tab..'token range: '..x.spanFrom.tokenIndex..', '..x.spanTo.tokenIndex)
		print(tab..'token substr:', tolua(fromTokenSpan))
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
		if k == 'spanFrom' then
			print(tab..'span = index range '..tostring(x.spanFrom.index)..'..'..tostring(x.spanTo.index)
				..', line/col range '..x.spanFrom.line..'/'..x.spanFrom.col..'..'..x.spanTo.line..'/'..x.spanTo.col)
		elseif k == 'spanTo' then
		elseif k ~= 'parent'
		and not (k == 'spanFrom' or k == 'spanTo')
		and k ~= 'parser'
		then
			if type(v) == 'table' then
				print(tab..k)
				printspan(v, tab..'  ')
			else
				print(tab..k..' = '..(v.toLua and v:toLua() or tostring(v)))--tolua(v))
			end
		end
	end
end

printspan(tree)
