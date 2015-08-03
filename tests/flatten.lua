#!/usr/bin/env lua

local tolua = require 'ext.tolua'
local parser = require 'parser'

local gcode = [[function g() return print'hi' end]]
local fcode = [[return function() g() end]]
local code = gcode..'\n'..fcode

print('original code')
print(code)
print()

local ftree = parser.parse(fcode)
print('f code')
print(tolua(ftree))
print('f ast code (should match original code)')
print(ftree)
print()

local gtree = parser.parse(gcode)
print('g code')
print(tolua(gtree))
print('g ast code')
print(gtree)
print()

local fflat = ftree:flatten{
	g=unpack(gtree),	-- TODO gtree:find'g' to look for global-level definitions?
}
print('flattened f ast')
print(tolua(fflat))
print('flattened f code')
print(fflat)
