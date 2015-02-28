#!/usr/bin/env lua

local ast = require 'ast'

local gcode = [[function g() return print'hi' end]]
local fcode = [[return function() g() end]]
local code = gcode..'\n'..fcode

print('original code')
print(code)
print()

local ftree = ast.parse(fcode)
print('f code')
print(toLua(ftree, '', ' '))
print('f ast code (should match original code)')
print(ftree)
print()

local gtree = ast.parse(gcode)
print('g code')
print(toLua(gtree, '', ' '))
print('g ast code')
print(gtree)
print()

local fflat = ast.flatten(ftree, {
	g=unpack(gtree),	-- TODO gtree:find'g' to look for global-level definitions?
})
print('flattened f ast')
print(toLua(fflat, '', ' '))
print('flattened f code')
print(fflat)
