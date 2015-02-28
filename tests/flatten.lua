#!/usr/bin/env lua

local ast = require 'ast'
local parser = require 'ast.parser'

local gcode = [[function g() return print'hi' end]]
local fcode = [[return function() g() end]]
local code = gcode..'\n'..fcode

print('original code')
print(code)

print('ast')
local ftree = parser(fcode).tree
print('f code')
print(toLua(ftree, '', ' '))
print('f ast code (should match original code)')
print(ftree)
local gtree = parser(gcode).tree
print('g code')
print(toLua(gtree, '', ' '))
print('g ast code')
print(gtree)
print()

local fflat = ast.flatten(ftree, {
	g=unpack(gtree.stmts),	-- TODO gtree:find'g'
})
print('flattened f ast')
print(toLua(fflat, '', ' '))
print('flattened f code')
print(fflat)
