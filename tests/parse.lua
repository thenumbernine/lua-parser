#!/usr/bin/env lua
local ast = require 'ast'
local tree = ast.parse(io.readfile(assert(..., "expected filename")))
print(tree)

local function process(n)
	if type(n) ~= 'table' then return end
	local m = getmetatable(n)
	if m then n.type = m.type end
	for k,v in pairs(n) do
		process(v)
	end
end
process(tree)
