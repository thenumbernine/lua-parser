#!/usr/bin/env lua
local Parser = require 'ast.parser'
local parser = Parser{filename=...}
local tree = parser.tree
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
