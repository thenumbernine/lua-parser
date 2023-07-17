#!/usr/bin/env lua
require'ext'
local parser = require 'parser'
local tree = parser.parse(path(assert(..., "expected filename")):read())
print(tree)
