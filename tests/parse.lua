#!/usr/bin/env lua
require'ext'
local parser = require 'parser'
local tree = parser.parse(io.readfile(assert(..., "expected filename")))
print(tree)
