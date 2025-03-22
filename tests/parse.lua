#!/usr/bin/env lua
local path = require 'ext.path'
local parser = require 'parser'
local tree = assert(parser.parse(path(assert(..., "expected filename")):read()))
print(tree:toLua())
