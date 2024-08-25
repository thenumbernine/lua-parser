#!/usr/bin/env luajit
local asserteq = require 'ext.assert'.eq
local Parser = require 'parser'

local function test(codein, eq)
	local codeout = ''..Parser.parse(codein)
	print(codein, codeout)
	local s = assert(load(codeout))()	-- evaluate it ...
	asserteq(s, eq)							-- assert it's correct
end

-- parse dec escape code, since 5.1
if _VERSION >= 'Lua 5.1' then
	test([[return '\97']], 'a')
end

-- parse hex escape code, since 5.2
if _VERSION >= 'Lua 5.2' then
	test([[return '\x62']], 'b')	-- don't test same as before, in case false positives
	test([[return '\x7a']], 'z')	-- make sure to test hex chars
end

-- parse unicode, since 5.3
if _VERSION >= 'Lua 5.3' then
	test([[return '\u{2200}']], '∀')
	test([[return '\u{2a01}']], '⨁')
end
