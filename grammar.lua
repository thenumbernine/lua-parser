--[[
building a parser from a grammar ...
grammar syntax:

::= is used to define an AST node with the name `name`
block ::= chunk

| means multiple optional rules
binop ::= `+` | `-`

{} means multiple optional rules

[] means a single optional rule

``  means a keyword / symbol ... notice keywords are alphabetic only and symbols are non-alphabetic only. The distinction is to enforce that keywords cannot neighbor one another while symbols can, and though keywords are legal variable names (while symbols are not), variables must be checked to ensure they are not keywords. not sure how I'll mix these ...

grammar implementation:
1) scan all rules for all literal strings/keywords.  sort them all by size, largest-to-smallest. 
2) need to explicitly define some axiom rules. 
	For Lua: Name, Numeral, LiteralString 

--]]
local Tokenizer = require 'parser.tokenizer'
local Parser = require 'parser.parserbase'

local GrammarTokenizer = Tokenizer:subclass()

function GrammarTokenizer:initSymbolsAndKeywords()
	for w in ([[::= | { } [ ]]]):gmatch('%S+') do
		self.symbols:insert(w)
	end
end

local GrammarParser = Parser:subclass()

function GrammarParser:buildTokenizer(data)
	return GrammarTokenizer(data)
end

function GrammarParser:parseTree()
	self.rules = table()
	repeat
		local rule = self:parseRule()
		if not rule then break end
		self.rules:insert(rule)
	until false
end

function GrammarParser:parseRule()
	local name = self:mustbe(nil, 'name')
end

return GrammarParser
