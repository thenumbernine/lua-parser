--[[
building a parser from a grammar ...
grammar syntax:

::= is used to define an AST node with the name `name`
block ::= chunk

| means multiple optional rules
binop ::= `+` | `-`

{} means zero-or-more multiple optional rules

[] means a single optional rule

``  means a keyword / symbol ... notice keywords are alphabetic only and symbols are non-alphabetic only. The distinction is to enforce that keywords cannot neighbor one another while symbols can, and though keywords are legal variable names (while symbols are not), variables must be checked to ensure they are not keywords. not sure how I'll mix these ...

grammar implementation:
1) scan all rules for all literal strings/keywords.  sort them all by size, largest-to-smallest. 
2) need to explicitly define some axiom rules. 
	For Lua: Name, Numeral, LiteralString 

--]]
local table = require 'ext.table'
local Tokenizer = require 'parser.tokenizer'
local Parser = require 'parser.parserbase'


local GrammarTokenizer = Tokenizer:subclass()

function GrammarTokenizer:initSymbolsAndKeywords()
	for w in ([[ ::= | ; { } [ ] ]]):gmatch('%S+') do
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
		if not self.t.token then break end	-- nothing left = done
		
		local rule = self:parseRule()
		if not rule then break end
		
		self:canbe(';', 'symbol')
		self.rules:insert(rule)
	until false
end

-- TODO the real issue with exprOr's multiple-expr's is rule here
-- there's nothing to distinguish the next expression in a list from a rule name
-- except maybe two newlines ... then i'd have to rework my whitespace parser ...
function GrammarParser:parseRule()
	local name = self:mustbe(nil, 'name')
	self:mustbe('::=', 'symbol')
	local expr = self:parseExprOr()
print('got rule', name, require 'ext.tolua'(expr))
	return expr
end

function GrammarParser:parseExprOr()
	local expr = self:parseExprList()
	local orexpr

	if self:canbe('|', 'symbol') then
		local expr2 = self:parseExprOr()
		if not orexpr then
			orexpr = table{'or', expr}
			expr = orexpr
		end
		if expr2[1] == 'or' then
			-- merge or's
			orexpr:append(expr2:sub(2))
		else
			orexpr:insert(expr2)
		end
	end
	return expr
end

function GrammarParser:parseExprList()
	local expr = table{'expr'}
	repeat
		if self:canbe('{', 'symbol') then
			local expr2 = self:parseExprOr()
			self:mustbe('}', 'symbol')
			expr:insert(table{'multiple', expr2})
		elseif self:canbe('[', 'symbol') then
			local expr2 = self:parseExprOr()
			self:mustbe(']', 'symbol')
			expr:insert(table{'optional', expr2})
		elseif self:canbe(nil, 'name') then
			expr:insert{'name', self.lasttoken}
		elseif self:canbe(nil, 'number') then
			expr:insert{'number', self.lasttoken}
		elseif self:canbe(nil, 'string') then
			expr:insert{'string', self.lasttoken}
		else
			break
		end	
	until false
	return expr
end

-- [[ test:
local path = require 'ext.path'
local syntax51 = GrammarParser(path'syntax_5.1.txt':read())
--]]

return GrammarParser
