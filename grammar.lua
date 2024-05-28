--[[
building a parser from a grammar ...
grammar syntax:

::= is used to define an AST node with the name `name`
block ::= chunk

| means multiple optional rules
binop ::= `+` | `-`

{} means zero-or-more multiple optional rules

[] means a single optional rule

''  means a keyword / symbol ... notice keywords are alphabetic only and symbols are non-alphabetic only. The distinction is to enforce that keywords cannot neighbor one another while symbols can, and though keywords are legal variable names (while symbols are not), variables must be checked to ensure they are not keywords. not sure how I'll mix these ...

; means end-of-expression-list
	I was debating a few ways to distingish rule ends.  Options could be:
	- Wrap in { }
	- Use ; as a terminator
	- Prefix rules with "def" or something, because the end of an expression-list is either a | or a new rule.

Grammar implementation:
1) scan all rules for all literal strings/keywords.  sort them all by size, largest-to-smallest. 
2) need to explicitly define some axiom rules. 
	For Lua: Name, Numeral, LiteralString 

--]]
local table = require 'ext.table'
local asserteq = require 'ext.assert'.eq
local asserttype = require 'ext.assert'.type
local assertindex = require 'ext.assert'.index
local tolua = require 'ext.tolua'
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

function GrammarParser:setData(...)
	GrammarParser.super.setData(self, ...)

	-- now we should have our self.tree
	-- from here we can convert it into a parse structure
	-- our first rule will be the start, i.e. :parseTree()
	-- subsequent rules become member functions
	
	local rulesForName = {}
	-- builtin rules
	rulesForName.Name = true
	rulesForName.LiteralString = true
	rulesForName.Numeral = true
	for _,rule in ipairs(self.tree) do
print(tolua(rule))
		asserteq(rule[1], 'rule')
		local name = asserttype(rule[2], 'string')
		rulesForName[name] = rule
	end

	-- while we're here, traverse all rules and pick out all symbols and keywords
	local keywords = {}
	local symbols = {}
	local function process(node)
		if node[1] == 'name' then
			asserteq(#node, 2)
			-- names in the grammar should always point to either other rules, or to builtin axiomatic rules (Name, Numeric, LiteralString)
			local name = asserttype(node[2], 'string')
			assertindex(rulesForName, name)
		elseif node[1] == 'string' then
			asserteq(#node, 2)
			local s = asserttype(node[2], 'string')

			-- keywords vs symbols are parsed separately
			-- keywords must be space-separated, and for now are only letters -- no symbol characters used (allowed?)
			-- symbols don't have to be space-separated and for now cannot be letters
			if s:find'%A' then
				assert(not s:find'%a')
				symbols[s] = true
			else
				keywords[s] = true
			end
		end

		for i=2,#node do
			local child = node[i]
			if type(child) == 'table' then
				process(child)
			end
		end
	end
	for _,rule in ipairs(self.tree) do
		-- rule[2] is the rule name
		-- rule[3] is the expression AST node
		process(rule)
	end

	print('keywords', tolua(table.keys(keywords):sort():concat' '))
	print('symbols', tolua(table.keys(symbols):sort():concat' '))
end

function GrammarParser:parseTree()
	rules = table()
	repeat
		if not self.t.token then break end	-- nothing left = done
		
		local rule = self:parseRule()
		if not rule then break end
		
		self:canbe(';', 'symbol')
		rules:insert(rule)
	until false
	return rules
end

function GrammarParser:parseRule()
	local name = self:mustbe(nil, 'name')
	self:mustbe('::=', 'symbol')
	local expr = self:parseExprOr()
--print('got rule', name, tolua(expr))
	return table{'rule', name, expr}
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
