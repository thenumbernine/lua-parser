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
local path = require 'ext.path'
local table = require 'ext.table'
local class = require 'ext.class'
local asserteq = require 'ext.assert'.eq
local asserttype = require 'ext.assert'.type
local assertindex = require 'ext.assert'.index
local tolua = require 'ext.tolua'
local template = require 'template'
local Tokenizer = require 'parser.tokenizer'
local Parser = require 'parser.parserbase'


local GrammarTokenizer = Tokenizer:subclass()

function GrammarTokenizer:initSymbolsAndKeywords()
	for w in ([[ ::= | ; { } [ ] ]]):gmatch('%S+') do
		self.symbols:insert(w)
	end
end

local GrammarParser = Parser:subclass()

-- static method, call with :
function GrammarParser:fromFile(fn)
	return self(assert(path(fn):read()), fn)
end

function GrammarParser:buildTokenizer(data)
	return GrammarTokenizer(data)
end

local Rule = class()

function GrammarParser:getcode(node)
	local function tab(s)
		return s:gsub('\n', '\n\t')
	end

	if node[1] == 'or' then	-- a or b or c or d ...
		return template([[
-- or
(function()
<? for i=2,#node do 
	local child = node[i]
?>	local node = <?=tab(self:getcode(child))?>
	if node then return node end
<? end 
?>end)()]]
, 	{
			self = self,
			node = node,
			tab = tab,
		})
	elseif node[1] == 'optional' then	-- optinally a
		return template([[
-- optional
<?=tab(self:getcode(node[2]))
?>]],		{
			self = self,
			node = node,
			tab = tab,
		})
	elseif node[1] == 'multiple' then 	-- a a a a ...
		return template([[
-- multiple
(function()
	local result = table()
	repeat
<? for i=2,#node do 
	local child = node[i]
?>		local node = <?=tab(self:getcode(child))?>
		if not node then break end
		result:insert(node)
<? end
?>	until false
	return result
end)()]]
,		{
			tolua = tolua,
			self = self,
			node = node,
			tab = tab,
		})
	elseif node[1] == 'expr' then		-- list of strings / rules, process list sequentially
		return template([[
-- expr
(function()
	local result = table()
<? for i=2,#node do 
	local child = node[i]
?>	local node = <?=tab(self:getcode(child))?>
	assert(node)
	result:insert(node)
<? end
?>	return result
end)()]]
, 	{
			tolua = tolua,
			self = self,
			node = node,
			tab = tab,
		})
	elseif node[1] == 'name' then
		asserteq(#node, 2)
		local name = asserttype(node[2], 'string')
		assertindex(self.ruleForName, name)
		return 'self:parse_'..name..'()'
	elseif node[1] == 'string' then
		asserteq(#node, 2)
		local s = asserttype(node[2], 'string')
		-- keyword / symbol
		if self.langKeywords[s] then
			return "self:canbe('"..s.."', 'keyword')"
		elseif self.langSymbols[s] then
			return "self:canbe('"..s.."', 'symbol')"
		else
			error("found a string that wasn't a keyword or a symbol: "..tolua(s))
		end
	else
		error("need to handle grammar type "..tolua(node[1]))
	end
	return ''
end

function GrammarParser:setData(data, source, ...)
	GrammarParser.super.setData(self, data, source, ...)

	-- now we should have our self.tree
	-- from here we can convert it into a parse structure
	-- our first rule will be the start, i.e. :parseTree()
	-- subsequent rules become member functions
	
	self.ruleForName = {}
	-- builtin rules
	self.ruleForName.Name = true
	self.ruleForName.LiteralString = true
	self.ruleForName.Numeral = true
	for _,rule in ipairs(self.tree) do
		asserteq(#rule, 3)
		--[[ TODO should I convert from indexed to named fields here?
		asserteq(rule[1], 'rule')
		rule.name = rule[2]
		rule.expr = rule[3]
		rule[1] = nil
		rule[2] = nil
		rule[3] = nil
		--]]
		self.ruleForName[rule[2]] = rule
	end

	-- while we're here, traverse all rules and pick out all symbols and keywords
	self.langKeywords = {}
	self.langSymbols = {}
	local function process(node)
		local nodetype = node[1]
		if nodetype == 'name' then
			asserteq(#node, 2)
			-- names in the grammar should always point to either other rules, or to builtin axiomatic rules (Name, Numeric, LiteralString)
			local name = asserttype(node[2], 'string')
			local rule = self.ruleForName[name]
			if not rule then
				error("rule referenced but not defined: "..tolua(name))
			end
			-- TODO replace the element in the table with the AST? that'd remove the DAG property of the AST.  no more pretty `tolua()` output.
		elseif nodetype == 'string' then
			asserteq(#node, 2)
			local s = asserttype(node[2], 'string')

			-- keywords vs symbols are parsed separately
			-- keywords must be space-separated, and for now are only letters -- no symbol characters used (allowed?)
			-- symbols don't have to be space-separated and for now cannot be letters
			if s:find'%A' then
				assert(not s:find'%a')
				self.langSymbols[s] = true
			else
				self.langKeywords[s] = true
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

	-- At this point I'm torn
	-- Should I initialize the Tokenizer & Parser classes here, and therefore require a GrammarParser to be run every time the class is initialized?
	-- Seems like a needless amount of work, but it happens pretty quickly.
	-- Or should I code-generate the Tokenizer & Parser?
	-- Downside to codegen is you potentially lose access to the source material.
	-- Meh, I can just tolua() it in the output if I really want it.

	local tokenizerClassName = 'LuaTokenizer'
	local parserClassName = 'LuaParser'

	print(template([=[
-- generated by 'parser.grammar' using file "<?=source?>"

function <?=tokenizerClassName?>:initSymbolsAndKeywords()
	self.symbols = <?=tolua(table.keys(self.langSymbols))?>
	self.keywords = <?=tolua(self.langKeywords)?>
end

function <?=parserClassName?>:buildTokenizer(data)
	return <?=tokenizerClassName?>(data)
end

function <?=parserClassName?>:parseTree()
	return <?=parserClassName?>:parse_<?=rules[1].name?>()
end

<? for _,rule in ipairs(rules) do ?>
--[[
<?=tolua(rule[3])?>
--]]
function <?=parserClassName?>:parse_<?=rule[2]?>()
	return table{<?=tolua(rule[2])?>, <?=self:getcode(rule[3])?>}
end
<? end ?>
]=], {
		-- requires above
		table = table,
		tolua = tolua,
		-- self
		self = self,
		-- locals
		source = source,
		rules = self.tree,
		tokenizerClassName = tokenizerClassName,
		parserClassName = parserClassName,
	}))
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
			--assert(expr2[1] ~= 'multiple') -- no {{ }} allowed, just { }
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
local syntax51 = GrammarParser:fromFile'syntax_5.1.txt'
--]]

return GrammarParser
