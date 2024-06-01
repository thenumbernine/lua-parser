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
local assertlen = require 'ext.assert'.len
local asserttype = require 'ext.assert'.type
local assertindex = require 'ext.assert'.index
local tolua = require 'ext.tolua'
local template = require 'template'
local GrammarTokenizer = require 'parser.grammar.tokenizer'
local Parser = require 'parser.base.parser'


-- all grammar ast classes, key'd by rule-name
local ast = {}

-- hmm ... move this to ASTNode root eventually
ast.refreshparents = require 'parser.lua.ast'.refreshparents

-- TODO for these rules (and for the rules that GrammarParser code-generates)
-- I might as well create AST objects and override their :getcode() instead of making all the if/else conditions in GrammarParser:getcode()

local ASTNode = require 'parser.base.ast'


local GrammarASTNode = ASTNode:subclass()

function GrammarASTNode:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end

GrammarASTNode.insert = table.insert
GrammarASTNode.append = table.append

local function nodeclass(type)
	local cl = GrammarASTNode:subclass()
	cl.type = type
	ast['_'..type] = cl
	return cl
end

local _rule = nodeclass'rule'
-- how to alias?
_rule.__index = function(self, k)
	if k == 'name' then return self[1] end
	if k == 'expr' then return self[2] end
	--return _rule.super.__index(self, k)
	return _rule.super.__index[k]
end


local _expr = nodeclass'expr'
local _or = nodeclass'or'
local _multiple = nodeclass'multiple'
local _optional = nodeclass'optional'

local _name = nodeclass'name'
local _number = nodeclass'number'
local _string = nodeclass'string'

--[[ hmm why does this get errors about {"stat"} ...
_name.__index = function(self, k)
	if k == 'value' then return self[1] end
	return _name.super.__index[k]
end

_number.__index = function(self, k)
	if k == 'value' then return self[1] end
	return _number.super.__index[k]
end

_string.__index = function(self, k)
	if k == 'value' then return self[1] end
	return _string.super.__index[k]
end
--]]


local GrammarParser = Parser:subclass()
GrammarParser.ast = ast

-- static method, call with :
function GrammarParser:fromFile(fn)
	return self(assert(path(fn):read()), fn)
end

function GrammarParser:buildTokenizer(data)
	return GrammarTokenizer(data)
end

local Rule = class()

local function tab(s)
	return s:gsub('\n', '\n\t')
end

-- TODO
-- hmm maybe a better design is to build the node obj, then just add subexprs to it as we go ... no distinguishing of parts by this code ...
-- and then maybe add to the grammar ( ) to capture parts into separate sub-tables ?
-- and then add to the grammar key= named/alias'd fields?
function GrammarParser:getcode(node, mustbe)
	local function temp(s)
		return tab(template(s, {
			ast = ast,
			tolua = tolua,
			self = self,
			node = node,
			tab = tab,
		}))
	end

	if ast._or:isa(node) then	-- a or b or c or d ...
		return temp[[
-- or
(function()
<? for _,child in ipairs(node) do
?>	local node = <?=tab(self:getcode(child))?>
	if node then return node end
<? end
?>end)()]]
	elseif ast._optional:isa(node) then	-- optinally a
		assertlen(node, 1)
		return self:getcode(node[1])
		-- optional is only different in that, after the optional code, we don't need any assert()'s / mustbe()'s
	elseif ast._multiple:isa(node) then 	-- a a a a ...
		return temp[[
(function()	-- multiple
	local result = table()
	repeat
<? for i,child in ipairs(node) do
		local chsrc = child
		if ast._optional:isa(chsrc) then
			chsrc = chsrc[1]
		end
?>
		local node = <?=tab(self:getcode(chsrc))?>	<? -- multiple always canbe, ... or is it? ?>
		if not node then break end	<? -- TODO a token rewind maybe? ?>
<?
		if true then -- not ast._string:isa(chsrc) then -- don't keep
?>		result:insert(node)
<?		end
	end
?>	until false
	return result
end)()]]
	elseif ast._expr:isa(node) then		-- list of strings / rules, process list sequentially
		if #node == 1 then
			return self:getcode(node[1])
		else
			return temp[[
(function()	-- expr
	local result = table()
<? for i,child in ipairs(node) do
	local argcode = tab(self:getcode(child))
	local chsrc = child
	if ast._optional:isa(chsrc) then
		chsrc = chsrc[1]
	else
		argcode = 'assert('..argcode..')'
	end
	if false then -- ast._string:isa(chsrc) then	-- don't keep
?>	<?=argcode?>	-- nocapture <?=chsrc.type?>
<?	else	-- keep
?>	result:insert((<?=argcode?>)) -- <?=chsrc.type?>
<?	end
end
?>	return result
end)():unpack()]]
		end

	-- for symbols and keywords when do we want to keep them in the AST, vs when do we want to just consume them?
	elseif ast._name:isa(node) then
		assertlen(node, 1)
		local name = asserttype(node[1], 'string')
		assertindex(self.ruleForName, name)
		return 'self:parse_'..name..'()'
	elseif ast._string:isa(node) then
		assertlen(node, 1)
		local s = asserttype(node[1], 'string')
		-- keyword / symbol
		-- TODO this should be 'mustbe' unless its parent is 'optional' or 'multiple' ...
		-- or maybe don't make that change here, but make it in the parent node that generates this code ...
		if self.langKeywords[s] then
			return "self:canbe('"..s.."', 'keyword')"
		elseif self.langSymbols[s] then
			return "self:canbe('"..s.."', 'symbol')"
		else
			error("found a string that wasn't a keyword or a symbol: "..tolua(s))
		end
	else
		error("need to handle grammar type "..tolua(node.type).." "..tolua(node))
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
		assertlen(rule, 2)
		self.ruleForName[rule.name] = rule
	end

	-- while we're here, traverse all rules and pick out all symbols and keywords
	self.langKeywords = {}
	self.langSymbols = {}
	local function process(node)
		if ast._name:isa(node) then
			assertlen(node, 1)
			-- names in the grammar should always point to either other rules, or to builtin axiomatic rules (Name, Numeric, LiteralString)
			local name = asserttype(node[1], 'string')
			local rule = self.ruleForName[name]
			if not rule then
				error("rule referenced but not defined: "..tolua(name))
			end
			-- TODO replace the element in the table with the AST? that'd remove the DAG property of the AST.  no more pretty `tolua()` output.
		elseif ast._string:isa(node) then
			assertlen(node, 1)
			local s = asserttype(node[1], 'string')

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

		for i,child in ipairs(node) do
			if type(child) == 'table' then
				process(child)
			end
		end
	end
	for _,rule in ipairs(self.tree) do
		process(rule)
	end

	-- At this point I'm torn
	-- Should I initialize the Tokenizer & Parser classes here, and therefore require a GrammarParser to be run every time the class is initialized?
	-- Seems like a needless amount of work, but it happens pretty quickly.
	-- Or should I code-generate the Tokenizer & Parser?
	-- Downside to codegen is you potentially lose access to the source material.
	-- Meh, I can just tolua() it in the output if I really want it.

	local rootASTClassName = 'LuaASTNode'
	local tokenizerClassName = 'LuaTokenizer'
	local parserClassName = 'LuaParser'

	print(template([=[
-- generated by 'parser.grammar' using file "<?=source?>"
local table = require 'ext.table'
local ASTNode = require 'parser.base.ast'
local Tokenizer = require 'parser.base.tokenizer'


local <?=rootASTClassName?> = ASTNode:subclass()

local ast = {}
<? for _,rule in ipairs(rules) do
?>ast._<?=rule.name?> = <?=rootASTClassName?>:subclass{type=<?=tolua(rule.name)?>}
<? end 
?>

local <?=tokenizerClassName?> = Tokenizer:subclass()

<?=tokenizerClassName?>.symbols = table(<?=tolua(table.keys(self.langSymbols))?>)
<?=tokenizerClassName?>.keywords = <?=tolua(self.langKeywords)?>

function <?=parserClassName?>:buildTokenizer(data)
	return <?=tokenizerClassName?>(data)
end

function <?=parserClassName?>:parseTree()
	return <?=parserClassName?>:parse_<?=rules[1][1]?>()
end

<? for _,rule in ipairs(rules) do ?>
function <?=parserClassName?>:parse_<?=rule.name?>()
	return ast._<?=rule.name?>(<?=self:getcode(rule.expr)?>)
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
		rootASTClassName = rootASTClassName,
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

	-- can-be + capture + assign 'name'
	local name = self:mustbe(nil, 'name')

	-- must-be + ignore ... do we ever want to capture a must-be? maybe?
	self:mustbe('::=', 'symbol')

	-- TODO i'm overusing and improperly using the term 'expr'
	-- can-be + capture + assign 'expr'
	local expr = self:parseExprOr()
--print('got rule', name, tolua(expr))

	return ast._rule(name, expr)
end

function GrammarParser:parseExprOr()
	local expr = self:parseExprList()
	local orexpr

	if self:canbe('|', 'symbol') then
		local expr2 = self:parseExprOr()
		if not orexpr then
			orexpr = ast._or(expr)
			expr = orexpr
		end
		if ast._or:isa(expr2) then
			-- merge or's
			for i,child in ipairs(expr2) do
				orexpr:insert(child)
			end
		else
			orexpr:insert(expr2)
		end
	end
	return expr
end

function GrammarParser:parseExprList()
	local expr = ast._expr()
	repeat
		if self:canbe('{', 'symbol') then
			local expr2 = self:parseExprOr()
			--assert(not ast._multiple:isa(expr2)) -- no {{ }} allowed, just { }
			self:mustbe('}', 'symbol')
			expr:insert(ast._multiple(expr2))
		elseif self:canbe('[', 'symbol') then
			local expr2 = self:parseExprOr()
			self:mustbe(']', 'symbol')
			expr:insert(ast._optional(expr2))
		elseif self:canbe(nil, 'name') then
			expr:insert(ast._name(self.lasttoken))
		elseif self:canbe(nil, 'number') then
			expr:insert(ast._number(self.lasttoken))
		elseif self:canbe(nil, 'string') then
			expr:insert(ast._string(self.lasttoken))
		else
			break
		end
	until false
	-- unwrap
	while #expr == 1 and ast._expr:isa(expr) do
		expr = expr[1]
	end
	return expr
end

-- [[ test:
local syntax51 = GrammarParser:fromFile'syntax_ast_5.1.txt'
--]]

return GrammarParser
