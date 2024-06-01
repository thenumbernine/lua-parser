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

local function tab(s)
	return s:gsub('\n', '\n\t')
end


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

function GrammarASTNode:getcode(parser)
	error("need to handle grammar type "..tolua(self.type).." "..tolua(self))
end

-- how many places really use this?  and why not just search for underscore fields?
ast.allclasses = table()

local function nodeclass(type)
	local cl = GrammarASTNode:subclass()
	cl.type = type
	ast['_'..type] = cl
	ast.allclasses:insert(cl)
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


-- :getcode(parser) will generate the code for inserting into the current created node,
-- the current created node is assumed to be named 'result'

local _or = nodeclass'or'
function _or:getcode(parser)
	return template([[
-- or
-- TODO push rewind point here?
repeat
<? for _,child in ipairs(node) do
?>
	local oldlen = #result
	do
		<?=tab(tab(child:getcode(parser)))?>
	end
	if #result == oldlen then
		-- nothing was added? then break.
		-- TODO rewind token here?
		break
	end
<? end
?>until true]],
	{
		node = self,
		parser = parser,
		tab = tab,
	})
end

local _optional = nodeclass'optional'
-- optional is only different in that, after the optional code, we don't need any assert()'s / mustbe()'s
function _optional:getcode(parser)
	assertlen(self, 1)
	return self[1]:getcode(parser)
end

local _multiple = nodeclass'multiple'
function _multiple:getcode(parser)
	return template([[
-- multiple
repeat
<?
for i,child in ipairs(node) do
	local chsrc = child
	if ast._optional:isa(chsrc) then
		chsrc = chsrc[1]
	end
?>
	local oldlen = #result
	do
		<?=tab(tab(chsrc:getcode(parser)))?>	<? -- multiple always canbe, ... or is it? ?>
	end
	if #result == oldlen then
		-- didn't get anything
		-- TODO a token rewind maybe?
		break
	end
<?
end
?>until false]],
	{
		node = self,
		parser = parser,
		ast = ast,
		tab = tab,
	})
end

-- expr just encapsulates multiple child exprs?  hmm seems it does close to nothing.
local _expr = nodeclass'expr'
function _expr:getcode(parser)
	if #self == 1 then return self[1]:getcode(parser) end
	return template([[
-- expr
<? for i,child in ipairs(node) do
	local chsrc = child
	if ast._optional:isa(chsrc) then
		chsrc = chsrc[1]
	end
?><?=chsrc:getcode(parser)?>
<?end
?>]],
	{
		node = self,
		parser = parser,
		ast = ast,
		tab = tab,
	})
end

local _capture = nodeclass'capture'
function _capture:getcode(parser)
	return '-- capture'
end

local _name = nodeclass'name'
function _name:getcode(parser)
	assertlen(self, 1)
	local name = asserttype(self[1], 'string')
	assertindex(parser.ruleForName, name)
	return 'result:insert(self:parse_'..name..'())'
end

local _string = nodeclass'string'
function _string:getcode(parser)
	assertlen(self, 1)
	local s = asserttype(self[1], 'string')
	-- keyword / symbol
	-- TODO this should be 'mustbe' unless its parent is 'optional' or 'multiple' ...
	-- or maybe don't make that change here, but make it in the parent node that generates this code ...
	if parser.langKeywords[s] then
		return "self:canbe('"..s.."', 'keyword')"
	elseif parser.langSymbols[s] then
		return "self:canbe('"..s.."', 'symbol')"
	else
		error("found a string that wasn't a keyword or a symbol: "..tolua(s))
	end
end

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

local ast = {}
ast.allclasses = table()

local <?=rootASTClassName?> = ASTNode:subclass()

local function nodeclass(args, parent)
	parent = parent or <?=rootASTClassName?>
	local cl = parent:subclass(args)
	ast['_'..cl.type] = cl
	ast.allclasses:insert(cl)
	return cl
end

<? for _,rule in ipairs(rules) do
?>local _<?=rule.name?> = nodeclass{type=<?=tolua(rule.name)?>}
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
	local result = ast._<?=rule.name?>()
	<?=tab(rule.expr:getcode(self))?>
	return result
end
<? end ?>
]=], {
		-- requires above
		table = table,
		tolua = tolua,
		-- self
		self = self,
		-- locals
		tab = tab,
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
		elseif self:canbe('(', 'symbol') then
			local expr2 = self:parseExprOr()
			self:mustbe(')', 'symbol')
			expr:insert(ast._capture(expr2))
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
