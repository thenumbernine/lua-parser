local class = require 'ext.class'
local table = require 'ext.table'
local tolua = require 'ext.tolua'

local Parser = class()

-- seems redundant. does anyone need to construct a Parser without data? maybe to modify the syntax or something?  just build a subclass in that case?
function Parser:init(data, ...)
	if data then
		assert(self:setData(data, ...))
	end
end

--[[
returns
	true upon success
	nil, msg, loc upon failure
--]]
function Parser:setData(data, source)
	assert(data, "expected data")
	data = tostring(data)
	self.source = source
	local t = self:buildTokenizer(data)
	self.t = t

	-- default entry point for parsing data sources
	local parseError
	local result = table.pack(xpcall(function()
		t:start()
		self.tree = self:parseTree()
	end, function(err)
		-- throw an object if it's an error parsing the code
		if type(err) == 'table' then
--DEBUG:print('got parse error:', require'ext.tolua'(err))
--DEBUG:print(debug.traceback())
			parseError = err
			return
		else
			return err..'\n'
				..self.t:getpos()..'\n'
				..debug.traceback()
		end
	end))
	if not result[1] then
		if not parseError then error(result[2]) end	-- internal error
		return false, self.t:getpos()..': '..tostring(parseError.msg) 	-- parsed code error
	end

	--
	-- now that we have the tree, build parents
	-- ... since I don't do that during construction ...
	if self.ast
	and self.ast.refreshparents
	then
		self.ast.refreshparents(self.tree)
	end

	if self.t.token then
		return false, self.t:getpos()..": expected eof, found "..self.t.token
	end
	return true
end

-- TODO I don't need all these, just :getloc()
function Parser:getloc()
	local loc = self.t:getloc()
	loc.source = self.source
	return loc
end

function Parser:canbe(token, tokentype)	-- token is optional
	assert(tokentype)
	if (not token or token == self.t.token)
	and tokentype == self.t.tokentype
	then
		self.lasttoken, self.lasttokentype = self.t.token, self.t.tokentype
		self.t:consume()
		return self.lasttoken, self.lasttokentype
	end
end

function Parser:mustbe(token, tokentype, opentoken, openloc)
	local lasttoken, lasttokentype = self.t.token, self.t.tokentype
	self.lasttoken, self.lasttokentype = self:canbe(token, tokentype)
	if not self.lasttoken then
		local msg = "expected token="..tolua(token).." tokentype="..tolua(tokentype)
			.." but found token="..tolua(lasttoken).." type="..tolua(lasttokentype)
		if opentoken then
			msg = msg .. " to close "..tolua(opentoken).." at line="..openloc.line..' col='..openloc.col
		end
		error{msg=msg}
	end
	return self.lasttoken, self.lasttokentype
end

-- make new ast node, assign it back to the parser (so it can tell what version / keywords / etc are being used)
function Parser:node(index, ...)
--DEBUG:print('Parser:node', index, ...)
	local node = self.ast[index](...)
	node.parser = self
	return node
end

-- used with parse_expr_precedenceTable
function Parser:getNextRule(rules)
	for _, rule in pairs(rules) do
		-- TODO why even bother separate it in canbe() ?
		local keywordOrSymbol = rule.token:match'^[_a-zA-Z][_a-zA-Z0-9]*$' and 'keyword' or 'symbol'
		if self:canbe(rule.token, keywordOrSymbol) then
			return rule
		end
	end
end

-- a useful tool for specifying lots of precedence level rules
-- used with self.parseExprPrecedenceRulesAndClassNames
-- example in parser/lua/parser.lua
function Parser:parse_expr_precedenceTable(i)
--DEBUG:print('Parser:parse_expr_precedenceTable', i, 'of', #self.parseExprPrecedenceRulesAndClassNames, 'token=', self.t.token)
	local precedenceLevel = self.parseExprPrecedenceRulesAndClassNames[i]
	if precedenceLevel.unaryLHS then
		local from = self:getloc()
		local rule = self:getNextRule(precedenceLevel.rules)
		if rule then
			local nextLevel = i
			if rule.nextLevel then
				nextLevel = self.parseExprPrecedenceRulesAndClassNames:find(nil, function(level)
					return level.name == rule.nextLevel
				end) or error{msg="couldn't find precedence level named "..tostring(rule.nextLevel)}
			end
			local a = assert(self:parse_expr_precedenceTable(nextLevel), {msg='unexpected symbol'})
			a = self:node(rule.className, a)
			if a.span then
				a:setspan{from = a.span.from, to = self:getloc()}
			end
			return a
		end

		if i < #self.parseExprPrecedenceRulesAndClassNames then
			return self:parse_expr_precedenceTable(i+1)
		else
			return self:parse_subexp()
		end
	else
		-- binary operation by default
		local a
		if i < #self.parseExprPrecedenceRulesAndClassNames then
			a = self:parse_expr_precedenceTable(i+1)
		else
			a = self:parse_subexp()
		end
		if not a then return end
		local rule = self:getNextRule(precedenceLevel.rules)
		if rule then
			local nextLevel = i
			if rule.nextLevel then
				nextLevel = self.parseExprPrecedenceRulesAndClassNames:find(nil, function(level)
					return level.name == rule.nextLevel
				end) or error{msg="couldn't find precedence level named "..tostring(rule.nextLevel)}
			end
			a = self:node(rule.className, a, (assert(self:parse_expr_precedenceTable(nextLevel), {msg='unexpected symbol'})))
			if a.span then
				a:setspan{from = a.span.from, to = self:getloc()}
			end
		end
		return a
	end
end



return Parser
