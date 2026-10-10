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
		parseError = err:match'MSG:(.*)'
		if parseError then
--DEBUG(%5):print('got parse error:', parseError)
--DEBUG(%5):print(debug.traceback())
			return
		else
			return err..'\n'
				..self.t:getpos()..'\n'
				..debug.traceback()
		end
	end))
	if not result[1] then
		if not parseError then error(result[2]) end			-- internal error
		return false, self.t:getpos()..': '..parseError 	-- parsed code error
	end

	--
	-- now that we have the tree, build parents
	-- ... since I don't do that during construction ...
	if self.ast
	and self.ast.refreshparents
	then
		self.ast.refreshparents(self.tree)
	end

	if self.t.tokenFrom then
		return false, self.t:getpos()..": expected eof, found "..tostring(self.t:gettoken())
	end
	return true
end

function Parser:getloc()
	return self.t:getloc()
end

function Parser:canbe(reqToken, reqTokenType)	-- reqToken is optional
--DEBUG:assert(reqTokenType)
	if reqTokenType ~= self.t.tokenType then return end

	local thisToken = self.t:gettoken()
	if reqToken and reqToken ~= thisToken then return end

	self.lasttoken, self.lasttokentype = thisToken, self.t.tokenType
	self.t:consume()
	return true
end

function Parser:mustbe(reqToken, reqTokenType, opentoken, openloc)
	local t = self.t
	local lastTokenFrom, lastTokenTo, lastTokenType, lastTokenData
		= t.tokenFrom, t.tokenTo, t.tokenType, t.tokenData
	if not self:canbe(reqToken, reqTokenType) then
		-- same as Tokenzier:gettoken() but defer until failure
		local lastToken = lastTokenFrom and (lastTokenData or t.r.data):sub(lastTokenFrom, lastTokenTo)

		local msg = "expected token="..tolua(reqToken).." tokenType="..tolua(reqTokenType)
			.." but found token="..tolua(lastToken).." type="..tolua(lastTokenType)
		if opentoken then
			local line, col = self.t:getIndexLineCol(openloc)
			msg = msg .. " to close "..tolua(opentoken).." at line="..line..' col='..col
		end
		error('MSG:'..msg)
	end
	return true
end

-- make new ast node, assign it back to the parser (so it can tell what version / keywords / etc are being used)
function Parser:node(index, ...)
--DEBUG(@5):print('Parser:node', index, ...)
	local node = self.ast[index](...)

	-- TODO pass 'parser' into the ctor instead of doing this ...
	-- this is here so langfix loop nodes don't have to scan all their children
	node.parser = self
	if node.updateParser then node:updateParser() end

	return node
end

-- used with parse_expr_precedenceTable
function Parser:getNextRule(rules)
	for _, rule in pairs(rules) do
		-- TODO why even bother separate it in canbe() ?
		local keywordOrSymbol = rule.token:find'^[_a-zA-Z][_a-zA-Z0-9]*$' and 'keyword' or 'symbol'
		if self:canbe(rule.token, keywordOrSymbol) then
			return rule
		end
	end
end

-- a useful tool for specifying lots of precedence level rules
-- used with self.parseExprPrecedenceRulesAndClassNames
-- example in parser/lua/parser.lua
function Parser:parse_expr_precedenceTable(i)
--DEBUG(@5):print('Parser:parse_expr_precedenceTable', i, 'of', #self.parseExprPrecedenceRulesAndClassNames, 'token=', self.t:gettoken())
	local precedenceLevel = self.parseExprPrecedenceRulesAndClassNames[i]
	if precedenceLevel.unaryLHS then
		local from = self:getloc()
		local rule = self:getNextRule(precedenceLevel.rules)
		if rule then
			local nextLevel = i
			if rule.nextLevel then
				nextLevel = self.parseExprPrecedenceRulesAndClassNames:find(nil, function(level)
					return level.name == rule.nextLevel
				end) or error("MSG:couldn't find precedence level named "..tostring(rule.nextLevel))
			end
			local a = assert(self:parse_expr_precedenceTable(nextLevel), 'MSG:unexpected symbol')
			a = self:node(rule.className, a)
			if a.spanFrom then
				a:setspan(a.spanFrom, self:getloc())
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
				end) or error("MSG:couldn't find precedence level named "..tostring(rule.nextLevel))
			end
			a = self:node(rule.className, a, (assert(self:parse_expr_precedenceTable(nextLevel), 'MSG:unexpected symbol')))
			if a.spanFrom then
				a:setspan(a.spanFrom, self:getloc())
			end
		end
		return a
	end
end


return Parser
