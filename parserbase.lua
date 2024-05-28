--[[
TODO rename parser => luaparser and this to just 'parser'
but then the lua parser is require 'parser.luaparser' ... hmmm
--]]
local class = require 'ext.class'
local tolua = require 'ext.tolua'
local ast = require 'parser.ast'

local Parser = class()

-- seems redundant. does anyone need to construct a Parser without data? maybe to modify the syntax or something?  just build a subclass in that case?
function Parser:init(data)
	if data then
		self:setData(data)
	end
end

function Parser:setData(data)
	assert(data, "expected data")
	data = tostring(data)
	local t = self:buildTokenizer(data)
	t:start()
	self.t = t

	-- default entry point for parsing data sources
	assert(xpcall(function()
		self.tree = self:parseTree()
	end, function(err)
		return err..'\n'
			..self.t:getpos()..'\n'
			..debug.traceback()
	end))

	-- now that we have the tree, build parents
	-- ... since I don't do that during construction ...
	ast.refreshparents(self.tree)

	if self.t.token then
		error("unexpected "..self.t.token)
	end
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

function Parser:mustbe(token, tokentype)
	local lasttoken, lasttokentype = self.t.token, self.t.tokentype
	self.lasttoken, self.lasttokentype = self:canbe(token, tokentype)
	if not self.lasttoken then
		error("expected token="..tolua(token).." tokentype="..tolua(tokentype).." but found token="..tolua(lasttoken).." type="..tolua(lasttokentype))
	end
	return self.lasttoken, self.lasttokentype
end

return Parser 
