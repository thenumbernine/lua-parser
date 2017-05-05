--[[
use this file to insert a parser hook into the require() function
from there, you can register callbacks to do whatever you want with the parsed Lua
--]]

local Parser = require 'parser'

local oldrequire = assert(require)

require = setmetatable({
	callbacks = setmetatable({}, {__index=table}),
	totalLines = 0,
	oldrequire = oldrequire, 
}, {
	__call = function(self, m, ...)
		local result = package.loaded[m]
		if result ~= nil then return result end
		local result = package.preload[m]
		local err = {"module '"..m.."' not found:"}
		if result ~= nil then
			result = result()
			package.loaded[m] = result
			return result
		end
		table.insert(err, "\tno field package.preload['"..m.."']")
		for path in package.path:gmatch'[^;]+' do
			local fn = path:gsub('%?', (m:gsub('%.', '/')))
			local f = io.open(fn, 'rb')
			if f then
				local str = f:read'*a'
				if f then f:close() end
				if str then
					-- here i'm going to insert a profiling call into each function
--print('parsing filename',fn)
					self.totalLines = self.totalLines + #str:gsub('\n+','\n'):gsub('[^\n]','') + 1
					local parser
					local result, tree = xpcall(function()
						parser = Parser()
						parser:setData(str)
						return parser.tree
					end, function(err)
						return err..'\n'..debug.traceback()
					end)
					if not result then
						error('\n\t'..fn..' at '..parser.t:getpos()..'\n'..tree)
					end
				
					for _,cb in ipairs(self.callbacks) do
						cb(tree)
					end

					str = tostring(tree)
print('parsed '..fn..' total lines:',self.totalLines)
					result = assert(load(str))()
					package.loaded[m] = result
					return result
				end
			end
			table.insert(err, "\tno field '"..fn.."'")
		end
		for path in package.cpath:gmatch'[^;]+' do
			local fn = path:gsub('%?', (m:gsub('%.', '/')))
			local f = io.open(fn, 'rb')
			if f then
				f:close()
				local result = assert(package.loadlib(fn, m))()
				package.loaded[m] = result
				return result
			end
			table.insert(err, "\tno field '"..fn.."'")
		end
		error(table.concat(err, '\n'))
	end
})
return require 
