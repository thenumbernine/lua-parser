

-- already got 'do', 'else', 'while', etc ... all we're looking for is the 'end'
-- eats the 'end'
function block()
	local s = {}
	while not canbe 'end' do
		table.insert(s, stmt())
	end
	return s
end

-- already got 'do'
function _do()
	return ast._do(unpack(block()))
end

-- already got 'while'
function _while()
	local c = _cond()
	mustbe 'do'
	return ast._while(c, unpack(block()))
end

-- already got 'repeat'
function _repeat()
	local s = {}
	while not canbe 'until' do
		table.insert(s, stmt())
	end
	local c = _cond()
	return ast._repeat(c, unpack(s))
end

-- already got 'elseif'
-- doesn't process the closing 'elseif', 'else', or 'end', but leaves it for _if()
function _elseif()
	local c = _cond()
	mustbe 'then'
	local s = {}
	while next() ~= 'else' and next() ~= 'elseif' and next() ~= 'end' do
		table.insert(s, stmt())
	end
	return ast._elseif(c, unpack(s))
end

-- same as above, already got 'else', don't process 'end'
function _else()
	local s = {}
	while next() ~= 'else' and next() ~= 'elseif' and next() ~= 'end' do
		table.insert(s, stmt())
	end
	return ast._elsestmt(unpack(s))
end

-- already got 'if'
function _if()
	local c = _cond()
	mustbe 'then'
	local s = {}
	while next() ~= 'else' and next() ~= 'elseif' and next() ~= 'end' do
		table.insert(s, stmt())
	end	
	while true do
		if canbe 'elseif' then
			table.insert(s, _elseif())
		elseif canbe 'else' then
			table.insert(s, _else())
		elseif canbe 'end' then
			break
		else
			error()
		end
	end
	return ast._if(c, unpack(s))
end

function _for()
	local vars = varlist()
	if canbe 'in' then
		local iter = expr()
		mustbe 'do'
		local stmts = block()
		return ast._forin(vars, iter, unpack(stmts))
	elseif canbe '=' then
		assrt(#vars == 1)
		local ranges = exprlist()
		asesrt(#ranges >= 1 and #ranges <= 3)
		mustbe 'do'
		local stmts = block()
		return ast._foreq(vars[1], ranges[1], ranges[2], ranges[3], unpack(stmts))
	else
		error()
	end
end

function _function()
	local n
	if next() ~= '(' then
		n = name()
	end
	mustbe '('
	local vs = varlist()
	mustbe ')'
	local ss = block()
	return ast._function(n, vs, unpack(ss))
end

function _local()
	return ast._local(assign())
end

function stmt()
	if canbe 'do' then
		return _do()
	elseif canbe 'while' then
		return _while()
	elseif canbe 'repeat' then
		return _repeat()
	elseif canbe 'if' then
		return _if()
	elseif canbe 'for' then
		return _for()
	elseif canbe 'function' then
		return _function()
	elseif canbe 'local' then
		return _local()
	elseif canbe 'break' then	-- better be last...
	elseif canbe 'return' then
	else	-- stmt ... assign or call?
		error("not finished")
	end
end