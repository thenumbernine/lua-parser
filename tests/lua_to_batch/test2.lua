local basedir = [[C:\Users\crm\frameworks]]

-- [[ building up to iterating over varargs
print(basedir)
for i=1,3 do print'here' end
for i=1,3 do print(i) end
local argcount = select('#', ...)
argcount = argcount + argcount
print(argcount)
--]]

