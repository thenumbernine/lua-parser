local basedir = [[C:\Users\crm\frameworks]]
for i=1,select('#', ...) do
	local arg = select(i, ...)
	print(i, arg)
	if arg == 'clang'
	or arg == 'clang-6.0.1'
	then
		print'found clang'
	else
		print(i,"is not clang")
	end
end

