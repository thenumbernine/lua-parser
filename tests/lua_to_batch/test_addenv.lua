local basedir = USERPROFILE..[[\frameworks]]
for i=1,select('#', ...) do
	local arg = select(i, ...)
	print(i, arg)
	if arg == 'clang'
	or arg == 'clang-6.0.1'
	then
		print'found clang'
	elseif arg == 'lua'
	or arg == 'lua-5.3.5'
	then
		print'found lua'
	else
		print('unknown',arg)
	end
end

