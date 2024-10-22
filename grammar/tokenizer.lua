local Tokenizer = require 'parser.base.tokenizer'

local GrammarTokenizer = Tokenizer:subclass()

function GrammarTokenizer:initSymbolsAndKeywords()
	for w in ([[ ::= | ; { } [ ] ( ) ]]):gmatch('%S+') do
		self.symbols:insert(w)
	end
end

return GrammarTokenizer
