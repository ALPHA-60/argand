BIN=argand

$(BIN):  Argand.hs Parser.hs Lexer.hs LinComb.hs Core.hs
	ghc -main-is Argand.main -XBangPatterns -o $@ $<

Parser.hs: Parser.y
	happy $<

Lexer.hs: Lexer.x AlexWrapper-monadIOUserState AlexTemplate-ghc
	alex -g -t . $<

test.ps: test.i $(BIN)
	./$(BIN) $< > $@



clean:
	rm -rf $(BIN) *.o Parser.hs Lexer.hs *.hi
	rm -rf test.ps
