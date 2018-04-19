myinterpreter: myInterpreter.hs MyLexer.hs MyParser.hs
	   ghc --make myInterpreter.hs -o myinterpreter

MyLexer.hs: MyLexer.x
		alex MyLexer.x

MyParser.hs: MyParser.y
		happy MyParser.y