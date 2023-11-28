exception ParseErr
structure WhileLrVals = WhileLrValsFun(structure Token = LrParser.Token)
structure WhileLex = WhileLexFun(structure Tokens = WhileLrVals.Tokens);
structure WhileParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = WhileLrVals.ParserData
     	       structure Lex = WhileLex)
     
fun ParseProgram(filename) = 
	let val instream = TextIO.openIn filename;
		val read : int -> string = fn a => if TextIO.endOfStream instream
						   then "" 
						   else TextIO.inputN(instream,a);
		val errorPrint = fn (s,pos,c) => print ("Syntax Error:"^Int.toString(pos)^":"^Int.toString(c)^":"^s^"\n");
		val (a,b) = WhileParser.parse(0,(WhileParser.makeLexer read ),errorPrint,())
						handle WhileParser.ParseError => raise ParseErr;
		val _ = TextIO.closeIn instream;
	in 
		a
	end


