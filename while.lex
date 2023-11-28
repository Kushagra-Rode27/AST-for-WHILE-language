structure Tokens= Tokens
  exception inValidCharacter
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val c = ref 0
  val eof = fn () => Tokens.EOF(!pos, !c)
  val error = fn (token, pos, col) => TextIO.output(TextIO.stdOut, "Unknown Token:" ^ Int.toString(!pos) ^ ":" ^ Int.toString(!c) ^ ":" ^ token ^ "\n")

  
%%
%header (functor WhileLexFun(structure Tokens:While_TOKENS));

letter = [A-Za-z];
ws = [\ \t\n];
digit = [0-9];
tilde = [\~];
plus  = [\+];

%%
";"      => (c := !c + 1 ;Tokens.SEMICOLON(!pos,!c));
":"      => (c := !c + 1 ;Tokens.COLON(!pos,!c));
","      => (c := !c + 1 ;Tokens.COMMA(!pos,!c));
"!"      => (c := !c + 1 ;Tokens.NOT(!pos,!c));
"tt"   => (c := !c + 2 ;Tokens.TT(!pos,!c));
"ff"  => (c := !c + 2 ; Tokens.FF(!pos,!c));
"&&"    => (c := !c + 2 ;Tokens.AND(!pos,!c));
"||"     => (c := !c + 2 ; Tokens.OR(!pos,!c));
":="  => (c := !c + 2; Tokens.ASSIGN(!pos,!c));
"::"  => (c := !c + 2; Tokens.DBCOLON(!pos,!c));

"program" => (c := !c + 7 ;Tokens.PROGRAM(!pos,!c));
"int"    => (c := !c + 3 ;Tokens.INT(!pos,!c));
"bool"   => (c := !c + 4 ;Tokens.BOOL(!pos,!c));
"if"     => (c := !c + 2 ;Tokens.IF(!pos,!c));
"then"   => (c := !c + 4 ;Tokens.THEN(!pos,!c));
"else"   => (c := !c + 4 ;Tokens.ELSE(!pos,!c));
"endif"  => (c := !c + 5 ;Tokens.ENDIF(!pos,!c));
"while"  => (c := !c + 5 ;Tokens.WHILE(!pos,!c));
"var"    => (c := !c + 3 ;Tokens.VAR(!pos,!c));
"do"     => (c := !c + 2 ;Tokens.DO(!pos,!c));
"endwh"  => (c := !c + 5 ;Tokens.ENDWH(!pos,!c));
"read"  => (c := !c + 4 ;Tokens.READ(!pos,!c));
"write" => (c := !c + 5 ;Tokens.WRITE(!pos,!c));


"("      => (c := !c + 1 ; Tokens.LPAREN(!pos,!c));
")"      => (c := !c + 1 ;Tokens.RPAREN(!pos,!c));
"{"      => (c := !c + 1 ;Tokens.LBRACE(!pos,!c));
"}"      => (c := !c + 1 ;Tokens.RBRACE(!pos,!c));

"+"   => (c := !c + 1 ;Tokens.PLUS(!pos,!c));
"-"   => (c := !c + 1 ;Tokens.MINUS(!pos,!c));
"*"   => (c := !c + 1 ;Tokens.TIMES(!pos,!c));
"/"   => (c := !c + 1 ;Tokens.DIV(!pos,!c));
"%"   => (c := !c + 1 ;Tokens.MOD(!pos,!c));
"~"   => (c := !c + 1 ;Tokens.TILDE(!pos,!c));

">="   => (c := !c + 2 ;Tokens.GEQ(!pos,!c));
"<="   => (c := !c + 2 ;Tokens.LEQ(!pos,!c));
"<"   => (c := !c + 1 ;Tokens.LT(!pos,!c));
">"   => (c := !c + 1 ;Tokens.GT(!pos,!c));
"=" => (c := !c + 1 ;Tokens.EQ(!pos,!c));
"<>" => (c := !c + 1 ;Tokens.NEQ(!pos,!c));

{letter}({letter}|{digit})* => (c := !c + size(yytext);Tokens.IDENTIFIER(yytext,!pos,!c));
({tilde})?{digit}+ => (c := !c + size(yytext) ;Tokens.NUMERAL (valOf(Int.fromString yytext), !pos, !c));
{ws}+    => (c := !c + size(yytext) ; lex());
[\n|\r\n]       => (pos := (!pos) + 1; c := 0; lex());
.       => (error(yytext,pos,c); c := !c + size(yytext);raise inValidCharacter; lex());
