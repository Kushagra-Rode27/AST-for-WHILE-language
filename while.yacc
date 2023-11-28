(* User  declarations *)


%%
(* required declarations *)
%name While

%term
  IDENTIFIER of string | NUMERAL of int | ASSIGN | SEMICOLON | DBCOLON | NOT
| AND | OR | EQ | NEQ | READ | WRITE
| PLUS | MINUS | TIMES | DIV | MOD | TILDE | GT | GEQ | LEQ | TT | FF 
| LT | COMMA | INT | BOOL | PROGRAM | COLON | VAR | DO
| IF | THEN | ELSE | ENDIF | WHILE | ENDWH | RPAREN | LPAREN | RBRACE | LBRACE | EOF

%nonterm 
    prog of AST.program 
    | block of AST.blk
    | dec of AST.decl 
    | varlist of AST.varlist 
    | Type of AST.basictype 
    | cmdseq of AST.cmdseq 
    | cmd of AST.cmd 
    | exp of AST.exp

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

%left OR 
%left AND
%right NOT
%left EQ NEQ
%left GT LT GEQ LEQ 
%left MINUS
%left PLUS
%left TIMES
%left MOD DIV 
%right TILDE

%start prog

%verbose

%%

prog : PROGRAM IDENTIFIER DBCOLON block (AST.finalProgcheck(AST.PROG(IDENTIFIER,block),AST.typecheck(AST.PROG(IDENTIFIER,block))))

block : dec LBRACE cmdseq RBRACE (AST.BLK(dec,cmdseq))

dec : VAR varlist COLON Type SEMICOLON dec (AST.DEC(AST.makeID(varlist,Type),dec)) 
    |                                             (AST.empty1)

varlist :  IDENTIFIER COMMA varlist (IDENTIFIER :: varlist)
        |  IDENTIFIER ([IDENTIFIER])

Type : INT (AST.INT)
    | BOOL (AST.BOOL)

cmdseq : cmd SEMICOLON cmdseq (AST.SEQ(cmd,cmdseq))
    | (AST.empty)

cmd : IDENTIFIER ASSIGN exp (AST.SET(IDENTIFIER,exp))
  | READ IDENTIFIER (AST.READ(IDENTIFIER))
  | WRITE exp (AST.WRITE(exp))
  | IF exp THEN LBRACE cmdseq RBRACE ELSE LBRACE cmdseq RBRACE ENDIF(AST.ITE(exp,cmdseq1,cmdseq2))
  | WHILE exp DO LBRACE cmdseq RBRACE ENDWH (AST.WH(exp,cmdseq))


exp : TT (AST.TT)
    | FF (AST.FF)
    | NUMERAL (AST.NUM(NUMERAL))
    | PLUS NUMERAL (AST.NUM(NUMERAL))
    | IDENTIFIER (AST.ID(IDENTIFIER))
    | exp PLUS exp (AST.Binexp(AST.PLUS,exp1,exp2))
    | exp MINUS exp (AST.Binexp(AST.MINUS,exp1,exp2))
    | exp TIMES exp (AST.Binexp(AST.TIMES,exp1,exp2))
    | exp MOD exp (AST.Binexp(AST.MOD,exp1,exp2))
    | exp DIV exp (AST.Binexp(AST.DIV,exp1,exp2))
    | exp GT exp (AST.Binexp(AST.GT,exp1,exp2))
    | exp GEQ exp (AST.Binexp(AST.GEQ,exp1,exp2))
    | exp LT exp (AST.Binexp(AST.LT,exp1,exp2))
    | exp LEQ exp (AST.Binexp(AST.LEQ,exp1,exp2))
    | exp EQ exp (AST.Binexp(AST.EQ,exp1,exp2))
    | exp NEQ exp (AST.Binexp(AST.NEQ,exp1,exp2))
    | exp AND exp (AST.Binexp(AST.AND,exp1,exp2))
    | exp OR exp (AST.Binexp(AST.OR,exp1,exp2))
    | TILDE exp (AST.Unexp(AST.TILDE,exp))
    | LPAREN exp RPAREN (AST.Paren(exp))
    | NOT exp (AST.Unexp(AST.NOT,exp))


