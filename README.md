# Regarding Assignment

This assignment produces an Abstract Syntax Tree for the WHILE Language using ML-Lex(for generating Lexer) and ML-Yacc(for generating Parser) along with type checking.

# About the Files

1. while.lex -> Contains the description for Lexer
2. while.yacc -> Contains the grammar and semantic rules for Parser
3. ast_dt.sml -> Contains the datatype for the Abstract Syntax Tree using a structure AST. It also contains various other functions for type-checking and symbol table management.
4. while_LexParse.sml -> This file contains glue code for joining the Lexer and Parser. It also contains the main parsing function ParseProgram filename which generates the AST.
5. while_ast.sml -> This file is used for loading all the .lex.sml, .yacc.sml, .yacc.sig etc. in the sequence required.
   There are also some test files present in the tests folder

# Running the Program

In the command prompt run the following commands sequentially

1. `ml-lex while.lex`
2. `ml-yacc while.yacc`
3. `sml while_ast.sml`
   The 3rd command will open up the sml terminal. After this you can run the following function for generating the AST.
   `ParseProgram(filename)`
   filename is the name of the file(essentially a string) which contains any test program written in the WHILE Language Syntax.

# Context Free Grammar for the WHILE language

The set of non-terminals N is {prog,block,dec,varlist,Type,cmdseq,cmd,exp}

The set of terminals T is {IDENTIFIER, NUMERAL, ASSIGN, SEMICOLON, DBCOLON, NOT, AND, OR, EQ, NEQ,READ, WRITE, PLUS, MINUS , TIMES , DIV , MOD , TILDE , GT , GEQ, LEQ, TT, FF, LT, COMMA, INT, BOOL, PROGRAM , COLON , VAR , DO , IF , THEN , ELSE , ENDIF , WHILE , ENDWH , RPAREN , LPAREN , RBRACE , LBRACE , EOF}

Now following are the production rules P of the Grammar :

prog -> PROGRAM IDENTIFIER DBCOLON block

block -> dec LBRACE cmdseq RBRACE

dec -> VAR varlist COLON Type SEMICOLON dec | epsilon

varlist -> IDENTIFIER COMMA varlist | IDENTIFIER

Type -> INT | BOOL

cmdseq -> cmd SEMICOLON cmdseq | epsilon

cmd -> IDENTIFIER ASSIGN exp
| READ IDENTIFIER
| WRITE exp
| IF exp THEN LBRACE cmdseq RBRACE ELSE LBRACE cmdseq RBRACE ENDIF
| WHILE exp DO LBRACE cmdseq RBRACE ENDWH

exp -> TT
| FF
| NUMERAL
| PLUS NUMERAL
| IDENTIFIER
| exp PLUS exp
| exp MINUS exp
| exp TIMES exp
| exp MOD exp
| exp DIV exp
| exp GT exp
| exp GEQ exp
| exp LT exp
| exp LEQ exp
| exp EQ exp
| exp NEQ exp
| exp AND exp
| exp OR exp
| TILDE exp
| LPAREN exp RPAREN
| NOT exp

IDENTIFIER = {letter}({letter}|{digit})\*
NUMERAL = ({tilde})?{digit}+

letter = [A-Za-z]
digit = [0-9]

The start symbol S is prog

# AST Datatype definition

`
datatype binop = PLUS | MINUS | DIV | MOD | TIMES | LT | LEQ | GT | GEQ | EQ | NEQ | AND | OR
and unop = TILDE | NOT

datatype basictype = INT | BOOL
type varlist = string list
type vlist = (basictype * string) list;

datatype program = PROG of string * blk
and blk = BLK of decl * cmdseq
and decl = DEC of vlist * decl | empty1
and cmdseq = SEQ of cmd * cmdseq | empty
and cmd = SET of string * exp | READ of string | WRITE of exp | ITE of exp * cmdseq * cmdseq | WH of exp * cmdseq
and exp = TT | FF | NUM of int | ID of string | Binexp of binop * exp * exp | Unexp of unop * exp| Paren of exp `

Here, the datatype of AST has been defined recursively(according to the grammar) by first specifying the datatype of program using the other datatypes and then similarly for datatypes for other non-terminal states.

# Syntax-Directed Translation (Semantic Actions)

prog : AST.PROG(IDENTIFIER,block) -> Adds the topmost node using PROG constructor which left child as the IDENTIFIER and right child as a block (representing program IDENTIFIER :: block)

block : (AST.BLK(dec,cmdseq)) -> adds a node using BLK as the constructor with left child as a declaration and right child as a command sequence (representing dec {cmdseq} )

dec : (AST.DEC(AST.makeID (varlist,Type),dec)) -> adds a node using DEC as the constructor with left child as a list of variables with its Type and right child as another declaration
(AST.empty1)

varlist : IDENTIFIER COMMA varlist (IDENTIFIER :: varlist) -> Adds an IDENTIFIER to a variable list present in AST structure
([IDENTIFIER]) -> Creates a list containing the IDENTIFIER

Type : (AST.INT)
(AST.BOOL)

cmdseq : (AST.SEQ(cmd,cmdseq)) -> Adds a node with constructor as SEQ contaning left child as a cmd and right
child an cmdseq (representing cmd ; cmdseq (a recursive translation))
(AST.empty) -> adds empty to the AST

cmd : (AST.SET(IDENTIFIER,exp)) -> Adds a node with constructor as SET contaning left child as IDENTIFIER and right
child an expression (representing IDENTIFIER := exp )

(AST.READ(IDENTIFIER)) -> Adds a node with constructor as READ contaning IDENTIFIER (representing read IDENTIFIER )

(AST.WRITE(exp)) -> Adds a node with constructor as WRITE contaning exp (representing write exp )

(AST.ITE(exp,cmdseq,cmdseq)) -> Adds a node with constructor as ITE and children as exp,cmdseq1 and cmdseq2 (representing if exp then {cmdseq1} else {cmdseq2} endif )

(AST.WH(exp,cmdseq)) -> Adds a node with constructor as WH an left child exp and right child as a cmdseq(representing while exp do {cmdseq} endwh)

exp:(AST.TT) -> adds a string TT in the AST

    (AST.FF) ->  adds a string TT in the AST

    (AST.NUM(NUMERAL)) -> adds the numeral to AST as a Node using constructor NUM

    (AST.ID(IDENTIFIER)) -> adds the variable to AST as a Node using constructor ID

    (AST.Binexp(AST.PLUS,exp1,exp2)) -> Adds a node containing the root as the PLUS symbol and left child as the first expression and right child as the second expression(representing exp1 + exp2). Similarly for other integer operators.

    (AST.Binexp(AST.GT,exp1,exp2)) -> Adds a node containing the root as the GT symbol and left child as the first expression and right child as the second expression(representing exp1 > exp2). Similarly for other relational operators.

    (AST.Binexp(AST.AND,exp1,exp2)) -> Adds a node containing the root as the AND symbol and left child as the first expression and right child as the second expression(representing exp1 && exp2). Similarly for the OR operator.

    (AST.Unexp(AST.TILDE,exp)) -> Adds a node containing exp expression and TILDE Symbol(representing '~'exp)

    (AST.Paren(exp)) -> Adds a node containing exp expression(representing '('exp')')

    (AST.Unexp(AST.NOT,exp)) -> Adds a node containing NOT and exp expression(representing '!' exp)

# Auxiliary functions and Data

`typeMismatch exception` -> this is raised whenever we encounter an invalid type check
`undeclaredVariableFound exception` -> raised if an undeclared variable is used in the READ command

1. `addVar(v,typ)` -> this is used for add a variable and its type into the symbol table
2. `getVartype(v)` -> this is used for retrieving the type of a variable v from symbol table
3. `makeID(v::vl : varlist,ty)` -> recursive function used to add all the variables in a variable list along with the type declared for all variables.
4. `sameType(x,y)` -> used to check if x and y have the same type or not(helper function for typeOfExp function)
5. `typeOfExp` -> this contains all the cases of a possible and valid expression in the while language. It evaluates the type of the exp and returns an exception if it is not correct.
6. `typecmdcheck` -> this checks for the possible commands in the while language. It produces a boolean result checking the types of the various expressions involved in the command.
7. `typeseqcheck` -> checks if a given command sequence has correct types within its commands.
8. `typeblkcheck` -> checks if a block has correct type or not by recursively checking its declarations and command sequence.
9. `typecheck` -> final function which checks if the complete program declaration is correct or not by recursing on the block.
10. `finalProgcheck` -> returns the AST generated if the typecheck function returns true otherwise I raise an exception typeMismatch

# Other Design Decisions

1. One major design decision which I have taken is to include type-checking in this program as without typechecking, the CFG could parse some extra expressions like 4 + tt , tt + ff etc. which are obviously not a part of the language and also do not make any sense. So I have also done type-checking in the parsing stage using auxillary functions(mostly recursive in nature) defined in the AST structure. This ensures that only correct expressions are parsed by the parser.

2. All my AST datatypes and functions for creating AST using semantic rules are defined in the file ast_dt.sml.

3. I have used the HashMap structure available in SMLNJ for creating a symbol table for ease of data retrieval and storage which is used in the type checking process.

4. The LookAhead for the Parser has been set to 0.

5. The payload for each token contains two values, one is the linenum and other is the column number.

# Other Implementation Decisions

1. In the Grammar, I have merged the Integer expressions and Boolean expressions(as mentioned in the EBNF of the language) into one single expression as during reduction, if a variable is encountered it may go to either int expression or bool expression which results in reduce-reduce conflicts. For handling this, I have done type-checking using symbol table.

2. For resolving shift-reduce conflicts, I have set the associativity and precedence of the various operators according to what is used by most of the common programming languages like C,Python etc. Only unary operators ,'~' and '!' are right associative all others are left associative.
   i.e. Logical Operators < Relational Operators < Integer Operators
   Logical Operators := || < && < !
   Relational Operators := =,<> < >,>=,<,<=
   Integer Operators := '-' < '+' < '*' < '/' < '%' < '~'

3. For resolving ambiguity in the grammar, the lexer identifies a string as numeral only if it is either a combination of digits preceded by a '~' sign or nothing. So for identifying '+45'(and numbers like these) I have added in my grammar another production in which
   exp -> PLUS NUMERAL. This resolves the ambiguity in expressions like (3*4+4) where '+4' could have been identified as a numeral in the lexer itself(if I had included the plus symbol numerals in my lex file)

# Acknowledgements

All the decisions and code written in the assignment have been done by myself. I have used the following two resources for understanding the syntax and structure of ML-Lex and ML-Yacc

1. [User's Guide to ML-Lex and ML-Yacc](http://rogerprice.org/ug/ug.pdf) - I referred this pdf file for most of the syntax and understanding of ML-Lex and ML-Yacc functions, how to define them and how to join the Parser and Lexer together.

2. Foundations of Programming Languages by Kent D Lee - I used this book for refering to simple examples of ML-Lex and ML-Yacc Programs given in it. Also, it helped me in defining my AST datatype.
