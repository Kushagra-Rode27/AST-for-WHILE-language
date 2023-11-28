structure AST =
struct
exception typeMismatch;
exception undeclaredVariableFound;
datatype binop = PLUS | MINUS | DIV | MOD | TIMES | LT | LEQ | GT | GEQ | EQ | NEQ | AND | OR
and unop = TILDE | NOT

datatype basictype = INT | BOOL
type varlist = string list
type vlist = ( basictype * string) list;

datatype program = PROG of string * blk

and blk = BLK of decl * cmdseq
and decl = DEC of vlist * decl |  empty1
and cmdseq = SEQ of cmd * cmdseq |  empty
and cmd = SET of string * exp | READ of string | WRITE of exp | ITE of exp * cmdseq *  cmdseq | WH of exp * cmdseq
and exp = TT | FF
		| NUM of int
		| ID of string
		| Binexp of binop * exp * exp
		| Unexp of unop * exp
		| Paren of exp

(*Type checking is done in the section below so that some invalid expressions like tt + 1 etc. do not get parsed by the parser*)
val symbolTable : (string, basictype) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (20, Fail "Variable Not Declared")

fun addVar(v,typ) = 
		let 
			val ins = HashTable.insert symbolTable(v,typ); 
		in 
			v 
		end;

fun getVartype(v)=HashTable.lookup symbolTable (v);

val varFinalList = [] : vlist;
fun makeID(v :: [],ty) = (ty,addVar(v,ty)) :: varFinalList
  |makeID(v::vl : varlist,ty) = (ty,addVar(v,ty)) :: makeID(vl,ty)
  |makeID([],ty) = varFinalList;

fun sameType(x,y) = (x = INT andalso y = INT) orelse (x = BOOL andalso y = BOOL);

fun typeOfExp(Binexp(PLUS,x,y))=if typeOfExp(x)=INT andalso typeOfExp(y)=INT then INT else raise typeMismatch
 | typeOfExp(Binexp(MINUS,x,y))=if typeOfExp(x)=INT andalso typeOfExp(y)=INT then INT else raise typeMismatch
 | typeOfExp(Binexp(TIMES,x,y))=if typeOfExp(x)=INT andalso typeOfExp(y)=INT then INT else raise typeMismatch
 | typeOfExp(Binexp(DIV,x,y))=if typeOfExp(x)=INT andalso typeOfExp(y)=INT then INT else raise typeMismatch
 | typeOfExp(Binexp(MOD,x,y))=if typeOfExp(x)=INT andalso typeOfExp(y)=INT then INT else raise typeMismatch
 | typeOfExp(Binexp(LT,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(LEQ,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(GT,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(GEQ,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(EQ,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(NEQ,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(AND,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Binexp(OR,x,y))=if sameType(typeOfExp(x),typeOfExp(y)) then BOOL else raise typeMismatch
 | typeOfExp(Unexp(NOT,x))=if typeOfExp(x)=BOOL then BOOL else raise typeMismatch
 | typeOfExp(Paren(x))= typeOfExp(x)
 | typeOfExp(Unexp(TILDE,x))=if typeOfExp(x)=INT then INT else raise typeMismatch
 | typeOfExp(NUM(x))=INT
 | typeOfExp(ID(x))=getVartype(x)
 | typeOfExp(TT)=BOOL
 | typeOfExp(FF)=BOOL;


fun typecmdcheck(SET(x,y)) =  getVartype(x) = typeOfExp(y)
  | typecmdcheck(ITE(x,y,z))= 
	let 
		fun typecmdseqcheck(SEQ(x,y)) = typecmdcheck(x) andalso typecmdseqcheck(y)
			| typecmdseqcheck(empty)=true;
	in 
  		typeOfExp(x)=BOOL andalso typecmdseqcheck(y) andalso typecmdseqcheck(z)
	end
  | typecmdcheck(WH(x,y))=
  	let 
		fun typecmdseqcheck(SEQ(x,y)) = typecmdcheck(x) andalso typecmdseqcheck(y)
			| typecmdseqcheck(empty)=true;
	in 
  		typeOfExp(x)=BOOL andalso typecmdseqcheck(y)
	end
  | typecmdcheck(READ(x)) =  if HashTable.inDomain symbolTable(x) then true else raise undeclaredVariableFound
  | typecmdcheck(WRITE(x)) = typeOfExp(x) = INT;

fun typeseqcheck(SEQ(x,y)) = typecmdcheck(x) andalso typeseqcheck(y)
  | typeseqcheck(empty)=true

fun typeblkcheck(BLK(x,y)) = typeseqcheck(y);

fun typecheck(PROG(x,y))=typeblkcheck(y);

fun finalProgcheck(x,y)=if y then x else raise typeMismatch;

end
