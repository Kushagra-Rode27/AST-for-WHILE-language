functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User  declarations *)



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\004\000\000\000\
\\001\000\001\000\011\000\000\000\
\\001\000\001\000\031\000\002\000\030\000\006\000\029\000\013\000\028\000\
\\018\000\027\000\022\000\026\000\023\000\025\000\039\000\024\000\000\000\
\\001\000\001\000\034\000\000\000\
\\001\000\002\000\057\000\000\000\
\\001\000\003\000\035\000\000\000\
\\001\000\004\000\021\000\000\000\
\\001\000\004\000\061\000\000\000\
\\001\000\005\000\005\000\000\000\
\\001\000\007\000\054\000\008\000\053\000\009\000\052\000\010\000\051\000\
\\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\024\000\042\000\031\000\041\000\000\000\
\\001\000\007\000\054\000\008\000\053\000\009\000\052\000\010\000\051\000\
\\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\024\000\042\000\033\000\059\000\000\000\
\\001\000\007\000\054\000\008\000\053\000\009\000\052\000\010\000\051\000\
\\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\024\000\042\000\038\000\076\000\000\000\
\\001\000\026\000\038\000\027\000\037\000\000\000\
\\001\000\028\000\003\000\000\000\
\\001\000\029\000\019\000\000\000\
\\001\000\034\000\084\000\000\000\
\\001\000\035\000\088\000\000\000\
\\001\000\037\000\083\000\000\000\
\\001\000\040\000\022\000\000\000\
\\001\000\040\000\081\000\000\000\
\\001\000\040\000\082\000\000\000\
\\001\000\040\000\087\000\000\000\
\\001\000\041\000\009\000\000\000\
\\001\000\041\000\062\000\000\000\
\\001\000\041\000\077\000\000\000\
\\001\000\041\000\085\000\000\000\
\\001\000\042\000\000\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\030\000\008\000\000\000\
\\094\000\000\000\
\\095\000\025\000\020\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\001\000\018\000\011\000\017\000\012\000\016\000\032\000\015\000\
\\036\000\014\000\000\000\
\\100\000\007\000\054\000\008\000\053\000\009\000\052\000\010\000\051\000\
\\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\024\000\042\000\000\000\
\\101\000\000\000\
\\102\000\007\000\054\000\008\000\053\000\009\000\052\000\010\000\051\000\
\\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\024\000\042\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\015\000\048\000\016\000\047\000\017\000\046\000\000\000\
\\111\000\013\000\050\000\015\000\048\000\016\000\047\000\017\000\046\000\000\000\
\\112\000\016\000\047\000\017\000\046\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\000\000\
\\116\000\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\000\000\
\\117\000\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\000\000\
\\118\000\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\000\000\
\\119\000\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\024\000\042\000\000\000\
\\120\000\013\000\050\000\014\000\049\000\015\000\048\000\016\000\047\000\
\\017\000\046\000\019\000\045\000\020\000\044\000\021\000\043\000\
\\024\000\042\000\000\000\
\\121\000\009\000\052\000\010\000\051\000\013\000\050\000\014\000\049\000\
\\015\000\048\000\016\000\047\000\017\000\046\000\019\000\045\000\
\\020\000\044\000\021\000\043\000\024\000\042\000\000\000\
\\122\000\007\000\054\000\009\000\052\000\010\000\051\000\013\000\050\000\
\\014\000\049\000\015\000\048\000\016\000\047\000\017\000\046\000\
\\019\000\045\000\020\000\044\000\021\000\043\000\024\000\042\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\009\000\052\000\010\000\051\000\013\000\050\000\014\000\049\000\
\\015\000\048\000\016\000\047\000\017\000\046\000\019\000\045\000\
\\020\000\044\000\021\000\043\000\024\000\042\000\000\000\
\"
val actionRowNumbers =
"\013\000\000\000\008\000\030\000\
\\022\000\027\000\001\000\036\000\
\\014\000\032\000\006\000\018\000\
\\002\000\002\000\002\000\003\000\
\\005\000\012\000\001\000\036\000\
\\028\000\009\000\002\000\043\000\
\\042\000\002\000\004\000\002\000\
\\044\000\046\000\010\000\039\000\
\\038\000\002\000\007\000\034\000\
\\033\000\031\000\035\000\023\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\011\000\060\000\045\000\
\\062\000\024\000\037\000\030\000\
\\036\000\054\000\055\000\053\000\
\\052\000\050\000\051\000\049\000\
\\048\000\047\000\057\000\056\000\
\\059\000\058\000\061\000\036\000\
\\029\000\019\000\020\000\017\000\
\\015\000\041\000\025\000\036\000\
\\021\000\016\000\040\000\026\000"
val gotoT =
"\
\\001\000\087\000\000\000\
\\000\000\
\\000\000\
\\002\000\005\000\003\000\004\000\000\000\
\\000\000\
\\000\000\
\\004\000\008\000\000\000\
\\006\000\011\000\007\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\021\000\000\000\
\\008\000\030\000\000\000\
\\008\000\031\000\000\000\
\\000\000\
\\000\000\
\\005\000\034\000\000\000\
\\004\000\037\000\000\000\
\\006\000\038\000\007\000\010\000\000\000\
\\000\000\
\\000\000\
\\008\000\053\000\000\000\
\\000\000\
\\000\000\
\\008\000\054\000\000\000\
\\000\000\
\\008\000\056\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\058\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\061\000\000\000\
\\008\000\062\000\000\000\
\\008\000\063\000\000\000\
\\008\000\064\000\000\000\
\\008\000\065\000\000\000\
\\008\000\066\000\000\000\
\\008\000\067\000\000\000\
\\008\000\068\000\000\000\
\\008\000\069\000\000\000\
\\008\000\070\000\000\000\
\\008\000\071\000\000\000\
\\008\000\072\000\000\000\
\\008\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\076\000\000\000\
\\006\000\077\000\007\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\078\000\007\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\084\000\007\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 88
val numrules = 36
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUMERAL of unit ->  (int) | IDENTIFIER of unit ->  (string)
 | exp of unit ->  (AST.exp) | cmd of unit ->  (AST.cmd)
 | cmdseq of unit ->  (AST.cmdseq) | Type of unit ->  (AST.basictype)
 | varlist of unit ->  (AST.varlist) | dec of unit ->  (AST.decl)
 | block of unit ->  (AST.blk) | prog of unit ->  (AST.program)
end
type svalue = MlyValue.svalue
type result = AST.program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "IDENTIFIER"
  | (T 1) => "NUMERAL"
  | (T 2) => "ASSIGN"
  | (T 3) => "SEMICOLON"
  | (T 4) => "DBCOLON"
  | (T 5) => "NOT"
  | (T 6) => "AND"
  | (T 7) => "OR"
  | (T 8) => "EQ"
  | (T 9) => "NEQ"
  | (T 10) => "READ"
  | (T 11) => "WRITE"
  | (T 12) => "PLUS"
  | (T 13) => "MINUS"
  | (T 14) => "TIMES"
  | (T 15) => "DIV"
  | (T 16) => "MOD"
  | (T 17) => "TILDE"
  | (T 18) => "GT"
  | (T 19) => "GEQ"
  | (T 20) => "LEQ"
  | (T 21) => "TT"
  | (T 22) => "FF"
  | (T 23) => "LT"
  | (T 24) => "COMMA"
  | (T 25) => "INT"
  | (T 26) => "BOOL"
  | (T 27) => "PROGRAM"
  | (T 28) => "COLON"
  | (T 29) => "VAR"
  | (T 30) => "DO"
  | (T 31) => "IF"
  | (T 32) => "THEN"
  | (T 33) => "ELSE"
  | (T 34) => "ENDIF"
  | (T 35) => "WHILE"
  | (T 36) => "ENDWH"
  | (T 37) => "RPAREN"
  | (T 38) => "LPAREN"
  | (T 39) => "RBRACE"
  | (T 40) => "LBRACE"
  | (T 41) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.block block1, _, block1right)) :: _ :: ( _,
 ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, PROGRAM1left,
 _)) :: rest671)) => let val  result = MlyValue.prog (fn _ => let val 
 (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (block as block1) = block1 ()
 in (
AST.finalProgcheck(AST.PROG(IDENTIFIER,block),AST.typecheck(AST.PROG(IDENTIFIER,block)))
)
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, block1right), rest671)
end
|  ( 1, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.cmdseq 
cmdseq1, _, _)) :: _ :: ( _, ( MlyValue.dec dec1, dec1left, _)) :: 
rest671)) => let val  result = MlyValue.block (fn _ => let val  (dec
 as dec1) = dec1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (AST.BLK(dec,cmdseq))
end)
 in ( LrTable.NT 1, ( result, dec1left, RBRACE1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.dec dec1, _, dec1right)) :: _ :: ( _, ( 
MlyValue.Type Type1, _, _)) :: _ :: ( _, ( MlyValue.varlist varlist1,
 _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.dec (fn _ => let val  (varlist as varlist1) = varlist1 ()
 val  (Type as Type1) = Type1 ()
 val  (dec as dec1) = dec1 ()
 in (AST.DEC(AST.makeID(varlist,Type),dec))
end)
 in ( LrTable.NT 2, ( result, VAR1left, dec1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.dec (fn _ => (
AST.empty1))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _ ::
 ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)) :: 
rest671)) => let val  result = MlyValue.varlist (fn _ => let val  (
IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (varlist as varlist1) = varlist1 ()
 in (IDENTIFIER :: varlist)
end)
 in ( LrTable.NT 3, ( result, IDENTIFIER1left, varlist1right), rest671
)
end
|  ( 5, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.varlist
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in ([IDENTIFIER])
end)
 in ( LrTable.NT 3, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 6, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.INT))
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 7, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.BOOL))
 in ( LrTable.NT 4, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.cmdseq cmdseq1, _, cmdseq1right)) :: _ :: (
 _, ( MlyValue.cmd cmd1, cmd1left, _)) :: rest671)) => let val  result
 = MlyValue.cmdseq (fn _ => let val  (cmd as cmd1) = cmd1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (AST.SEQ(cmd,cmdseq))
end)
 in ( LrTable.NT 5, ( result, cmd1left, cmdseq1right), rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.cmdseq (fn _ => (
AST.empty))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)) :: rest671)) =>
 let val  result = MlyValue.cmd (fn _ => let val  (IDENTIFIER as 
IDENTIFIER1) = IDENTIFIER1 ()
 val  (exp as exp1) = exp1 ()
 in (AST.SET(IDENTIFIER,exp))
end)
 in ( LrTable.NT 6, ( result, IDENTIFIER1left, exp1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, IDENTIFIER1right
)) :: ( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.cmd (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in (AST.READ(IDENTIFIER))
end)
 in ( LrTable.NT 6, ( result, READ1left, IDENTIFIER1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
WRITE1left, _)) :: rest671)) => let val  result = MlyValue.cmd (fn _
 => let val  (exp as exp1) = exp1 ()
 in (AST.WRITE(exp))
end)
 in ( LrTable.NT 6, ( result, WRITE1left, exp1right), rest671)
end
|  ( 13, ( ( _, ( _, _, ENDIF1right)) :: _ :: ( _, ( MlyValue.cmdseq 
cmdseq2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.cmdseq cmdseq1, _, _
)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, IF1left,
 _)) :: rest671)) => let val  result = MlyValue.cmd (fn _ => let val 
 (exp as exp1) = exp1 ()
 val  cmdseq1 = cmdseq1 ()
 val  cmdseq2 = cmdseq2 ()
 in (AST.ITE(exp,cmdseq1,cmdseq2))
end)
 in ( LrTable.NT 6, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 14, ( ( _, ( _, _, ENDWH1right)) :: _ :: ( _, ( MlyValue.cmdseq 
cmdseq1, _, _)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, 
( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.cmd
 (fn _ => let val  (exp as exp1) = exp1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (AST.WH(exp,cmdseq))
end)
 in ( LrTable.NT 6, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 15, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (AST.TT))
 in ( LrTable.NT 7, ( result, TT1left, TT1right), rest671)
end
|  ( 16, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (AST.FF))
 in ( LrTable.NT 7, ( result, FF1left, FF1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.NUMERAL NUMERAL1, NUMERAL1left, 
NUMERAL1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (NUMERAL as NUMERAL1) = NUMERAL1 ()
 in (AST.NUM(NUMERAL))
end)
 in ( LrTable.NT 7, ( result, NUMERAL1left, NUMERAL1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.NUMERAL NUMERAL1, _, NUMERAL1right)) :: ( _
, ( _, PLUS1left, _)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  (NUMERAL as NUMERAL1) = NUMERAL1 ()
 in (AST.NUM(NUMERAL))
end)
 in ( LrTable.NT 7, ( result, PLUS1left, NUMERAL1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.exp (fn
 _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in (AST.ID(IDENTIFIER))
end)
 in ( LrTable.NT 7, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.PLUS,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.MINUS,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.TIMES,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.MOD,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.DIV,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.GT,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.GEQ,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.LT,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.LEQ,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.EQ,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.NEQ,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.AND,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.Binexp(AST.OR,exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
TILDE1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (exp as exp1) = exp1 ()
 in (AST.Unexp(AST.TILDE,exp))
end)
 in ( LrTable.NT 7, ( result, TILDE1left, exp1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (AST.Paren(exp))
end)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (AST.Unexp(AST.NOT,exp))
end)
 in ( LrTable.NT 7, ( result, NOT1left, exp1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
fun NUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUMERAL (fn () => i),p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun DBCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TILDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
