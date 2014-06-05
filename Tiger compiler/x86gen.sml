signature CODEGEN =
sig
    structure Frame: FRAME
    val codegen: Frame.frame -> Tree.stm -> Assem.instr list
end

structure X86Gen: CODEGEN =
struct

structure Frame: FRAME = X86Frame
structure S = Symbol
structure T = Tree
structure Tm = Temp
structure A = Assem
structure F = Frame
structure PT = PrintTree(F)

exception TODO
exception Bug
fun raiseBug msg = ((print ("Bug: " ^ msg ^ "\n")); raise Bug)

fun int i =
    if i >= 0
    then Int.toString i
    else "-" ^ Int.toString (~i)

fun codegen frame stm =
    let
        val ilist = ref (nil: A.instr list)

        fun emit x = (ilist := x :: (!ilist))

        fun result gen =
            let val t = Tm.newtemp ()
            in  gen t; t
            end

        fun operator2jump oper =
            case oper of
                T.EQ => "je"
              | T.NE => "jne"
              | T.LT => "jl"
              | T.GT => "jg"
              | T.LE => "jle"
              | T.GE => "jge"
              | _ => "bad branch operator!"

        fun moveInstr s d doc = A.MOVE { assem = "\tmovl `s0, `d0"
                                       , src = s
                                       , dst = d
                                       , doc = "x86gen:" ^ doc}

        fun adjustSP count = A.OPER { assem = "\taddl $" ^ int count ^ ", `d0"
                                    , src = [F.SP] (* old-SP used *)
                                    , dst = [F.SP]
                                    , jump = NONE
                                    , doc = "x86gen:57"}

        fun allocArgs count = adjustSP (~F.wordSize*count)
        fun freeArgs count = adjustSP (F.wordSize*count)

        fun munchStm (T.SEQ (a, b)) = (munchStm a; munchStm b)

          (* MOVE *)
          | munchStm (T.MOVE (T.TEMP t, T.CALL (T.NAME l, args))) =
            ( emit (A.OPER { assem = "\tcall " ^ S.name l
                           , src = munchArgs args
                           , dst = F.calldefs
                           , jump = NONE
                           , doc = "x86gen:70"})
            ; emit (freeArgs (length args))
            ; emit (moveInstr F.EAX t "72"))

          | munchStm (T.MOVE (T.MEM e1, T.CALL (T.NAME l, args))) =
            let 
                val t = Tm.newtemp ()
            in  
                (emit (A.OPER { 
			   assem = "\tcall " ^ S.name l
                         , src = munchArgs args
                         , dst = F.calldefs
                         , jump = NONE
                         , doc = "x86gen:83"})
		; emit (A.OPER {
			     assem = "\tmovl `s0, `d0"
			   , src = [munchExp e1]
			   , dst = [t]
			   , jump = NONE
			   , doc = "x86gen:89"})
		; emit (freeArgs (length args))
		; emit (moveInstr F.EAX t "91"))
	    end

          | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2)) =
            (emit (A.OPER {
			assem = "\tmovl `s1, " ^ int i ^ "(`s0)"
		      , dst = []
		      , src = [munchExp e1, munchExp e2]
		      , jump = NONE
		      , doc = "x86gen:100"}))

          | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2)) =
            (emit (A.OPER {
			assem = "\tmovl `s1, " ^ int i ^ "(`s0)"
		      , dst = []
		      , src = [munchExp e1, munchExp e2]
		      , jump = NONE
		      , doc = "x86gen:108"}))

          | munchStm (T.MOVE (T.MEM (T.CONST i), e2)) =
            (emit (A.OPER {
			assem = "\tmovl `s0, " ^ int i ^ "(%ebp)"
		      , dst = []
		      , src = [munchExp e2]
		      , jump = NONE
		      , doc = "x86gen:116"}))

          | munchStm (T.MOVE (T.MEM e1, e2)) =
            (emit (moveInstr (munchExp e1) (munchExp e2) "119")) 

          | munchStm (T.MOVE (T.TEMP i, e2)) =
            (emit (moveInstr (munchExp e2) i "122"))

          | munchStm (T.LABEL lab) =
            (emit (A.LABEL{
			assem = (S.name lab) ^ ":"
		      , lab = lab
		      , doc = "x86gen:128"}))
		
          (* JUMP *)
          | munchStm (T.CJUMP (oper, T.CONST i, e2, lab1, lab2)) =
            (emit ( A.OPER {
			assem = "\tcmpl $" ^ int i ^ ", `s0"
		      , dst = []
		      , src = [munchExp e2]
		      , jump = NONE
		      , doc = "x86gen:137"})
	    ; emit (A.OPER {
			 assem = "\t" ^ (operator2jump (T.symRel oper)) ^ " `j0 \n \tjmp `j1"
		       , dst = []
		       , src = []
		       , jump = SOME[lab1, lab2]
		       , doc = "x86gen:143"}))
		
          | munchStm (T.CJUMP (oper, e1, T.CONST i, lab1, lab2)) =
            (emit ( A.OPER {
			 assem = "\tcmpl $" ^ int i ^ ", `s0"
		       , dst = []
		       , src = [munchExp e1]
		       , jump = NONE
		       , doc = "x86gen:159"})
	    ; emit (A.OPER {
			 assem = "\t" ^ (operator2jump oper) ^ " `j0 \n \tjmp `j1"
		       , dst = []
		       , src = []
		       , jump = SOME[lab1, lab2]
		       , doc = "x86gen:165"}))
		 
          | munchStm (T.CJUMP (oper, e1, e2, lab1, lab2)) =
            (emit ( A.OPER {
			assem = "\tcmpl `s0, `s1" 
		      , dst = []
		      , src = [munchExp e1, munchExp e2]
		      , jump = NONE
		      , doc = "x86gen:165"})
	    ; emit(A.OPER {
			assem = "\t" ^ (operator2jump (T.symRel oper)) ^ " `j0 \n \tjmp `j1"
		      , dst = []
		      , src = []
		      , jump = SOME[lab1, lab2]
		      , doc = "x86gen:171"}))
		
          | munchStm (T.JUMP (T.NAME lab, llst)) =
            (emit (A.OPER {
			assem = "\tjmp " ^ (S.name lab)
		      , dst = []
		      , src = []
		      , jump = SOME[Temp.namedLabel(S.name lab)]
		      , doc = "x86gen:179"}))
		
          (* EXP *)
          | munchStm (T.EXP (T.CALL (T.NAME lab, args))) =
           ((emit (A.OPER {
			assem = "\tcall " ^ S.name lab
		      , src = munchArgs args
		      , dst = F.calldefs
		      , jump = NONE
		      , doc = "x86gen:188"})
	    ; emit (freeArgs (length args))))
               
          | munchStm (T.EXP exp) =
            (munchExp exp; ())

          (* If no match so far, complain *)
          | munchStm (T.JUMP a) =
            emit (A.OPER { assem = "\t# JUMP: bad JUMP in munchStm!"
                         , src = []
                         , dst = []
                         , jump = NONE
                         , doc = "x86gen:213"})
		 
          | munchStm (T.MOVE a) =
            emit (A.MOVE { assem = "\t# MOVE: bad MOVE in munchStm!"
                         , src = Tm.newtemp ()
                         , dst = Tm.newtemp ()
                         , doc = "x86gen:219"})
		 
        and munchArgs args =
            (* in the simple approach used here, we pass all args in memory *)
            let 
		val rargs = rev args (* push args right-to-left *)
                fun munchArgs1 [] = []
                  | munchArgs1 (ah::at) = ((emit(A.OPER {
						      assem = "\tpushl `s0"
						    , src = [munchExp ah]
						    , dst = []
						    , jump = NONE
						    , doc = "x86gen:218"})
					   ; munchArgs1 at))
            in 
		munchArgs1 rargs
            end

        (* Memory access *)
        and munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST n))) =
            result (fn r => emit (A.OPER { assem = "\tmovl " ^ int n ^
                                                   "(`s0), `d0"
                                         , src = [munchExp e]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:231"}))

          | munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST n, e))) =
            result (fn r => (emit (A.OPER {
					assem = "\tmovl " ^ int n ^ "(`s0), `d0"
				      , dst = [r]
				      , src = [munchExp e]
				      , jump = NONE
				      , doc = "x86gen: 239"})))
		   
          | munchExp (T.MEM (T.BINOP (T.MINUS, e, T.CONST n))) =
            result (fn r =>((print "din mor \n" ;emit (A.OPER {
				       assem = "\tmovl " ^ int n ^ "(`s0), `d0" (*måske et ~ på n*)
				     , dst = [r]
				     , src = [munchExp e]
				     , jump = NONE
				     , doc = "x86gen:263"}))))
		   
          | munchExp (T.MEM e) =
            result (fn r => (emit (A.OPER {
					assem = "\tmovl (`s0), `d0"
				      , dst = [r]
				      , src = [munchExp e]
				      , jump = NONE
				      , doc = "x86gen:255"})))
		   
	  (* PLUS *)
	  | munchExp (T.BINOP (T.PLUS, e1, T.CONST i)) =
            result (fn r => ((emit (moveInstr (munchExp e1) r "277")); 
			     emit (A.OPER {
					assem = "\taddl $" ^ int i ^ ",  `d0"
				      , dst = [r]
				      , src = [r]
				      , jump = NONE
				      , doc = "x86gen:265"})))

          | munchExp (T.BINOP (T.PLUS, T.CONST i, e1)) = (*FH*)
            result (fn r => ((emit (moveInstr (munchExp e1) r "287")
			     ;emit (A.OPER {
					assem = "\taddl $" ^ int i ^ ", `d0"
				      , dst = [r]
				      , src = [r]
				      , jump = NONE
				      , doc = "x86gen:293"}))))
		   
          | munchExp (T.BINOP (T.PLUS, e1, e2)) =
            (* Hint, p203: use src=[r,_] and do not use `s0,
             * which specifies that r is used *)
            result (fn r => ((emit (moveInstr (munchExp e1) r "279")
			     ;emit (A.OPER {
					 assem = "\taddl `s1, `d0"
				       , dst = [r]
				       , src = [r, munchExp e2]
				       , jump = NONE
				       , doc = "x86gen:285"}))))
		   
          (* MINUS *)
          | munchExp (T.BINOP (T.MINUS, e1, T.CONST i)) = (*lr*)
            result (fn r => ((emit (moveInstr (munchExp e1) r "289"))
			    ; emit (A.OPER {
					 assem = "\tsubl $" ^ int i ^ ",  `d0"
				       , dst = [r]
				       , src = [r]
				       , jump = NONE
				       , doc = "x86gen:295"})))

          | munchExp (T.BINOP (T.MINUS, T.CONST 0, e1)) =
            result (fn r => ((emit (moveInstr (munchExp e1) r "298"))
			    ; emit(A.OPER{
					assem = "\tnegl `s0"
				      , dst = [r]
				      , src = [r]
				      , jump = NONE
				      , doc = "x86gen: 304"})))
		   
          | munchExp (T.BINOP (T.MINUS, T.CONST i, e1)) =
            result (fn r =>((emit (moveInstr (munchExp e1) r "307"))
			   ; emit (A.OPER {
					assem = "\taddl $" ^ int i ^ ",  `s0"
				      , dst = [r]
				      , src = [munchExp(T.BINOP(T.MINUS, T.CONST 0, e1))]
				      , jump = NONE
				      , doc = "x86gen:313"})))

          | munchExp (T.BINOP (T.MINUS, e1, e2)) =
            result (fn r => ((emit (moveInstr (munchExp e2) r "316"))
			     ; emit (A.OPER {
					  assem = "\tsubl `s0, `d0"
					, dst = [r]
					, src = [munchExp e1]
					, jump = NONE
					, doc = "x86gen:322"})))

          (* MULTIPLY *)
          | munchExp (T.BINOP (T.MUL, e1, e2)) =
            result (fn r => ((emit (moveInstr (munchExp e1) r "326"))
			    ; (emit (A.OPER {
					  assem = "\timull `s1, `s0"
					, dst = [r]
					, src = [r, munchExp e2]
					, jump = NONE
					, doc = "x86gen:332"}))))
		   
          (* DIVIDE *)
          | munchExp (T.BINOP (T.DIV, e1, e2)) =
            (* Hint from
             * http://www.cs.mun.ca/~rod/winter2004/cs3724/notes/asm.html:
             *
             * "To divide x by y, we first convert it into 64-bits, and
             * them use idivl.
             *
             *  movl  x, %eax
             *  cltd
             *  idivl y
             *
             * The quotient is in %eax, and the remainder is in %edx."
             *)
	    result (fn r => ((emit(moveInstr (munchExp e1) F.EAX "374")
			      ; emit (A.OPER {
					   assem = "\tcltd"
					 , dst = []
					 , src = []
					 , jump = NONE
					 , doc = "x86gen:354"})
			      ; emit (A.OPER {
					   assem = "\tidivl `s0"
					 , dst = []
					 , src = [munchExp e2]
					 , jump = NONE
					 , doc = "x86gen:360"})
			      ; emit (A.OPER {
					   assem = "\tmovl `s0, `d0"
					 , dst = [r]
					 , src = [F.EAX]
					 , jump = NONE
					 , doc = "x86gen:366"}))))
			     		   
          (* AND *)
          | munchExp (T.BINOP (T.AND, e1, T.CONST i)) =
            result (fn r => (raiseBug "unexpected IR tree (AND1)"))

          | munchExp (T.BINOP (T.AND, T.CONST i, e1)) =
            result (fn r => (raiseBug "unexpected IR tree (AND2)"))

          | munchExp (T.BINOP (T.AND, e1, e2)) =
            result (fn r => (raiseBug "unexpected IR tree (AND3)"))

          (* OR *)
          | munchExp (T.BINOP (T.OR, e1, T.CONST i)) =
            result (fn r => (raiseBug "unexpected IR tree (OR1)"))

          | munchExp (T.BINOP (T.OR, T.CONST i, e1)) =
            result (fn r => (raiseBug "unexpected IR tree (OR2)"))

          | munchExp (T.BINOP (T.OR, e1, e2)) =
            result (fn r => (raiseBug "unexpected IR tree (OR3)"))

          (* Other constructs *)
          | munchExp (T.TEMP t) = t

          | munchExp (T.ESEQ (s, e)) = (raiseBug "unexpected IR tree (ESEQ)")

          | munchExp (T.NAME label) =
            result (fn r => (A.OPER {
				  assem = "\tmovl $"^ S.name label ^ ", `d0"
				, dst = [r]
				, src = [r]
				, jump = NONE
				, doc = "x86gen:399"}))

          | munchExp (T.CONST n) =
            result (fn r => (emit (A.OPER {
					assem = "\tmovl $" ^ int n ^ ", `d0" 
				      , dst = [r]
				      , src = []
				      , jump = NONE
				      , doc = "x86gen:407"})))

          (* If no match so far, complain *)
          | munchExp (tr as T.CALL (_, _)) =
            ( TextIO.output (TextIO.stdErr, "\nBUG: bad CALL in munchExp:\n")
            ; PT.printExp (TextIO.stdErr, tr)
            ; TextIO.flushOut TextIO.stdErr
            ; result (fn _ => emit (A.OPER { assem = "\t# bad CALL!"
                                           , src = []
                                           , dst = []
                                           , jump = NONE
                                           , doc = "x86gen:418"})))

          | munchExp (tr as T.BINOP (_, _, _)) =
            ( TextIO.output (TextIO.stdErr, "\nBUG: bad BINOP in munchExp:\n")
            ; PT.printExp (TextIO.stdErr, tr)
            ; TextIO.flushOut TextIO.stdErr
            ; result (fn _ => emit (A.OPER { assem = "\t# bad BINOP!"
                                           , src = []
                                           , dst = []
                                           , jump = NONE
                                           , doc = "x86gen:428"})))
    in
        munchStm stm;
        rev (!ilist)
    end

end (* X86Gen *)

