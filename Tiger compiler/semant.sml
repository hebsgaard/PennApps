signature SEMANT =
sig
  val transProg: Absyn.exp -> Translate.frag list
end

structure Semant :> SEMANT =
struct

structure S = Symbol
structure A = Absyn
structure E = Env
structure Ty = Types
structure Tr = Translate
structure PT = PrintTypes

exception TODO (* TODO: replace 'raise TODO' with suitable code *)

val err = ErrorMsg.error

(* Nesting of while- and for- loops, so we know if a break is allowed. *)
val nesting = ref 0
fun incNesting() = nesting := !nesting + 1
fun decNesting() = nesting := !nesting - 1

fun lookupTy tenv sym pos =
    let
        val tyOpt = S.look (tenv, sym)
    in
        case tyOpt of
	    SOME someType => someType
	  | NONE => (err pos ("Type is not defined in the enviroment: " ^ S.name sym) 
		    ; Ty.ERROR)
    end

(* NB: Some function names adjusted to use CamelCase more consistently.
 * For example: 'actual_ty' was renamed to 'actualTy' *)

fun lookUpFields(id:S.symbol, fields:(S.symbol*Ty.ty) list, counter) =
    let
	fun luf (id:S.symbol, []:(S.symbol*Ty.ty) list, counter) = (Ty.ERROR, counter)
	  | luf (id, x::xs:(S.symbol*Ty.ty) list, counter) = 
	    if (id = (#1 x))
	    then (#2 x, counter)
	    else luf(id, xs, (counter+1))
    in
	luf(id, fields, counter)
    end

fun getSymbolOfVar (variable) = 
    (case variable of 
	 A.SimpleVar(sym, pos) => sym
       | A.FieldVar(var, sym, pos) => getSymbolOfVar(var)
       | A.SubscriptVar(var, exp, pos) =>  getSymbolOfVar(var))

fun actualTy (Ty.NAME (s, ty)) pos =
    (case !ty of
	 NONE => (err pos "Undeclared type" ; Ty.ERROR)
       | SOME t => actualTy t pos)
  | actualTy t _ = t

fun isRecordType (Ty.RECORD _) = true
  | isRecordType _ = false

fun checkInt (ty, pos) =
    case ty of
        Ty.INT => ()
      | Ty.ERROR => ()
      | _ => err pos ("INT required, " ^
                      PT.asString ty ^ " provided")

fun checkString (ty, pos) = 
    case ty of 
	Ty.STRING => ()
      | Ty.ERROR =>() 
      | _ => err pos ("String required, " ^ PT.asString ty^ "provided")

fun checkUnit (ty, pos) =
    case ty of
	Ty.UNIT => ()
      | Ty.ERROR => ()
      | _ => err pos ("UNIT required, " ^ PT.asString ty ^" provided")

fun checkAssignable (declared: Ty.ty, assigned: Ty.ty, pos, msg) =
    let
        val aDeclared = actualTy declared pos
        val aAssigned = actualTy assigned pos
    in
        case aDeclared of
	    Ty.RECORD (_, u1) =>
	    (case aAssigned of
		 Ty.NIL => true
	       | Ty.RECORD (_, u2) =>
		 if (u1=u2)
		 then true
		 else (err pos ("Mismatch of the record unique ref in: " ^ msg) ; false)
	       | _ => (err pos ("Mismatch of the types in: " ^ msg) ; false))
	  | Ty.ARRAY(_, u1) =>
	    (case aAssigned of
		 Ty.ARRAY(_, u2) => 
		 if (u1=u2)
		 then true
		 else (err pos ("Mismatch of the array unique ref in: " ^ msg) ; false)
	       | _ => (err pos ("Mismatch of the types in: " ^ msg) ; false))
	    | Ty.NIL => 
	      (case aAssigned of 
	      Ty.RECORD _=> true
		| Ty.NIL => true
		| _ =>(err pos ("Mismatch of types in: " ^msg); false))
	  | x => 
	    (*Check all other cases*)
	    if (x= aAssigned) 
	    then true 
	    else (err pos ("Mismatch of the types in: " ^ msg) ; false)
    end
	
fun transTy (tenv, t) =
    let 
	(*Translate Absyn fielddata to Type recorddata*)
	fun transRecordData [] = []
	  | transRecordData ({name, escape, typ, pos} :: fieldList) =
	    let
		val (sym, pos1) = typ
	    in 
		(name, lookupTy tenv sym pos1):: transRecordData fieldList
	    end
    in
	case t of
	    (*ref (), is a unique ref*)
	    A.RecordTy(fielddata) => Ty.RECORD (transRecordData fielddata, ref ())
	  | A.ArrayTy(sym, pos) => Ty.ARRAY (lookupTy tenv sym pos, ref ())
	  | A.NameTy(sym, pos) => lookupTy tenv sym pos
    end
	
fun transExp (venv, tenv, level) =
    let
        fun trexp (A.NilExp) _ = {exp = Tr.nil2IR (), ty = Ty.NIL}
          | trexp (A.VarExp var) break = trvar var false break
          | trexp (A.IntExp i) _ = {exp = Tr.int2IR i, ty = Ty.INT}
          | trexp (A.StringExp (str, pos)) _ = {exp = Tr.string2IR str, ty = Ty.STRING}
          | trexp (A.OpExp {left, oper, right, pos}) break = 
	    if oper = A.PlusOp 
	       orelse oper = A.MinusOp
               orelse oper = A.TimesOp
               orelse oper = A.DivideOp
            then
                let
                    val {exp = lefte, ty = t} = trexp left break
		    val {exp = righte, ty = t1} = trexp right break
                    val right' = trexp right break
		    val left' = trexp left break
                in
                    (checkInt (t, pos); 
                     checkInt(t1, pos);
                     {exp = Tr.intOp2IR (oper, lefte, righte), ty=Ty.INT})
                end
	    else if oper = A.LtOp
                    orelse oper = A.LeOp
                    orelse oper = A.GtOp
                    orelse oper = A.GeOp
            then
                let 
                    val {exp = lefte, ty = t} = trexp left break
                    val {exp = righte, ty = t1} = trexp right break
                in
                    case t of 
			Ty.INT => (checkInt (t1, pos);
                                   {exp = Tr.intOp2IR (oper, lefte, righte), ty = Ty.INT})
                      | Ty.STRING => (checkString(t1, pos);
                                      {exp = Tr.stringOp2IR (oper, lefte, righte), ty = Ty.INT})
                      | _ => (err pos "can't compare these types"
				  ; {exp = Tr.nil2IR (), ty = Ty.ERROR})
                end
	    else if oper = A.EqOp
                    orelse oper = A.NeqOp
	    then
		let
		    val {exp = lefte, ty = t} = trexp left break
                    val {exp = righte, ty = t1} = trexp right break
		    val isAssignable = checkAssignable(t, t1, pos, "EQ and NEQ")
		in
		    if (isAssignable) 
		    then  
			case t of 
			    Ty.INT => {exp = Tr.intOp2IR (oper, lefte, righte),
				       ty = Ty.INT}
			  | Ty.STRING => {exp = Tr.stringOp2IR (oper, lefte, righte),
					  ty = Ty.INT}
			  | _ => {exp = Tr.nil2IR (), ty = Ty.INT} (*Types are array, record and nil*)
		    else (err pos "Types must be assignable in Eq and Neq"
			     ; {exp = Tr.nil2IR (), ty = Ty.ERROR})
		end
            else (err pos "Unknown operator"; {exp = Tr.nil2IR (), ty = Ty.ERROR})

          | trexp (A.CallExp {func, args, pos}) break =
	    let
		val  args' = map #1 args
		val fromLevel = level
		val expList = 
		    let 
			fun expListfun (args, expList) = 
			    (case args of
				 [] => expList
				|d::ds => (let
					      val (exp, pos) = d
					      val {exp=exp, ty=typ} = trexp exp break
					  in
					      (expListfun(ds, (expList @ (exp::[]))))
					  end))
		    in expListfun(args, [])
		    end
		fun argsMatch (formals, argsExp) = 
		    let 
			fun argsTy (formalTy, exp) = 
			    let
				val {exp, ty} = trexp exp break
				val expTy = actualTy ty pos
			    in
				if (formalTy = expTy)
				then ()
				else err pos ("Argument has incorrect type in: " ^ S.name func)
			    end
		    in
			if (length (formals) = length(argsExp))
			then ((map argsTy (ListPair.zip(formals, argsExp)));())
			else err pos ("Wrong amount of arguments in: " ^ S.name func)
		    end
	    in
		(case S.look (venv, func) of 
		     NONE =>( err pos ("Function does not exist: " ^ S.name func)
			    ; {exp =Tr.nil2IR (), ty = Ty.ERROR})
		   | SOME(Env.VarEntry {ty, ...})  => (err pos 
						   "Expected function, a variable was provided"
					      ; {exp =Tr.nil2IR (), ty = Ty.ERROR}) 
		   | SOME (E.FunEntry{formals=formals, result=resultTy, level=level', label}) => 
		     (argsMatch (formals, args')
		     ; (if resultTy = Ty.UNIT 
			then {exp = Tr.procCall2IR(level, fromLevel, label, expList), ty = resultTy}
			else {exp = Tr.funCall2IR(level, fromLevel, label, expList), ty = resultTy})))
	    end
          | trexp (A.IfExp {test, thn, els, pos}) break =
	    (case els of
		 NONE =>
		 let
		     val {exp = e1, ty = t1} = trexp test break
		     val {exp = e2, ty = t2} = trexp thn break
		 in
		     ( checkInt(t1, pos)
		     ; checkUnit(t2, pos)
		     ; {exp = Tr.ifThen2IR (e1, e2), ty = t2})
		 end
		 | SOME els =>
		   let
		       val {exp = e1, ty = t1} = trexp test break
		       val {exp = e2, ty = t2} = trexp thn break
		       val {exp = e3, ty = t3} = trexp els break
		       val thnTy = actualTy t2 pos
		       val elsTy = actualTy t3  pos
		   in
		       ( checkInt(t1, pos)
		       ; if (checkAssignable (thnTy, elsTy, pos, "Then and else expression")) 
			 then 
			     if thnTy = Ty.NIL
			     then {exp = Tr.ifThenElse2IR (e1, e2, e3), ty = elsTy}
			     else {exp = Tr.ifThenElse2IR (e1, e2, e3), ty = thnTy}
			 else ( err pos "Type of then expression must match type of else expression"
			      ;  {exp = Tr.nil2IR (), ty = Ty.UNIT} ))
		   end)

          | trexp (A.WhileExp {test, body, pos}) break = 
            let 
		val _ = incNesting()
		val newBreakPoint = Tr.newBreakPoint("")
		val {exp = testExp, ty = testTy} = trexp test break
		val {exp = bodyExp, ty = bodyTy} = trexp body (SOME (newBreakPoint))
		val _ = decNesting()
	    in
		(checkInt(testTy, pos);
		 checkUnit(bodyTy, pos);
		 {exp = Tr.while2IR(testExp, bodyExp, newBreakPoint) , ty = Ty.UNIT})
	    end

          | trexp (A.RecordExp {fields, typ, pos}) break =
            let
		val typ' = S.look(tenv, typ)
		val fieldNames = map #1 fields
		val (fieldTypes, fieldExps) = 
		    let
			fun listFun (fields, typeList: Ty.ty list, expList: Tr.exp list) = 
			    (case fields of
				 [] => (typeList, expList)
				|d::ds => (let
					      val (sy, exp, pos) = List.hd fields
					      val ds = List.tl fields
					      val {exp=exp1, ty=typ} = trexp exp break
					  in
					      (listFun(ds, (typeList @ (typ::[])), (expList @ (exp1::[]))))
					  end))
		    in
			listFun(fields, [], [])
		    end
	    in
		(case typ' of
		     NONE => (err pos "Type not defined" ; {exp =Tr.nil2IR(), ty=Ty.ERROR})
		   | SOME ty => 
		     let 
			 val realTy = actualTy ty pos
		     in
			 (case realTy of 
			      Ty.RECORD(rfields, u) =>
			      let
				  val rFieldNames = map #1 rfields
				  val rFieldTypes = map (fn t => actualTy t pos) (map #2 rfields)
				  fun checkFieldTypes (fields, fieldTyp)= 
				      (case fieldTyp of 
					  [] => {exp = Tr.record2IR(fieldExps), ty = Ty.RECORD(rfields, u)}
					| _ => 
					  let 
					      val field = List.hd fields
					      val tail1 =List.tl fields
					      val fieldty = List.hd fieldTyp
					      val tail2 = List.tl fieldTyp
					  in
					      if field = fieldty orelse field = Ty.NIL
					      then checkFieldTypes(tail1, tail2)
					      else (err pos "The fieldtypes do not match" ; {exp = Tr.nil2IR(), ty=Ty.RECORD(rfields, u)}) 
					  end)
			      in
				  if fieldNames = rFieldNames
				  then
				      checkFieldTypes(fieldTypes, rFieldTypes)
				  else (err pos "The IDs do not match in record" ; {exp = Tr.nil2IR(), ty=Ty.RECORD(rfields, u)})
			      end
			    | _ =>  (err pos ("Not a record type " ^ S.name typ); {exp = Tr.nil2IR(), ty = Ty.ERROR}))
		     end)
	    end

	  (* using Tr.record2IR, maybe Tr.nil2IR with errors *)

          | trexp (A.SeqExp []) _ = {exp = Tr.seq2IR([]), ty = Ty.UNIT}
            (* ensure there is some expression if the SeqExp is empty *)

          | trexp (A.SeqExp (aexps as (aexp'::aexps'))) break =
	    let 
		fun seq (aexps, expList) = 
		    let
			val (exp', pos') = List.hd aexps
			val tail = List.tl aexps
			val {exp = expExp, ty = expTy} = trexp exp' break
			val expTy' = actualTy expTy pos'
			val expList' = expList @ expExp::[]
		    in
			(case  tail of 
			    [] => 
			    (case expTy' of 
			    Ty.UNIT => ({exp = Tr.seq2IR(expList'), ty = expTy})
			      | _ => ( {exp = Tr.eseq2IR(expList'), ty = expTy}))
			 | _ => seq(tail, expList') )
		    end
	    in
		seq (aexps, [])
	    end

          | trexp (A.AssignExp {var, exp, pos}) break = 
            let 
		val {exp = varExp, ty = varTy} = trvar (var) true break
		val {exp = expExp, ty = expTy} = trexp exp break
		val varSym = getSymbolOfVar(var)
		val varOption = S.look(venv, varSym)
	    in
		case varOption of 
		    SOME var1 =>
		    (case var1 of 
			 E.VarEntry ({access, ty, assignable, escape}) => 
			 if assignable 
			 then
			     (if checkAssignable(varTy, expTy, pos, "assignable")
			      then
				  {exp = Tr.assign2IR(varExp, expExp), ty = Ty.UNIT}
			      else
				  {exp = Tr.nil2IR(), ty = Ty.ERROR})
			 else (err pos "Cannot assign to iterator variable"; {exp = Tr.nil2IR(), ty = Ty.ERROR})
		       | _ =>(err pos "Should be a variable"; {exp = Tr.nil2IR(), ty = Ty.ERROR}))
		  | NONE => (err pos "Iterator is undefined"; {exp = Tr.nil2IR(), ty = Ty.ERROR})
	    end

          | trexp (A.ForExp {var, escape, lo, hi, body, pos}) _ =
            let
		val _ = incNesting()
		val escape = false
		val access' = Tr.allocLocal level escape
		val newBreakPoint = Tr.newBreakPoint("forLoop")
		val venv' = S.enter(venv, var, E.VarEntry{access = access', ty = Ty.INT
							  , assignable = false, escape = ref(escape)})
		val {exp = loExp, ty = loTy} =trexp lo NONE
		val {exp = hiExp, ty = hiTy} =trexp hi NONE
		val {exp = bodyExp, ty = bodyTy} = transExp (venv', tenv, level) body (SOME (newBreakPoint))
		val _ = decNesting()
	    in
		(checkInt(loTy, pos);
		 checkInt(hiTy, pos);
		 checkUnit(bodyTy, pos);
		 {exp = Tr.for2IR(Tr.simpleVar(access', level), 
                               newBreakPoint, loExp, hiExp, bodyExp)
		  , ty = Ty.UNIT})
	    end   

	  (* using Tr.newBreakPoint, Tr.allocLocal, Tr.forIR *)

          | trexp (A.BreakExp pos) break =
           if !nesting > 0
	    then 
		case break of 
		    SOME break =>{exp = Tr.break2IR(break), ty = Ty.UNIT}
		  | _ => {exp = Tr.nil2IR(), ty = Ty.ERROR}
	    else
		(err pos "Break expression outside of loop"; {exp = Tr.nil2IR(), ty = Ty.UNIT})  
	  (* using Tr.break2IR *)

          | trexp (A.LetExp {decls, body, pos}) break =
            let
		val ({venv= venv', tenv=tenv'},explist) = transDecs(venv, tenv,level,break, [], decls)
                val {exp = bodyExp, ty=bodyTy} = transExp(venv', tenv', level) body break
            in
		{exp = Tr.let2IR(explist, bodyExp), ty = bodyTy}
            end

          | trexp (A.ArrayExp {typ, size, init, pos}) break =
	      let
		val {exp = initExp, ty = initTy} = trexp init break
		val {exp = sizeExp, ty = sizeTy} = trexp size break
		val typ' = S.look(tenv, typ)
	    in
 		(checkInt (sizeTy, pos);
		( case typ' of
		    NONE => (err pos ("Type not defined for array" ^S.name typ)
			    ; {exp = Tr.nil2IR(), ty = Ty.UNIT})
		   |  SOME ty => 
		      let
			  val realTy = actualTy ty pos 
			  val realInitTy = actualTy initTy  pos
		      in
			  (case realTy of 
			       Ty.ARRAY(ty', u) => 
			       if (realInitTy = actualTy ty' pos) 
			       then {exp = Tr.array2IR(sizeExp, initExp), ty = Ty.ARRAY(actualTy ty' pos, u)}
			       else (err pos "Tried to insert wrong type into array"; {exp = Tr.nil2IR(), ty = Ty.ERROR})
			    | _ => (err pos "Should be an array type"; {exp = Tr.nil2IR(), ty = Ty.ERROR}))
		      end))				  
	    end

        (* NB: trvar must generate a tree describing the given
         * variable such that it will work for both evaluation and
         * assignment; any expression will be fine for evaluation,
         * but assignment only works with MOVE(TEMP _, _) and
         * MOVE(MEM _, _) because we can only store the new value
         * into a register or into a memory cell (error cases can
         * generate a NoOp value (Ex (CONST 0)), so they avoid
         * the problem).  This means that Tr.simpleVar, Tr.fieldVar, 
         * and Tr.subscript2IR must return an Ex (MEM _) or an 
         * Ex (TEMP _).
         *)

        and trvar (A.SimpleVar (id, pos)) mutationRequested break =
	    (case S.look(venv, id) of 
		 SOME (E.VarEntry{access, ty, assignable, escape}) => 
		 {exp = Tr.simpleVar(access, level), ty = actualTy ty pos}
	       | SOME (E.FunEntry _) => (err pos "Expected simple var but a function was provided"; 
					 {exp = Tr.nil2IR(), ty =Ty.ERROR}) 
	       | NONE => ((err pos ("Undefined variable " ^S.name id));
			  {exp = Tr.nil2IR(), ty = Ty.UNIT}))

	  (* using Tr.simpleVar *)

          | trvar (A.FieldVar (var, id, pos)) _ break =
	    let
		(*Her er jeg ikke sikker på vi skal bruge true*)
		val {exp = varExp, ty = varTy} = trvar var true break
	    in
		(case varTy of
		     Ty.RECORD(fields, _) =>
		     let 
			 val (fieldType, counter) = lookUpFields(id, fields, 1)
		     in
			 (case fieldType of
			      Ty.ERROR => (err pos ("Symbol is not in the record") ; 
					   {exp = Tr.nil2IR(), ty=Ty.ERROR })
			    | ty1 => {exp = Tr.fieldVar(varExp, counter), ty = ty1})
		     end
		   | _ => (err pos "Fieldvar has to be a record type" ; {exp = Tr.nil2IR(), ty = Ty.ERROR})) 
	    end
	    
	  (* using Tr.fieldVar *)

          | trvar (A.SubscriptVar (var, exp, pos)) mutationRequested break =
            let
		val {exp = expExp, ty = expTy} = trexp exp break
		val {exp = varExp, ty = varTy} = trvar var mutationRequested break
		val varTy = actualTy varTy  pos
	    in
		(checkInt(expTy, pos)
		; (case varTy of
		       Ty.ARRAY (ty1, _) => {exp = Tr.subscript2IR(varExp, expExp), ty = actualTy ty1 pos}
		     | _ => (err pos "SubscriptVar is not in an array" ; {exp = Tr.nil2IR(), ty = Ty.ERROR}))) 
	    end
		
	    (* ignore 'mutationRequested': all array entries are mutable *)
            (* using Tr.subscript2IR *)
	    in
		trexp
		end

and transDec ( venv                     (* environment var -> type *)
             , tenv                     (* environment tyname -> type *)
             , level                    (* frame of enclosing function *)
             , break                    (* jump to this label on 'break' *)
             , explist                  (* accumulate decl elaboration code *)
             , A.VarDec { name          (* AST of declaration to translate *)
                        , escape
                        , typ = NONE
                        , init
                        , pos}) =
    let
	val {exp, ty} = (transExp(venv, tenv, level)) init break
	val escape1 = true
	val access = Tr.allocLocal level escape1
	val simpleVarExp = Tr.simpleVar(access, level)
	val assignExp = Tr.assign2IR(simpleVarExp, exp)
	val venv' = S.enter(venv, name, 
			    E.VarEntry{access = access, ty = (actualTy ty pos), assignable = true, escape =escape})
	val explist' = [assignExp] @ explist
        (* TODO: create venv': venv extended with the new variable
         * and explist': explist extended with its init code; 
         * will use Tr.allocLocal, Tr.assign2IR, Tr.simpleVar *)
    in
	if ty = Ty.NIL
	then (err pos "Can't assign variable to NIL"; ({tenv=tenv, venv=venv}, explist))
	else ({tenv = tenv, venv = venv'}, explist')
    end

  | transDec (venv, tenv, level, break, explist, A.VarDec { name
                                                          , escape
                                                          , typ = SOME (s, pos)
                                                          , init
                                                          , pos = pos1}) =
    let
	val {exp, ty} = (transExp(venv, tenv, level)) init break
	val escape1 = true
	val access = Tr.allocLocal level escape1
	val simpleVarExp = Tr.simpleVar(access, level)
	val assignExp = Tr.assign2IR(simpleVarExp, exp)
	val venv' = S.enter(venv, name, 
			    E.VarEntry{access = access, ty = (actualTy ty pos), assignable=true, escape = escape})
	val explist' = [assignExp] @ explist
	val typ' = S.look(tenv, s)
    in
	(case typ' of 
	     NONE => (err pos "Type is not defined" ; ({tenv = tenv, venv = venv}, explist))
	   | SOME ty1 => (checkAssignable(ty1, ty, pos1, "Type in var dec should match")
			 ; ({tenv = tenv, venv = venv'}, explist')))
    end
					 
  | transDec (venv, tenv, level, break, explist, A.TypeDec tydecls) =
            let
	val pos = #pos (List.hd tydecls)
	(*Check for cycles in type declarations*)
	fun checkCycle (env, symNames, resList) =
	   (case symNames of 
		[] => true
	      | _ => 
		let 
		    val name = List.hd symNames
		    val tail = List.tl symNames
		    val typ = S.look (env, name)
		in
		    (case typ of 
			 SOME(Ty.NAME(sym, ty))=>
			 (case !ty of 
			      SOME(Ty.NAME(sym1, ty1))=> 
			      ( if (List.exists (fn x => x = name) resList)
				then false
				else (checkCycle(env, [sym1], resList@[name])))
			    | _ => checkCycle (env, tail, []))
		       | _ => checkCycle (env, tail, []))
		end)
	val names = map #name tydecls
	val types = map #ty tydecls
	fun parse1 (tyName, env, tyEnv) =
	    (case tyName of 
		 [] => env
	       | _ => 
		 let 
		     val name = List.hd tyName
		     val tail = List.tl tyName
		     val tyEnv' = 
			 (case S.look(tyEnv, name) of
			      NONE => S.enter(tyEnv, name, Ty.NAME(name, ref(NONE)))
			    | SOME _ => (err pos ("Type already exists: " ^ S.name name); tyEnv))
		     val env' = S.enter(env, name, Ty.NAME(name, ref(NONE)))
		 in
		     parse1(tail, env', tyEnv')
		 end)
	val tenv' = parse1 (names, tenv, E.baseTenv)
	val nameTypes = map (fn t => transTy (tenv', t)) types
	fun parse2 (name, tyName) = 
	    let val (SOME (Ty.NAME(_,ref'))) = S.look(tenv',name)
            in ref' := SOME tyName
           end
	val _ = app parse2 (ListPair.zip(names, nameTypes))
    in
	if (checkCycle(tenv', names, []))
	then ({tenv = tenv', venv = venv}, explist)
	else (err pos "There is a cycle in the type declarations"; ({tenv = tenv, venv = venv}, explist))
    end

  | transDec (venv, tenv, level, break, explist, A.FunctionDec fundecls) =
    let
	val fundef = ref []
        val levels = ref []
	fun paramsTy ({ name: S.symbol
		      , escape: bool ref
		      , typ: (S.symbol * A.pos)
		      , pos: A.pos})=
	    let
		val typ' = #1 typ
	    in
		case S.look (tenv, typ') of 
		    NONE => (err pos ("Type does not exist: " ^S.name typ'); 
			     {name = name, ty = Ty.ERROR})
		  | SOME ty=> {name = name, ty = actualTy ty pos}
	    end
	fun  resTy  (result) = 
	     case result of 
		 SOME(sym, pos) =>
		 (case S.look(tenv, sym) of
		      NONE => (err pos "Return type should be in scope" ; Ty.UNIT )
		    | SOME ty => actualTy ty pos)
	       | NONE => Ty.UNIT
	fun funDecs (env, venv1, decls) =
	    (case decls of
		 [] => venv1
		|_ =>
		 let 
		     val  { name: S.symbol
			  , params: A.fielddata list
			  , result : (S.symbol * A.pos) option
			  , body: A.exp
			  , pos: A.pos} = List.hd decls
		     val tail = List.tl decls
		     val res = resTy(result)
		     val params' = map paramsTy params

		     val formals = (map (fn ({name,escape,typ,pos}) => (!escape)) (params))
		     val funlabel = Temp.newLabel("Function_" ^ S.name name)
		     val newLevel = Tr.newLevel{parent=level,
						 name=funlabel,
						 formals=formals}
		     val _ = levels := newLevel::(!levels)

		     val env' = 
			 (case S.look(env, name) of 
			      NONE =>  S.enter(env, name, E.FunEntry{formals = map #ty params', result = res, label = funlabel, level = newLevel})
			    | SOME _ => (err pos ("Function: " ^S.name name^ " already exists"); env))
		     val venv' =  S.enter(venv1, name, E.FunEntry{formals = map #ty params', result = res, label = funlabel, level = newLevel})
		 in
		     funDecs(env', venv', tail)
		 end)
	fun checkExp (env, decls) = 
	    (case decls of 
		 [] => true
	       | _ =>
		 let
		     val  { name: S.symbol
			  , params: A.fielddata list
			  , result : (S.symbol * A.pos) option
			  , body: A.exp
			  , pos: A.pos} = List.hd decls
		     val tail = List.tl decls
		     val params' = map paramsTy params
		     val res = resTy (result)
		     fun enterparam (param: A.fielddata, env') = 
			 let
			     val access = Tr.allocLocal (hd(!levels)) (!(#escape param))
			     val (sym, pos1) = (#typ param)
			     val ty = lookupTy tenv sym pos1
			     val ty' = actualTy ty pos
			 in
			     S.enter (env', (#name param), 
				      E.VarEntry{access = access, ty=ty', 
						 assignable = true, escape = (#escape param)})
			 end
		     val venv'' = foldr enterparam env params
		     val {exp = bodyExp, ty = bodyTy} = transExp (venv'', tenv, level) body break
		     val checkLevel = hd ((!levels))
		     val _ = (levels := tl ((!levels)))
		 in
		    ( fundef := (!fundef)@[bodyExp]; 
		     if res = bodyTy 
		     then 
			 (case res of 
			      Ty.UNIT =>
			      (Tr.procEntryExit({body = bodyExp, level =checkLevel});
			      checkExp(env, tail))
			    | _ =>
 			      (Tr.funEntryExit({body = bodyExp, level = checkLevel});
			     checkExp(env, tail)))
		     else (err pos ("Mismatch in return types in: " ^ S.name name ); checkExp(env, tail)))
		 end
	    ) 
	val funvenv = funDecs (E.baseVenv, venv, fundecls)
	val fundef' = !fundef
        val explist' = (explist@fundef')
    in
        if (checkExp(funvenv, fundecls)) 
	then ({venv=funvenv, tenv=tenv}, explist')
	else({venv=venv, tenv = tenv}, explist)
    end



    (* TODO: create venv': venv extended with all the functions 
         * declared by 'fundecls'; also process the function bodies;
         * will use Tr.newLevel, Tr.accessOfFormal, Tr.procEntryExit,
         * Tr.funEntryExit *)

and transDecs (venv, tenv, level, break, explist, decls) =
    case decls of
        [] => ({venv = venv, tenv = tenv}, explist)
      | (d::ds) =>
        let
            val ({venv = venv', tenv = tenv'}, explist') =
                transDec (venv, tenv, level, break, explist, d)
        in
            transDecs (venv', tenv', level, break, explist', ds)
        end

fun transProg absyn =
    let
        val {exp, ty} = transExp (Env.baseVenv, Env.baseTenv, Env.initLevel)
                                 absyn NONE
    in
        if ty=Ty.UNIT
        then Tr.procEntryExit {level = Env.initLevel, body = exp}
        else Tr.funEntryExit {level = Env.initLevel, body = exp};
        Tr.getResult ()
    end

end (* Semant *)

