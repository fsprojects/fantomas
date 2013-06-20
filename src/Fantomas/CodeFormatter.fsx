#r "../../lib/FSharp.Compiler.dll"

#load "TokenMatcher.fs"
#load "FormatConfig.fs"
#load "SourceParser.fs"
#load "SourceTransformer.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.CodePrinter
open Fantomas.CodeFormatter

let config = { FormatConfig.Default with PageWidth = 120;SpaceBeforeArgument=true }

let test s = formatSourceString false s config |> printfn "%A"

test """
   /// XML COMMENT A
     // Other comment
let f() = 
      // COMMENT A
    let y = 1
      /// XML COMMENT B
    let z = 1
  // COMMENT B
    x + x + x

""" 

// FAILS - sticky-right comment becomes sticky-left
test """
1 +
// Comment
1""" 

test """
1 + // Comment
1""" 


// FAILS - sticky-right comment becomes sticky-left
test """
1 
// Comment
+ 1""" 

// FAILS - sticky-right comment becomes sticky-left
test """
let f() = 
    1 
    // Comment
    + 1
""" 

test """
1 + (* Comment *) 1""" 

test """ 1 + let x = 3 in x + x"""

test """ (let x = 3 in x + x) + (let x = 3 in x + x)"""

test """ (let x = 3 in x + x) + (let x = 3 in x + x)"""

test """
let f() = 
  let x = 1     // COMMENT
  x + x
""" 

test """
let f() = 
    // CommentB
    x + x
""" 

test """
/// XML comment
type X = 
   /// Hello
   | A 
   /// Goodbye
   | B
""" 

test """
/// XML comment
type X = 
   | A  // Hello
   | B // Goodbye
""" 

test """
/// XML comment
type (* comment *) X = 
   | A  // Hello
   | B // Goodbye
""" 

test """
/// XML comment
type (* comment *) 
     X = 
   | A  // Hello
   | B // Goodbye
""" 

// INADEQUATE: undentation not far enough
test """
let IsNonErasedTypar (tp:Typar) = not tp.IsErased
let DropErasedTypars (tps:Typar list) = tps |> List.filter IsNonErasedTypar
let DropErasedTyargs tys = tys |> List.filter (fun ty -> match ty with TType_measure _ -> false | _ -> true) 
let AddSpecialNameFlag (mdef:ILMethodDef) = { mdef with IsSpecialName = true }

"""

test """
let x = 1
#if SILVERLIGHT
let useHiddenInitCode = false
#else
let useHiddenInitCode = true
#endif
let y = 2
"""

test """
let iLdcZero = AI_ldc (DT_I4,ILConst.I4 0)
let iLdcInt64 i = AI_ldc (DT_I8,ILConst.I8 i)
let iLdcDouble i = AI_ldc (DT_R8,ILConst.R8 i)
let iLdcSingle i = AI_ldc (DT_R4,ILConst.R4 i)

"""

test """
let mkLdfldMethodDef (ilMethName,reprAccess,isStatic,ilTy,ilFieldName,ilPropType) =
   let ilFieldSpec = mkILFieldSpecInTy(ilTy,ilFieldName,ilPropType)
   let ilReturn = mkILReturn ilPropType
   let ilMethodDef = 
       if isStatic then 
           mkILNonGenericStaticMethod (ilMethName,reprAccess,[],ilReturn,mkMethodBody(true,emptyILLocals,2,nonBranchingInstrsToCode [mkNormalLdsfld ilFieldSpec],None))
       else 
           mkILNonGenericInstanceMethod (ilMethName,reprAccess,[],ilReturn,mkMethodBody (true,emptyILLocals,2,nonBranchingInstrsToCode [ mkLdarg0; mkNormalLdfld ilFieldSpec],None))
   ilMethodDef |> AddSpecialNameFlag
"""

test """
let ChooseParamNames fieldNamesAndTypes = 
    let takenFieldNames = fieldNamesAndTypes |> List.map p23 |> Set.ofList

    fieldNamesAndTypes
    |> List.map (fun (ilPropName,ilFieldName,ilPropType) -> 
        let lowerPropName = String.uncapitalize ilPropName 
        let ilParamName = if takenFieldNames.Contains(lowerPropName) then ilPropName else lowerPropName 
        ilParamName,ilFieldName,ilPropType)

"""

test """

let rec CheckCodeDoesSomething code = 
    match code with 
    | ILBasicBlock bb -> Array.fold (fun x i -> x || match i with (AI_ldnull | AI_nop | AI_pop) | I_ret |  I_seqpoint _ -> false | _ -> true) false bb.Instructions
    | GroupBlock (_,codes) -> List.exists CheckCodeDoesSomething codes
    | RestrictBlock (_,code) -> CheckCodeDoesSomething code
    | TryBlock _ -> true 
"""

test """
let ChooseFreeVarNames takenNames ts =
    let tns = List.map (fun t -> (t,None)) ts
    let rec chooseName names (t,nOpt) = 
        let tn = match nOpt with None -> t | Some n -> t + string n
        if Zset.contains tn names then
          chooseName names (t,Some(match nOpt with None ->  0 | Some n -> (n+1)))
        else
          let names = Zset.add tn names
          names,tn
    let names    = Zset.empty String.order |> Zset.addList takenNames
    let _names,ts = List.fmap chooseName names tns
    ts

"""


test """
/// Non-local information related to internals of code generation within an assembly
type IlxGenIntraAssemblyInfo = 
    { /// A table recording the generated name of the static backing fields for each mutable top level value where 
      /// we may need to take the address of that value, e.g. static mutable module-bound values which are structs. These are 
      /// only accessible intra-assembly. Across assemblies, taking the address of static mutable module-bound values is not permitted.
      /// The key to the table is the method ref for the property getter for the value, which is a stable name for the Val's
      /// that come from both the signature and the implementation.
      StaticFieldInfo : Dictionary<ILMethodRef, ILFieldSpec> }

"""

// IMPERFECT - first sticky-to-the-left comment has only one space.
test """
/// Non
type IlxGenIntraAssemblyInfo = 
    { // A
      // we
      // that
      StaticFieldInfo : int }

"""

test """
[<NoEquality>]
type IlxGenOptions = 
    { fragName: string
      generateFilterBlocks: bool
      workAroundReflectionEmitBugs: bool
      emitConstantArraysUsingStaticDataBlobs: bool
      // If this is set, then the last module becomes the "main" module and its toplevel bindings are executed at startup 
      mainMethodInfo: Tast.Attribs option
      localOptimizationsAreOn: bool
      generateDebugSymbols: bool
      testFlagEmitFeeFeeAs100001: bool
      ilxBackend: IlxGenBackend
      /// Indicates the code is being generated in FSI.EXE and is executed immediately after code generation
      /// This includes all interactively compiled code, including #load, definitions, and expressions
      isInteractive: bool 
      // Indicates the code generated is an interactive 'it' expression. We generate a setter to allow clearing of the underlying
      // storage, even though 'it' is not logically mutable
      isInteractiveItExpr: bool
      // Indicates System.SerializableAttribute is available in the target framework
      netFxHasSerializableAttribute : bool
      /// Whenever possible, use callvirt instead of call
      alwaysCallVirt: bool}

"""

/// FAIL - attributes
test """
[<NoEquality; NoComparison>]
type IlxGenOptions = 
    { fragName: string
      generateFilterBlocks: bool
      workAroundReflectionEmitBugs: bool
      emitConstantArraysUsingStaticDataBlobs: bool
      // If this is set, then the last module becomes the "main" module and its toplevel bindings are executed at startup 
      mainMethodInfo: Tast.Attribs option
      localOptimizationsAreOn: bool
      generateDebugSymbols: bool
      testFlagEmitFeeFeeAs100001: bool
      ilxBackend: IlxGenBackend
      /// Indicates the code is being generated in FSI.EXE and is executed immediately after code generation
      /// This includes all interactively compiled code, including #load, definitions, and expressions
      isInteractive: bool 
      // Indicates the code generated is an interactive 'it' expression. We generate a setter to allow clearing of the underlying
      // storage, even though 'it' is not logically mutable
      isInteractiveItExpr: bool
      // Indicates System.SerializableAttribute is available in the target framework
      netFxHasSerializableAttribute : bool
      /// Whenever possible, use callvirt instead of call
      alwaysCallVirt: bool}

"""


/// INADEQUATE (block comment not sticky-left)
test """
[<NoEquality>]
type IlxClosureInfo = 
    { cloILFormalRetTy: ILType;
      /// An immutable array of free variable descriptions for the closure
      cloILFreeVars: IlxClosureFreeVar[]; 
      cloFreeVars: Val list; (* nb. the freevars we actually close over *)
      ilCloLambdas: IlxClosureLambdas;
    }
"""

test """
let AddStorageForVal g (v,s) eenv = 
    let eenv = { eenv with valsInScope = eenv.valsInScope.Add v s }
    // If we're compiling fslib then also bind the value as a non-local path to 
    // allow us to resolve the compiler-non-local-references that arise from env.fs
    //
    // Do this by generating a fake "looking from the outside in" non-local value reference for
    // v, dereferencing it to find the corresponding signature Val, and adding an entry for the signature val.
    //
    // A similar code path exists in ilxgen.fs for the tables of "optimization data" for values
    if g.compilingFslib then 
        // Passing an empty remap is sufficient for FSharp.Core.dll because it turns out the remapped type signature can
        // still be resolved.
        match tryRescopeVal g.fslibCcu Remap.Empty  v with 
        | None -> eenv
        | Some vref -> 
            match vref.TryDeref with
            | None -> 
                //let msg = sprintf "could not dereference external value reference to something in FSharp.Core.dll during code generation, v.MangledName = '%s', v.Range = %s" v.MangledName (stringOfRange v.Range)
                //System.Diagnostics.Debug.Assert(false, msg)
                eenv
            | Some gv -> 
                { eenv with valsInScope = eenv.valsInScope.Add gv s }
    else 
        eenv

"""

// INADEQUATE: block comment not indented properly. Do we care?
test """
let StorageForVal m v eenv = 
    let v = 
        try eenv.valsInScope.[v]
        with :? KeyNotFoundException ->
          (* REVIEW: The  binary will probably still be written under these error conditions.
           *         That is useful when debugging the compiler, but not in Retail mode.
           *         Fail with an internal error if Retail? *)
          (* // Diagnostics for bug://4046
           * let vals = eenv.valsInScope.Contents |> Zmap.toList
           * vals |> List.iter (printf "v,s = %A\n")          
           *)
          assert false
          errorR(Error(FSComp.SR.ilUndefinedValue(showL(vspecAtBindL v)),m)); 
          notlazy (Arg 668(* random value for post-hoc diagnostic analysis on generated tree *) )
    v.Force()
"""

// INADEQUATE: undenation for "fun" not good enough
test """
/// Assembly generation buffers 
type AssemblyBuilder(cenv:cenv) as mgbuf = 
    // The Abstract IL table of types 
    let gtdefs= new TypeDefsBuilder() 
    // The definitions of top level values, as quotations. 
    let mutable reflectedDefinitions : System.Collections.Generic.Dictionary<Tast.Val,(string * int * Expr)> = System.Collections.Generic.Dictionary(HashIdentity.Reference)
    // A memoization table for generating value types for big constant arrays  
    let vtgenerator=
         new MemoizationTable<(CompileLocation * int) , ILTypeSpec>
              ((fun (cloc,size) -> 
                 let name   = CompilerGeneratedName ("T" + string(newUnique()) + "_" + string size + "Bytes") // Type names ending ...$T<unique>_37Bytes
                 let vtdef  = mkRawDataValueTypeDef cenv.g.ilg (name,size,0us)
                 let vtref = NestedTypeRefForCompLoc cloc vtdef.Name 
                 let vtspec = mkILTySpec(vtref,[])
                 let vtdef = {vtdef with Access= ComputeTypeAccess vtref true}
                 mgbuf.AddTypeDef(vtref, vtdef, false, true);
                 vtspec), 
               keyComparer=HashIdentity.Structural)

    let mutable explicitEntryPointInfo : ILTypeRef option  = None

    /// static init fields on script modules.
    let mutable scriptInitFspecs : (ILFieldSpec * range) list = []    
    
    member mgbuf.AddScriptInitFieldSpec(fieldSpec,range) =
        scriptInitFspecs <- (fieldSpec,range) :: scriptInitFspecs
        
    /// This initializes the script in #load and fsc command-line order causing their
    /// sideeffects to be executed.
    member mgbuf.AddInitializeScriptsInOrderToEntryPoint() = 
        // Get the entry point and intialized any scripts in order.
        match explicitEntryPointInfo with
        | Some tref ->
            let IntializeCompiledScript(fspec,m) =
                mgbuf.AddExplicitInitToSpecificMethodDef((fun md -> md.IsEntryPoint), tref, fspec, GenPossibleILSourceMarker cenv m, [], [])              
            scriptInitFspecs |> List.iter IntializeCompiledScript
        | None -> ()

     

    member mgbuf.GenerateRawDataValueType(cloc,size) = 
        // Byte array literals require a ValueType of size the required number of bytes.
        // With fsi.exe, S.R.Emit TypeBuilder CreateType has restrictions when a ValueType VT is nested inside a type T, and T has a field of type VT.
        // To avoid this situation, these ValueTypes are generated under the private implementation rather than in the current cloc. [was bug 1532].
        let cloc = CompLocForPrivateImplementationDetails cloc
        vtgenerator.Apply((cloc,size))

    member mgbuf.AddTypeDef(tref:ILTypeRef, tdef, eliminateIfEmpty, addAtEnd) = 
        gtdefs.FindNestedTypeDefsBuilder(tref.Enclosing).AddTypeDef(tdef, eliminateIfEmpty, addAtEnd)

    member mgbuf.GetCurrentFields(tref:ILTypeRef) =
        gtdefs.FindNestedTypeDefBuilder(tref).GetCurrentFields();

    member mgbuf.AddReflectedDefinition(vspec : Tast.Val,expr) = 
        // preserve order by storing index of item
        let n = reflectedDefinitions.Count
        reflectedDefinitions.Add(vspec, (vspec.CompiledName, n, expr))
   
    member mgbuf.ReplaceNameOfReflectedDefinition(vspec, newName) = 
        match reflectedDefinitions.TryGetValue vspec with
        | true, (name, n, expr) when name <> newName -> reflectedDefinitions.[vspec] <- (newName, n, expr)
        | _ -> ()

    member mgbuf.AddMethodDef(tref:ILTypeRef,ilMethodDef) = 
        gtdefs.FindNestedTypeDefBuilder(tref).AddMethodDef(ilMethodDef);
        if ilMethodDef.IsEntryPoint then 
            explicitEntryPointInfo <- Some(tref)

    member mgbuf.AddExplicitInitToSpecificMethodDef(cond,tref,fspec,sourceOpt,feefee,seqpt) = 
    // Authoring a .cctor with effects forces the cctor for the 'initialization' module by doing a dummy store & load of a field 
    // Doing both a store and load keeps FxCop happier because it thinks the field is useful 
        let instrs = 
            [ yield! (if condition "NO_ADD_FEEFEE_TO_CCTORS" then [] elif condition "ADD_SEQPT_TO_CCTORS"  then seqpt else feefee) // mark start of hidden code
              yield mkLdcInt32 0; 
              yield mkNormalStsfld fspec; 
              yield mkNormalLdsfld fspec; 
              yield AI_pop]   
        gtdefs.FindNestedTypeDefBuilder(tref).PrependInstructionsToSpecificMethodDef(cond,instrs,sourceOpt) 

    member mgbuf.AddEventDef(tref,edef) = 
        gtdefs.FindNestedTypeDefBuilder(tref).AddEventDef(edef)

    member mgbuf.AddFieldDef(tref,ilFieldDef) = 
        gtdefs.FindNestedTypeDefBuilder(tref).AddFieldDef(ilFieldDef)

    member mgbuf.AddOrMergePropertyDef(tref,pdef,m) = 
        gtdefs.FindNestedTypeDefBuilder(tref).AddOrMergePropertyDef(pdef,m)

    member mgbuf.Close() = 
        // old implementation adds new element to the head of list so result was accumulated in reversed order
        let orderedReflectedDefinitions = 
            [for (KeyValue(vspec, (name, n, expr))) in reflectedDefinitions -> n, ((name,vspec), expr)]
            |> List.sortBy (fst >> (~-)) // invert the result to get 'order-by-descending' behavior (items in list are 0..* so we don't need to worry about int.MinValue)
            |> List.map snd
        gtdefs.Close(), orderedReflectedDefinitions
    member mgbuf.cenv = cenv
    member mgbuf.GetExplicitEntryPointInfo() = explicitEntryPointInfo
"""

test """
let FeeFee (cenv:cenv) = if cenv.opts.testFlagEmitFeeFeeAs100001 then 100001 else 0x00feefee
let FeeFeeInstr (cenv:cenv) doc = 
      I_seqpoint (ILSourceMarker.Create(document = doc,
                                        line = FeeFee cenv,
                                        column = 0,
                                        endLine = FeeFee cenv,
                                        endColumn = 0))

"""


// FAIL: insertion of "elif" for "else if" leads to mismatches in tokens that we never recover from....
// TECHNIQUE : allow "elif" to "else if"
test """
/// Buffers for IL code generation
type CodeGenBuffer(m:range,
                   mgbuf: AssemblyBuilder,
                   methodName,
                   alreadyUsedArgs:int,
                   alreadyUsedLocals:int,
                   zapFirstSeqPointToStart:bool) = 

    let locals = new ResizeArray<((string * (Mark * Mark)) list * ILType)>(10)
    let codebuf = new ResizeArray<ILInstr>(200)
    let exnSpecs = new ResizeArray<ILExceptionSpec>(10)

    // Keep track of the current stack so we can spill stuff when we hit a "try" when some stuff
    // is on the stack.        
    let mutable stack : ILType list = []
    let mutable nstack=0
    let mutable maxStack=0
    let mutable seqpoint= None
    
    let codeLabelToPC : Dictionary<ILCodeLabel,int> = new Dictionary<_,_>(10)
    let codeLabelToCodeLabel : Dictionary<ILCodeLabel,ILCodeLabel> = new Dictionary<_,_>(10)
    
    let rec computeCodeLabelToPC n lbl = 
        if n = System.Int32.MaxValue then error(InternalError("recursive label graph",m))
        if codeLabelToCodeLabel.ContainsKey lbl then 
            computeCodeLabelToPC (n+1) codeLabelToCodeLabel.[lbl]
        else
           codeLabelToPC.[lbl] 
    
    let mutable lastSeqPoint = None
    // Add a nop to make way for the first sequence point. There is always such a 
    // sequence point even when zapFirstSeqPointToStart=false
    do if mgbuf.cenv.opts.generateDebugSymbols  then codebuf.Add(AI_nop);

    member cgbuf.DoPushes (pushes: Pushes) = 
        for ty in pushes do 
           stack <- ty :: stack; 
           nstack <- nstack + 1;
           maxStack <- Operators.max maxStack nstack

    member cgbuf.DoPops (n:Pops) = 
        for i = 0 to n - 1 do
           match stack with
           | [] -> 
               let msg = sprintf "pop on empty stack during code generation, methodName = %s, m = %s" methodName (stringOfRange m)
               System.Diagnostics.Debug.Assert(false, msg)
               warning(InternalError(msg,m));
           | _ :: t -> 
               stack <- t; 
               nstack <- nstack - 1

    member cgbuf.GetCurrentStack() = stack
    member cgbuf.AssertEmptyStack() = 
        if nonNil stack then 
            let msg = sprintf "stack flush didn't work, or extraneous expressions left on stack before stack restore, methodName = %s, stack = %+A, m = %s" methodName stack (stringOfRange m)
            System.Diagnostics.Debug.Assert(false, msg)
            warning(InternalError(msg,m));
        ()

    member cgbuf.EmitInstr(pops,pushes,i) = 
        cgbuf.DoPops pops;
        cgbuf.DoPushes pushes;
        codebuf.Add i

    member cgbuf.EmitInstrs (pops,pushes,is) = 
        cgbuf.DoPops pops;
        cgbuf.DoPushes pushes;
        is |> List.iter codebuf.Add 

    member cgbuf.GetLastSequencePoint() = 
        lastSeqPoint
       
    member private cgbuf.EnsureNopBetweenDebugPoints() = 
        // Always add a nop between sequence points to help .NET get the stepping right
        // Don't do this after a FeeFee marker for hidden code
        if (codebuf.Count > 0 && 
             (match codebuf.[codebuf.Count-1] with 
              | I_seqpoint sm when sm.Line <> FeeFee mgbuf.cenv -> true 
              | _ -> false)) then 
        
            codebuf.Add(AI_nop);

    member cgbuf.EmitSeqPoint(src) = 
        if mgbuf.cenv.opts.generateDebugSymbols then 
            cgbuf.EnsureNopBetweenDebugPoints()

            let attr = GenILSourceMarker mgbuf.cenv.g src
            assert(isSome(attr));
            let i = I_seqpoint (Option.get attr)
            codebuf.Add i;
            // Save the first sequence point away to snap it to the top of the method
            match seqpoint with 
            | Some _ -> ()
            | None -> seqpoint <- Some i
            // Save the last sequence point away so we can make a decision graph look consistent (i.e. reassert the sequence point at each target)
            lastSeqPoint <- Some src
            
    // For debug code, emit FeeFee breakpoints for hidden code, see http://blogs.msdn.com/jmstall/archive/2005/06/19/FeeFee_SequencePoints.aspx
    member cgbuf.EmitStartOfHiddenCode() = 
        if mgbuf.cenv.opts.generateDebugSymbols && not mgbuf.cenv.opts.localOptimizationsAreOn then 
            let doc = mgbuf.cenv.g.memoize_file m.FileIndex
            codebuf.Add(FeeFeeInstr mgbuf.cenv doc);   

    member cgbuf.EmitExceptionClause(clause) = 
         exnSpecs.Add clause

    member cgbuf.GenerateDelayMark(_nm) = 
         let lab = IL.generateCodeLabel()
         Mark lab

    member cgbuf.SetCodeLabelToCodeLabel(lab1,lab2) = 
#if DEBUG
        if codeLabelToCodeLabel.ContainsKey(lab1) then 
            let msg = sprintf "two values given for label %s, methodName = %s, m = %s" (formatCodeLabel lab1) methodName (stringOfRange m)
            System.Diagnostics.Debug.Assert(false, msg)
            warning(InternalError(msg,m));
#endif
        codeLabelToCodeLabel.[lab1] <- lab2

    member cgbuf.SetCodeLabelToPC(lab,pc) = 
#if DEBUG
        if codeLabelToPC.ContainsKey(lab) then 
            let msg = sprintf "two values given for label %s, methodName = %s, m = %s" (formatCodeLabel lab) methodName (stringOfRange m)
            System.Diagnostics.Debug.Assert(false, msg)
            warning(InternalError(msg,m));
#endif
        codeLabelToPC.[lab] <- pc 

    member cgbuf.SetMark (mark1: Mark, mark2: Mark) = 
        cgbuf.SetCodeLabelToCodeLabel(mark1.CodeLabel, mark2.CodeLabel)
        
    member cgbuf.SetMarkToHere (Mark lab) =  
        cgbuf.SetCodeLabelToPC(lab,codebuf.Count)

    member cgbuf.SetStack(s) = 
        stack <- s; 
        nstack <- s.Length

    member cgbuf.Mark(s) = 
        let res = cgbuf.GenerateDelayMark(s)
        cgbuf.SetMarkToHere(res);
        res 

    member cgbuf.mgbuf = mgbuf
    member cgbuf.MethodName = methodName
    member cgbuf.PreallocatedArgCount = alreadyUsedArgs

    member cgbuf.AllocLocal(ranges,ty) = 
        let j = locals.Count
        locals.Add((ranges,ty));
        j 

    member cgbuf.ReallocLocal(cond,ranges,ty) = 
        let j = 
            match ResizeArray.tryFindIndexi cond locals with 
            | Some j -> 
                let (prevRanges,_) = locals.[j]
                locals.[j] <- ((ranges@prevRanges),ty);
                j             
            | None -> 
                cgbuf.AllocLocal(ranges,ty)
        let j = j + alreadyUsedLocals
        j

    member cgbuf.Close() = 
        let instrs = codebuf.ToArray() 
        let instrs = 
            // If we omitted ANY sequence points, then promote the first sequence point to be the first instruction in the
            // method. A bit ugly but .NET debuggers only honour "step into" if the sequence point is the first in the method.
            //
            match seqpoint with 
            | Some(I_seqpoint sp as i) ->
                let i = 
                    if zapFirstSeqPointToStart then 
                        i
                    else
                        // This special dummy sequence point seems to be the magic to indicate that the head of the 
                        // method has no sequence point
                        I_seqpoint (ILSourceMarker.Create(document = sp.Document,
                                                          line = FeeFee mgbuf.cenv,
                                                          column = 0,
                                                          endLine = FeeFee mgbuf.cenv,
                                                          endColumn = 0))

                // Note we use physical equality '==' to compare the instruction objects. Nasty.
                instrs |> Array.mapi (fun idx i2 -> if idx = 0 then i else if i === i2 then AI_nop else i2)
            | _ -> 
                instrs
        ResizeArray.toList locals ,
        maxStack,
        (computeCodeLabelToPC 0),
        instrs,
        ResizeArray.toList exnSpecs,
        isSome seqpoint
"""
