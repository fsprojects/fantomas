#r "../packages/FSharp.Compiler.Service.0.0.57/lib/net45/FSharp.Compiler.Service.dll"

#load "TokenMatcher.fs"
#load "FormatConfig.fs"
#load "SourceParser.fs"
#load "SourceTransformer.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

open Fantomas.TokenMatcher
open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.CodePrinter
open Fantomas.CodeFormatter

let config = { FormatConfig.Default with StrictMode = false }

let test (s : string) = 
    formatSourceString false (s.Replace("\r\n", "\n")) config |> printfn "%A";;

fsi.AddPrinter (fun (p : Microsoft.FSharp.Compiler.Range.pos) -> p.ToString())
fsi.AddPrinter (fun (r : Microsoft.FSharp.Compiler.Range.range) -> r.ToString())

test """
[<DataContract>]
type Foo = 
    { [<field:DataMember>]
      Bar : string }
""";;

test """
open System
open System.Runtime.InteropServices
open Accessibility

[<DllImport("oleacc.dll")>]
extern int AccessibleChildren(
    IAccessible paccContainer, 
    int iChildStart, 
    int cChildren, 
    [<Out()>] [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] System.Object [] rgvarChildren, 
    int* pcObtained)
"""

test """
let handle = 
    if n<weakThreshhold then 
        assert onStrongDiscard.IsNone; // it disappeared
        Weak(WeakReference(v)) 
    else 
        Strong(v)
"""

test """
let newDocument = //somecomment
    { program = Encoding.Default.GetBytes(document.Program) |> Encoding.UTF8.GetString
    content = Encoding.Default.GetBytes(document.Content) |> Encoding.UTF8.GetString
    created = document.Created.ToLocalTime() }
    |> JsonConvert.SerializeObject
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

test """
let f() = 
    // CommentB
    x + x
""" 

// INADEQUATE: inline block comment should not emit new lines
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

// INADEQUATE: block comment not sticky-left
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