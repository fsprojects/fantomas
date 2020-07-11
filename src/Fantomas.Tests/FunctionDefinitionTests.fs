module Fantomas.Tests.FunctionDefinitionTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``recursive functions``() =
    formatSourceString false """
    let rec f x = g x
    and g x = x""" config
    |> prepend newline
    |> should equal """
let rec f x = g x

and g x = x
"""

[<Test>]
let ``recursive functions in type definition``() =
    formatSourceString false """
type C () = 
    let rec g x = h x
    and h x = g x

    member x.P = g 3""" config
    |> prepend newline
    |> should equal """
type C() =
    let rec g x = h x
    and h x = g x

    member x.P = g 3
"""

[<Test>]
let ``should keep mutually recursive functions``() =
    formatSourceString false """
let rec createJArray x = createJObject

and createJObject y = createJArray
    """ config
    |> should equal """let rec createJArray x = createJObject

and createJObject y = createJArray
"""

[<Test>]
let ``should keep mutually recursive functions in nested function``() =
    formatSourceString false """let f =
    let rec createJArray x = createJObject x

    and createJObject y = createJArray y
    createJArray
    """ config
    |> should equal """let f =
    let rec createJArray x = createJObject x

    and createJObject y = createJArray y
    createJArray
"""

[<Test>]
let ``should keep identifiers with whitespace in double backticks``() =
    formatSourceString false """let ``should keep identifiers in double backticks``() = x
    """ config
    |> should equal """let ``should keep identifiers in double backticks`` () = x
"""

[<Test>]
let ``should remove backticks from shouldn't identifier``() =
    formatSourceString false """let ``shouldn't`` () = x
    """ config
    |> should equal """let shouldn't () = x
"""

[<Test>]
let ``should keep identifiers with + in double backticks``() =
    formatSourceString false """let ``Foo+Bar``() = x
    """ config
    |> should equal """let ``Foo+Bar`` () = x
"""

[<Test>]
let ``double backticks with non-alphanum character, 776``() =
    formatSourceString false """let ``!foo hoo`` () = ()
let ``@foo hoo`` () = ()
let ``$foo hoo`` () = ()
let ``%foo hoo`` () = ()
let ``^foo hoo`` () = ()
let ``&foo hoo`` () = ()
let ``*foo hoo`` () = ()
let ``<foo hoo`` () = ()
let ``>foo hoo`` () = ()
let ``=foo hoo`` () = ()
let ``-foo hoo`` () = ()
    """ config
    |> should equal """let ``!foo hoo`` () = ()
let ``@foo hoo`` () = ()
let ``$foo hoo`` () = ()
let ``%foo hoo`` () = ()
let ``^foo hoo`` () = ()
let ``&foo hoo`` () = ()
let ``*foo hoo`` () = ()
let ``<foo hoo`` () = ()
let ``>foo hoo`` () = ()
let ``=foo hoo`` () = ()
let ``-foo hoo`` () = ()
"""

[<Test>]
let ``let bindings with return types``() =
    formatSourceString false """
       let divide x y =
           let stream : System.IO.FileStream = System.IO.File.Create("test.txt")
           let writer : System.IO.StreamWriter = new System.IO.StreamWriter(stream)
           try
              writer.WriteLine("test1");
              Some( x / y )
           finally
              writer.Flush()
              printfn "Closing stream"
              stream.Close()""" config
    |> prepend newline
    |> should equal """
let divide x y =
    let stream: System.IO.FileStream = System.IO.File.Create("test.txt")
    let writer: System.IO.StreamWriter = new System.IO.StreamWriter(stream)
    try
        writer.WriteLine("test1")
        Some(x / y)
    finally
        writer.Flush()
        printfn "Closing stream"
        stream.Close()
"""

[<Test>]
let ``simple subtype constraint``() =
    formatSourceString false """
let subtype (xs : seq<'t :> System.IDisposable>) = ()""" config
    |> prepend newline
    |> should equal """
let subtype (xs: seq<'t :> System.IDisposable>) = ()
"""

[<Test>]
let ``type constraints and inline``() =
    formatSourceString false """
let inline add(value1 : ^T when ^T : (static member (+) : ^T * ^T -> ^T), value2: ^T) =
    value1 + value2

let inline heterogenousAdd(value1 : ^T when (^T or ^U) : (static member (+) : ^T * ^U -> ^T), value2 : ^U) =
    value1 + value2""" config
    |> prepend newline
    |> should equal """
let inline add (value1: ^T when ^T: (static member (+): ^T * ^T -> ^T), value2: ^T) = value1 + value2

let inline heterogenousAdd (value1: ^T when (^T or ^U): (static member (+): ^T * ^U -> ^T), value2: ^U) =
    value1 + value2
"""

[<Test>]
let ``should keep whitespace after function call``() =
    formatSourceString false """let relative = (toRelativePath fileName).TrimStart '.'
    """ config
    |> should equal """let relative = (toRelativePath fileName).TrimStart '.'
"""

[<Test>]
let ``should keep type annotations``() =
    formatSourceString false """let empty<'T> : LazyList<'T> = EmptyValue<'T>.Value""" config
    |> should equal """let empty<'T> : LazyList<'T> = EmptyValue<'T>.Value
"""

[<Test>]
let ``should add spaces between multiline nested let bindings``() =
    formatSourceString false """let f1 = 
    let f2 x = 
        let _ = ()
        x + 1
    let f3 y = 
        let _ = ()
        y + 1
    x + y""" config
    |> should equal """let f1 =
    let f2 x =
        let _ = ()
        x + 1

    let f3 y =
        let _ = ()
        y + 1

    x + y
"""

[<Test>]
let ``should indent fun blocks``() =
    formatSourceString false """let f =
    fun x ->
    let y = 1
    x""" config
    |> should equal """let f =
    fun x ->
        let y = 1
        x
"""
[<Test>]
let ``should not add spaces into a series of function application``() =
    formatSourceString false """let f x = "d"
f(1).Contains("3")""" config
    |> should equal """let f x = "d"
f(1).Contains("3")
"""

[<Test>]
let ``should handle external functions``() =
    formatSourceString false """[<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
extern ReturnCode  GetParent (System.IntPtr inRef, byref outParentRef)""" config
    |> prepend newline
    |> should equal """
[<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
extern ReturnCode GetParent(System.IntPtr inRef, byref outParentRef)
"""

[<Test>]
let ``should handle simple external functions``() =
    formatSourceString false """module InteropWithNative =
        [<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
        extern IntPtr setCallbridgeSupportTarget(IntPtr newTarget)""" config
    |> prepend newline
    |> should equal """
module InteropWithNative =
    [<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr setCallbridgeSupportTarget(IntPtr newTarget)
"""

[<Test>]
let ``should handle external functions with void return type``() =
    formatSourceString false """module InteropWithNative =
        [<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
        extern void setCallbridgeSupportTarget(IntPtr newTarget)""" config
    |> prepend newline
    |> should equal """
module InteropWithNative =
    [<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
    extern void setCallbridgeSupportTarget(IntPtr newTarget)
"""

[<Test>]
let ``should handle external functions with fully-qualified attributes``() =
    formatSourceString false """[<System.Runtime.InteropServices.DllImport("user32.dll")>]
extern int GetWindowLong(System.IntPtr hwnd, int index)""" config
    |> prepend newline
    |> should equal """
[<System.Runtime.InteropServices.DllImport("user32.dll")>]
extern int GetWindowLong(System.IntPtr hwnd, int index)
"""

[<Test>]
let ``should handle external functions with special types``() =
    formatSourceString false """open System
open System.Runtime.InteropServices
open Accessibility

[<DllImport("oleacc.dll")>]
extern int AccessibleChildren(
    IAccessible paccContainer, 
    int iChildStart, 
    int cChildren, 
    [<Out()>] [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] System.Object [] rgvarChildren,
    int* pcObtained)""" config
    |> prepend newline
    |> should equal """
open System
open System.Runtime.InteropServices
open Accessibility

[<DllImport("oleacc.dll")>]
extern int AccessibleChildren(IAccessible paccContainer, int iChildStart, int cChildren, [<Out; MarshalAs(UnmanagedType.LPArray,
                                                                                                          SizeParamIndex =
                                                                                                              4s)>] System.Object [] rgvarChildren, int* pcObtained)
"""

[<Test>]
let ``DllImport and Marshall return type, 574``() =
    formatSourceString false """[<DllImport("userenv.dll", SetLastError = true)>]
[<MarshalAs(UnmanagedType.Bool)>]
extern bool DestroyEnvironmentBlock(IntPtr lpEnvironment)""" config
    |> should equal """[<DllImport("userenv.dll", SetLastError = true)>]
[<MarshalAs(UnmanagedType.Bool)>]
extern bool DestroyEnvironmentBlock(IntPtr lpEnvironment)
"""

[<Test>]
let ``should handle desugared matches correctly``() =
    formatSourceString false """
type U = X of int
let f = fun x -> match x with X (x) -> x
"""  config
    |> prepend newline
    |> should equal """
type U = X of int

let f =
    fun x ->
        match x with
        | X (x) -> x
"""

[<Test>]
let ``should handle member constraints and generic params correctly``() =
    formatSourceString false """
let inline implicit< ^a,^b when ^a : (static member op_Implicit : ^b -> ^a)> arg =
        (^a : (static member op_Implicit : ^b -> ^a) arg)
"""  config
    |> prepend newline
    |> should equal """
let inline implicit< ^a, ^b when ^a: (static member op_Implicit: ^b -> ^a)> arg =
    (^a: (static member op_Implicit: ^b -> ^a) arg)
"""

[<Test>]
let ``don't add spaces for function application inside dot access``() =
    formatSourceString false """
let f x = "foo"
f(42).Length
"""  config
    |> prepend newline
    |> should equal """
let f x = "foo"
f(42).Length
"""

[<Test>]
let ``do add spaces for function application inside parentheses inside dot access``() =
    formatSourceString false """let inputBlah = "So, I was like, Visual Studio did wat"
let someBlahing = (Blah.TryCreate inputBlah).Value"""  config
    |> prepend newline
    |> should equal """
let inputBlah = "So, I was like, Visual Studio did wat"
let someBlahing = (Blah.TryCreate inputBlah).Value
"""

[<Test>]
let ``don't create redundant parentheses outside trait calls``() =
    formatSourceString false """let f (arg : 'T) = (^T : (member Value : string) arg)"""  config
    |> prepend newline
    |> should equal """
let f (arg: 'T) = (^T: (member Value: string) arg)
"""

[<Test>]
let ``lambda with complex type``() =
    formatSourceString false """let x = fun ((u, v):(int*int)) -> 5"""  config
    |> prepend newline
    |> should equal """
let x = fun ((u, v): (int * int)) -> 5
"""

[<Test>]
let ``respect page-width setting in function signature, 495`` () =
    formatSourceString false """
let fold (funcs : ResultFunc<'Input, 'Output, 'TError> seq) (input : 'Input) : Result<'Output list, 'TError list> =
    let mutable anyErrors = false
    let mutable collectedOutputs = []
    let mutable collectedErrors = []

    let runValidator (validator : ResultFunc<'Input, 'Output, 'TError>) input =
        let validatorResult = validator input
        match validatorResult with
        | Error error ->
            anyErrors <- true
            collectedErrors <- error :: collectedErrors
        | Ok output -> collectedOutputs <- output :: collectedOutputs
    funcs |> Seq.iter (fun validator -> runValidator validator input)
    match anyErrors with
    | true -> Error collectedErrors
    | false -> Ok collectedOutputs
"""  ({ config with
            MaxLineLength = 100
            SpaceBeforeColon = true
            MaxInfixOperatorExpression = 70 })
    |> prepend newline
    |> should equal """
let fold (funcs : ResultFunc<'Input, 'Output, 'TError> seq)
         (input : 'Input)
         : Result<'Output list, 'TError list> =
    let mutable anyErrors = false
    let mutable collectedOutputs = []
    let mutable collectedErrors = []

    let runValidator (validator : ResultFunc<'Input, 'Output, 'TError>) input =
        let validatorResult = validator input
        match validatorResult with
        | Error error ->
            anyErrors <- true
            collectedErrors <- error :: collectedErrors
        | Ok output -> collectedOutputs <- output :: collectedOutputs

    funcs |> Seq.iter (fun validator -> runValidator validator input)
    match anyErrors with
    | true -> Error collectedErrors
    | false -> Ok collectedOutputs
"""

[<Test>]
let ``attributes above function signature should not force parameters on new line`` () =
    formatSourceString false """
[<Emit("console.log('%c' +  $1, 'color: ' + $0)")>]
let printInColor (color:string) (msg:string):unit = jsNative
"""  config
    |> prepend newline
    |> should equal """
[<Emit("console.log('%c' +  $1, 'color: ' + $0)")>]
let printInColor (color: string) (msg: string): unit = jsNative
"""

[<Test>]
let ``internal keyword included in function signature length check`` () =
    formatSourceString false """
  let internal UpdateStrongNaming (assembly : AssemblyDefinition) (key : StrongNameKeyPair option) =
    assembly.Name

  let UpdateStrongNamingX (assembly : AssemblyDefinition) (key : StrongNameKeyPair option) =
    assembly.Name
"""  ({ config with MaxLineLength = 90; SpaceBeforeColon = true })
    |> prepend newline
    |> should equal """
let internal UpdateStrongNaming (assembly : AssemblyDefinition)
                                (key : StrongNameKeyPair option)
                                =
    assembly.Name

let UpdateStrongNamingX (assembly : AssemblyDefinition) (key : StrongNameKeyPair option) =
    assembly.Name
"""

[<Test>]
let ``long function definition should put equals and body on a newline, 740`` () =
    formatSourceString false """
module FormatCode =

    let private format filename code config =
        let checker = Fantomas.FakeHelpers.sharedChecker.Force()
        let options = Fantomas.FakeHelpers.createParsingOptionsFromFile filename
        let source = SourceOrigin.SourceString code
        CodeFormatter.FormatDocumentAsync("tmp.fsx", source, config, options, checker)

    [<FunctionName("FormatCode")>]
    let run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest) (log: ILogger) = Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
"""  config
    |> prepend newline
    |> should equal """
module FormatCode =

    let private format filename code config =
        let checker =
            Fantomas.FakeHelpers.sharedChecker.Force()

        let options =
            Fantomas.FakeHelpers.createParsingOptionsFromFile filename

        let source = SourceOrigin.SourceString code
        CodeFormatter.FormatDocumentAsync("tmp.fsx", source, config, options, checker)

    [<FunctionName("FormatCode")>]
    let run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
            (log: ILogger)
            =
        Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
"""

[<Test>]
let ``long function definition with return type should have multiline signature`` () =
    formatSourceString false """
module FormatCode =

    let private format filename code config =
        let checker = Fantomas.FakeHelpers.sharedChecker.Force()
        let options = Fantomas.FakeHelpers.createParsingOptionsFromFile filename
        let source = SourceOrigin.SourceString code
        CodeFormatter.FormatDocumentAsync("tmp.fsx", source, config, options, checker)

    [<FunctionName("FormatCode")>]
    let run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest) (log: ILogger) : HttpResponse = Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
"""  config
    |> prepend newline
    |> should equal """
module FormatCode =

    let private format filename code config =
        let checker =
            Fantomas.FakeHelpers.sharedChecker.Force()

        let options =
            Fantomas.FakeHelpers.createParsingOptionsFromFile filename

        let source = SourceOrigin.SourceString code
        CodeFormatter.FormatDocumentAsync("tmp.fsx", source, config, options, checker)

    [<FunctionName("FormatCode")>]
    let run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
            (log: ILogger)
            : HttpResponse =
        Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
"""

[<Test>]
let ``long function signature, 492`` () =
    formatSourceString false """
let private addTaskToScheduler (scheduler : IScheduler) taskName taskCron prio (task : unit -> unit) groupName =
        let mutable jobDataMap = JobDataMap()
        jobDataMap.["task"] <- task
        let job =
            JobBuilder.Create<WrapperJob>().UsingJobData(jobDataMap)
                .WithIdentity(taskName, groupName).Build()
        1
"""  ({ config with MaxLineLength = 100 })
    |> prepend newline
    |> should equal """
let private addTaskToScheduler (scheduler: IScheduler)
                               taskName
                               taskCron
                               prio
                               (task: unit -> unit)
                               groupName
                               =
    let mutable jobDataMap = JobDataMap()
    jobDataMap.["task"] <- task

    let job =
        JobBuilder.Create<WrapperJob>().UsingJobData(jobDataMap).WithIdentity(taskName, groupName)
            .Build()

    1
"""

[<Test>]
let ``long function signature should align with equal sign, 883`` () =
    formatSourceString false """let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) : ReadModel<'Event, 'State> =
    ()
"""  { config with IndentSize = 2; SpaceBeforeColon = true }
    |> prepend newline
    |> should equal """
let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State)
              (initState : 'State)
              : ReadModel<'Event, 'State> =
  ()
"""

[<Test>]
let ``long function signature should align with equal sign, no return type`` () =
    formatSourceString false """let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) =
    ()
"""  { config with IndentSize = 2; SpaceBeforeColon = true; MaxLineLength = 80 }
    |> prepend newline
    |> should equal """
let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State)
              (initState : 'State)
              =
  ()
"""

[<Test>]
let ``long function signature with single tuple parameter and no return type`` () =
    formatSourceString false """
let fold (funcs: ResultFunc<'Input, 'Output, 'TError> seq, input: 'Input, input2: 'Input, input3: 'Input) =
    ()
"""  { config with MaxLineLength = 90 }
    |> prepend newline
    |> should equal """
let fold (funcs: ResultFunc<'Input, 'Output, 'TError> seq,
          input: 'Input,
          input2: 'Input,
          input3: 'Input) =
    ()
"""

[<Test>]
let ``long function signature with single tuple parameter and return type`` () =
    formatSourceString false """
let fold (funcs: ResultFunc<'Input, 'Output, 'TError> seq, input: 'Input, input2: 'Input, input3: 'Input) : Result<'Output list, 'TError list> =
    ()
"""  { config with MaxLineLength = 90 }
    |> prepend newline
    |> should equal """
let fold (funcs: ResultFunc<'Input, 'Output, 'TError> seq,
          input: 'Input,
          input2: 'Input,
          input3: 'Input)
         : Result<'Output list, 'TError list> =
    ()
"""

[<Test>]
let ``align long function signature to indentation without return type `` () =
    formatSourceString false """
let fold (funcs: ResultFunc<'Input, 'Output, 'TError> seq) (input: 'Input) (input2: 'Input) (input3: 'Input) = ()
"""  { config with MaxLineLength = 60; AlignFunctionSignatureToIndentation  = true }
    |> prepend newline
    |> should equal """
let fold
    (funcs: ResultFunc<'Input, 'Output, 'TError> seq)
    (input: 'Input)
    (input2: 'Input)
    (input3: 'Input)
    =
    ()
"""

[<Test>]
let ``align long function signature to indentation with return type`` () =
    formatSourceString false """let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) : ReadModel<'Event, 'State> =
    ()
"""  { config with IndentSize = 2; SpaceBeforeColon = true; AlignFunctionSignatureToIndentation = true }
    |> prepend newline
    |> should equal """
let readModel
  (updateState : 'State -> EventEnvelope<'Event> list -> 'State)
  (initState : 'State)
  : ReadModel<'Event, 'State>
  =
  ()
"""

[<Test>]
let ``align long function signature to indentation that are recursive`` () =
    formatSourceString false """
let rec run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest) (log: ILogger) : HttpResponse =
        logAnalyticsForRequest log req
        Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req

and logAnalyticsForRequest (log:ILogger) (httpRequest: HttpRequest) =
    log.Info (sprintf "Meh: %A" httpRequest)
"""  { config with MaxLineLength = 60; AlignFunctionSignatureToIndentation = true }
    |> prepend newline
    |> should equal """
let rec run
    ([<HttpTrigger(AuthorizationLevel.Anonymous,
                   "get",
                   "post",
                   Route = "{*any}")>] req: HttpRequest)
    (log: ILogger)
    : HttpResponse
    =
    logAnalyticsForRequest log req
    Http.main
        CodeFormatter.GetVersion
        format
        FormatConfig.FormatConfig.Default
        log
        req

and logAnalyticsForRequest
    (log: ILogger)
    (httpRequest: HttpRequest)
    =
    log.Info(sprintf "Meh: %A" httpRequest)
"""
