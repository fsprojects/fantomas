module Fantomas.Core.Tests.FunctionDefinitionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``recursive functions`` () =
    formatSourceString
        false
        """
    let rec f x = g x
    and g x = x"""
        config
    |> prepend newline
    |> should
        equal
        """
let rec f x = g x
and g x = x
"""

[<Test>]
let ``recursive functions in type definition`` () =
    formatSourceString
        false
        """
type C () =
    let rec g x = h x
    and h x = g x

    member x.P = g 3"""
        config
    |> prepend newline
    |> should
        equal
        """
type C() =
    let rec g x = h x
    and h x = g x

    member x.P = g 3
"""

[<Test>]
let ``should keep mutually recursive functions`` () =
    formatSourceString
        false
        """
let rec createJArray x = createJObject

and createJObject y = createJArray
    """
        config
    |> prepend newline
    |> should
        equal
        """
let rec createJArray x = createJObject

and createJObject y = createJArray
"""

[<Test>]
let ``should keep mutually recursive functions in nested function`` () =
    formatSourceString
        false
        """let f =
    let rec createJArray x = createJObject x

    and createJObject y = createJArray y
    createJArray
    """
        config
    |> should
        equal
        """let f =
    let rec createJArray x = createJObject x

    and createJObject y = createJArray y
    createJArray
"""

[<Test>]
let ``should keep identifiers with whitespace in double backticks`` () =
    formatSourceString
        false
        """let ``should keep identifiers in double backticks``() = x
    """
        config
    |> should
        equal
        """let ``should keep identifiers in double backticks`` () = x
"""

[<Test>]
let ``should not remove backticks from shouldn't identifier`` () =
    formatSourceString
        false
        """let ``shouldn't`` () = x
    """
        config
    |> should
        equal
        """let ``shouldn't`` () = x
"""

[<Test>]
let ``should keep identifiers with + in double backticks`` () =
    formatSourceString
        false
        """let ``Foo+Bar``() = x
    """
        config
    |> should
        equal
        """let ``Foo+Bar`` () = x
"""

[<Test>]
let ``double backticks with non-alphanum character, 776`` () =
    formatSourceString
        false
        """
let ``!foo hoo`` () = ()
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
let ``!foo hoo`` () : unit = ()
let ``@foo hoo`` = ()
let ``$foo hoo`` : unit = ()
    """
        config
    |> prepend newline
    |> should
        equal
        """
let ``!foo hoo`` () = ()
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
let ``!foo hoo`` () : unit = ()
let ``@foo hoo`` = ()
let ``$foo hoo``: unit = ()
"""

[<Test>]
let ``let bindings with return types`` () =
    formatSourceString
        false
        """
       let divide x y =
           let stream : System.IO.FileStream = System.IO.File.Create("test.txt")
           let writer : System.IO.StreamWriter = new System.IO.StreamWriter(stream)
           try
              writer.WriteLine("test1");
              Some( x / y )
           finally
              writer.Flush()
              printfn "Closing stream"
              stream.Close()"""
        config
    |> prepend newline
    |> should
        equal
        """
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
let ``simple subtype constraint`` () =
    formatSourceString
        false
        """
let subtype (xs : seq<'t :> System.IDisposable>) = ()"""
        config
    |> prepend newline
    |> should
        equal
        """
let subtype (xs: seq<'t :> System.IDisposable>) = ()
"""

[<Test>]
let ``type constraints and inline`` () =
    formatSourceString
        false
        """
let inline add(value1 : ^T when ^T : (static member (+) : ^T * ^T -> ^T), value2: ^T) =
    value1 + value2

let inline heterogenousAdd(value1 : ^T when (^T or ^U) : (static member (+) : ^T * ^U -> ^T), value2 : ^U) =
    value1 + value2"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline add (value1: ^T when ^T: (static member (+): ^T * ^T -> ^T), value2: ^T) = value1 + value2

let inline heterogenousAdd (value1: ^T when (^T or ^U): (static member (+): ^T * ^U -> ^T), value2: ^U) =
    value1 + value2
"""

[<Test>]
let ``should keep whitespace after function call`` () =
    formatSourceString
        false
        """let relative = (toRelativePath fileName).TrimStart '.'
    """
        config
    |> should
        equal
        """let relative = (toRelativePath fileName).TrimStart '.'
"""

[<Test>]
let ``should keep type annotations`` () =
    formatSourceString false """let empty<'T> : LazyList<'T> = EmptyValue<'T>.Value""" config
    |> should
        equal
        """let empty<'T> : LazyList<'T> = EmptyValue<'T>.Value
"""

[<Test>]
let ``should add spaces between multiline nested let bindings`` () =
    formatSourceString
        false
        """let f1 =
    let f2 x =
        let _ = ()
        x + 1
    let f3 y =
        let _ = ()
        y + 1
    x + y"""
        config
    |> should
        equal
        """let f1 =
    let f2 x =
        let _ = ()
        x + 1

    let f3 y =
        let _ = ()
        y + 1

    x + y
"""

[<Test>]
let ``should indent fun blocks`` () =
    formatSourceString
        false
        """let f =
    fun x ->
    let y = 1
    x"""
        config
    |> should
        equal
        """let f =
    fun x ->
        let y = 1
        x
"""

[<Test>]
let ``should not add spaces into a series of function application`` () =
    formatSourceString
        false
        """
let f x = "d"
f(1).Contains("3")"""
        config
    |> prepend newline
    |> should
        equal
        """
let f x = "d"
f(1).Contains("3")
"""

[<Test>]
let ``should handle desugared matches correctly`` () =
    formatSourceString
        false
        """
type U = X of int
let f = fun x -> match x with X (x) -> x
"""
        config
    |> prepend newline
    |> should
        equal
        """
type U = X of int

let f =
    fun x ->
        match x with
        | X(x) -> x
"""

[<Test>]
let ``should handle member constraints and generic params correctly`` () =
    formatSourceString
        false
        """
let inline implicit< ^a,^b when ^a : (static member op_Implicit : ^b -> ^a)> arg =
        (^a : (static member op_Implicit : ^b -> ^a) arg)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline implicit< ^a, ^b when ^a: (static member op_Implicit: ^b -> ^a)> arg =
    (^a: (static member op_Implicit: ^b -> ^a) arg)
"""

[<Test>]
let ``don't add spaces for function application inside dot access`` () =
    formatSourceString
        false
        """
let f x = "foo"
f(42).Length
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f x = "foo"
f(42).Length
"""

[<Test>]
let ``do add spaces for function application inside parentheses inside dot access`` () =
    formatSourceString
        false
        """let inputBlah = "So, I was like, Visual Studio did wat"
let someBlahing = (Blah.TryCreate inputBlah).Value"""
        config
    |> prepend newline
    |> should
        equal
        """
let inputBlah = "So, I was like, Visual Studio did wat"
let someBlahing = (Blah.TryCreate inputBlah).Value
"""

[<Test>]
let ``don't create redundant parentheses outside trait calls`` () =
    formatSourceString false """let f (arg : 'T) = (^T : (member Value : string) arg)""" config
    |> prepend newline
    |> should
        equal
        """
let f (arg: 'T) = (^T: (member Value: string) arg)
"""

[<Test>]
let ``lambda with complex type`` () =
    formatSourceString false """let x = fun ((u, v):(int*int)) -> 5""" config
    |> prepend newline
    |> should
        equal
        """
let x = fun ((u, v): (int * int)) -> 5
"""

[<Test>]
let ``respect page-width setting in function signature, 495`` () =
    formatSourceString
        false
        """
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
"""
        { config with
            MaxLineLength = 100
            SpaceBeforeColon = true
            MaxInfixOperatorExpression = 70 }
    |> prepend newline
    |> should
        equal
        """
let fold
    (funcs : ResultFunc<'Input, 'Output, 'TError> seq)
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
    formatSourceString
        false
        """
[<Emit("console.log('%c' +  $1, 'color: ' + $0)")>]
let printInColor (color:string) (msg:string):unit = jsNative
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Emit("console.log('%c' +  $1, 'color: ' + $0)")>]
let printInColor (color: string) (msg: string) : unit = jsNative
"""

[<Test>]
let ``internal keyword included in function signature length check`` () =
    formatSourceString
        false
        """
  let internal UpdateStrongNaming (assembly : AssemblyDefinition) (key : StrongNameKeyPair option) =
    assembly.Name

  let UpdateStrongNamingX (assembly : AssemblyDefinition) (key : StrongNameKeyPair option) =
    assembly.Name
"""
        { config with
            MaxLineLength = 90
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
let internal UpdateStrongNaming
    (assembly : AssemblyDefinition)
    (key : StrongNameKeyPair option)
    =
    assembly.Name

let UpdateStrongNamingX (assembly : AssemblyDefinition) (key : StrongNameKeyPair option) =
    assembly.Name
"""

[<Test>]
let ``long function definition should put equals and body on a newline, 740`` () =
    formatSourceString
        false
        """
module FormatCode =

    let private format filename code config =
        let checker = Fantomas.FakeHelpers.sharedChecker.Force()
        let options = Fantomas.FakeHelpers.createParsingOptionsFromFile filename
        let source = SourceOrigin.SourceString code
        CodeFormatter.FormatDocumentAsync("tmp.fsx", source, config, options, checker)

    [<FunctionName("FormatCode")>]
    let run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest) (log: ILogger) = Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
"""
        config
    |> prepend newline
    |> should
        equal
        """
module FormatCode =

    let private format filename code config =
        let checker = Fantomas.FakeHelpers.sharedChecker.Force()
        let options = Fantomas.FakeHelpers.createParsingOptionsFromFile filename
        let source = SourceOrigin.SourceString code
        CodeFormatter.FormatDocumentAsync("tmp.fsx", source, config, options, checker)

    [<FunctionName("FormatCode")>]
    let run
        ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
        (log: ILogger)
        =
        Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
"""

[<Test>]
let ``long function definition with return type should have multiline signature`` () =
    formatSourceString
        false
        """
module FormatCode =

    let private format filename code config =
        let checker = Fantomas.FakeHelpers.sharedChecker.Force()
        let options = Fantomas.FakeHelpers.createParsingOptionsFromFile filename
        let source = SourceOrigin.SourceString code
        CodeFormatter.FormatDocumentAsync("tmp.fsx", source, config, options, checker)

    [<FunctionName("FormatCode")>]
    let run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest) (log: ILogger) : HttpResponse = Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
"""
        config
    |> prepend newline
    |> should
        equal
        """
module FormatCode =

    let private format filename code config =
        let checker = Fantomas.FakeHelpers.sharedChecker.Force()
        let options = Fantomas.FakeHelpers.createParsingOptionsFromFile filename
        let source = SourceOrigin.SourceString code
        CodeFormatter.FormatDocumentAsync("tmp.fsx", source, config, options, checker)

    [<FunctionName("FormatCode")>]
    let run
        ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
        (log: ILogger)
        : HttpResponse =
        Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
"""

[<Test>]
let ``long function signature, 492`` () =
    formatSourceString
        false
        """
let private addTaskToScheduler (scheduler : IScheduler) taskName taskCron prio (task : unit -> unit) groupName =
        let mutable jobDataMap = JobDataMap()
        jobDataMap.["task"] <- task
        let job =
            JobBuilder.Create<WrapperJob>().UsingJobData(jobDataMap)
                .WithIdentity(taskName, groupName).Build()
        1
"""
        { config with MaxLineLength = 100 }
    |> prepend newline
    |> should
        equal
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    let mutable jobDataMap = JobDataMap()
    jobDataMap.["task"] <- task

    let job =
        JobBuilder
            .Create<WrapperJob>()
            .UsingJobData(jobDataMap)
            .WithIdentity(taskName, groupName)
            .Build()

    1
"""

[<Test>]
let ``long function signature should align with equal sign, 883`` () =
    formatSourceString
        false
        """let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) : ReadModel<'Event, 'State> =
    ()
"""
        { config with
            IndentSize = 2
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
let readModel
  (updateState : 'State -> EventEnvelope<'Event> list -> 'State)
  (initState : 'State)
  : ReadModel<'Event, 'State> =
  ()
"""

[<Test>]
let ``long function signature should align with equal sign, no return type`` () =
    formatSourceString
        false
        """let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) =
    ()
"""
        { config with
            IndentSize = 2
            SpaceBeforeColon = true
            MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let readModel
  (updateState : 'State -> EventEnvelope<'Event> list -> 'State)
  (initState : 'State)
  =
  ()
"""

[<Test>]
let ``long function signature with single tuple parameter and no return type`` () =
    formatSourceString
        false
        """
let fold (funcs: ResultFunc<'Input, 'Output, 'TError> seq, input: 'Input, input2: 'Input, input3: 'Input) =
    ()
"""
        { config with MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let fold
    (
        funcs: ResultFunc<'Input, 'Output, 'TError> seq,
        input: 'Input,
        input2: 'Input,
        input3: 'Input
    ) =
    ()
"""

[<Test>]
let ``long function signature with single tuple parameter and return type`` () =
    formatSourceString
        false
        """
let fold (funcs: ResultFunc<'Input, 'Output, 'TError> seq, input: 'Input, input2: 'Input, input3: 'Input) : Result<'Output list, 'TError list> =
    ()
"""
        { config with MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let fold
    (
        funcs: ResultFunc<'Input, 'Output, 'TError> seq,
        input: 'Input,
        input2: 'Input,
        input3: 'Input
    ) : Result<'Output list, 'TError list> =
    ()
"""

[<Test>]
let ``align long function signature to indentation without return type `` () =
    formatSourceString
        false
        """
let fold (funcs: ResultFunc<'Input, 'Output, 'TError> seq) (input: 'Input) (input2: 'Input) (input3: 'Input) = ()
"""
        { config with
            MaxLineLength = 60
            AlignFunctionSignatureToIndentation = true }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) : ReadModel<'Event, 'State> =
    ()
"""
        { config with
            IndentSize = 2
            SpaceBeforeColon = true
            AlignFunctionSignatureToIndentation = true }
    |> prepend newline
    |> should
        equal
        """
let readModel
  (updateState : 'State -> EventEnvelope<'Event> list -> 'State)
  (initState : 'State)
  : ReadModel<'Event, 'State>
  =
  ()
"""

[<Test>]
let ``align long function signature to indentation that are recursive`` () =
    formatSourceString
        false
        """
let rec run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest) (log: ILogger) : HttpResponse =
        logAnalyticsForRequest log req
        Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req

and logAnalyticsForRequest (log:ILogger) (httpRequest: HttpRequest) =
    log.Info (sprintf "Meh: %A" httpRequest)
"""
        { config with
            MaxLineLength = 60
            AlignFunctionSignatureToIndentation = true }
    |> prepend newline
    |> should
        equal
        """
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

[<Test>]
let ``typeof generic static constraint, 803`` () =
    formatSourceString
        false
        """
let inline test< ^foo> (foo: ^foo) =
    let bar = typeof< ^foo>
    bar.Name
"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline test< ^foo> (foo: ^foo) =
    let bar = typeof< ^foo>
    bar.Name
"""

[<Test>]
let ``space before ^ SRTP type is required in function call, 984`` () =
    formatSourceString
        false
        """
let inline deserialize< ^a when ^a: (static member FromJson: ^a -> Json< ^a >)> json =
    json |> Json.parse |> Json.deserialize< ^a>
"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline deserialize< ^a when ^a: (static member FromJson: ^a -> Json< ^a >)> json =
    json |> Json.parse |> Json.deserialize< ^a>
"""

[<Test>]
let ``SRTP or condition with non-generic type, 2168`` () =
    formatSourceString
        false
        """
let inline deserialize< ^a when ( ^a or FromJsonDefaults) : (static member FromJson :  ^a -> Json< ^a>)> json =
    json |> Json.parse |> Json.deserialize< ^a>
"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline deserialize< ^a when (^a or FromJsonDefaults): (static member FromJson: ^a -> Json< ^a >)> json =
    json |> Json.parse |> Json.deserialize< ^a>
"""

[<Test>]
let ``equals sign between hash directives, 1218`` () =
    formatSourceString
        false
        """
module Infrastructure =

    let internal ReportMessage
        (message: string)
#if DEBUG
        (_: ErrorLevel)
#else
        (errorLevel: ErrorLevel)
#endif
        =
#if DEBUG
        failwith message
#else
        let sentryEvent = SentryEvent (SentryMessage message, Level = errorLevel)
        ()
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Infrastructure =

    let internal ReportMessage
        (message: string)
#if DEBUG
        (_: ErrorLevel)
#else
        (errorLevel: ErrorLevel)
#endif
        =
#if DEBUG
        failwith message
#else
        let sentryEvent = SentryEvent(SentryMessage message, Level = errorLevel)
        ()
#endif
"""

[<Test>]
let ``single line value without return type `` () =
    formatSourceString
        false
        """
let a =  7
let private b  = ""
let internal c  = 8
let [<Foo>] d  = 9
let e<'t>  = 8
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 7
let private b = ""
let internal c = 8

[<Foo>]
let d = 9

let e<'t> = 8
"""

[<Test>]
let ``single line value with return type `` () =
    formatSourceString
        false
        """
let a : int =  7
let private b : string = ""
let internal c : int = 8
let [<Foo>] d : int = 9
let e<'t> : int = 8
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a: int = 7
let private b: string = ""
let internal c: int = 8

[<Foo>]
let d: int = 9

let e<'t> : int = 8
"""

[<Test>]
let ``multiline value without return type `` () =
    formatSourceString
        false
        """
let a =
    // a comment makes things multiline
    7
let private b =
    // a comment makes things multiline
    ""
let internal c =
    // a comment makes things multiline
    8
let [<Foo>] d =
    // a comment makes things multiline
    9
let e<'t> =
    // a comment makes things multiline
    8
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    // a comment makes things multiline
    7

let private b =
    // a comment makes things multiline
    ""

let internal c =
    // a comment makes things multiline
    8

[<Foo>]
let d =
    // a comment makes things multiline
    9

let e<'t> =
    // a comment makes things multiline
    8
"""

[<Test>]
let ``multiline value with return type `` () =
    formatSourceString
        false
        """
let a : int =
    // a comment makes things multiline
    7
let private b : string =
    // a comment makes things multiline
    ""
let internal c : int =
    // a comment makes things multiline
    8
let [<Foo>] d : int =
    // a comment makes things multiline
    9
let e<'t> : int =
    // a comment makes things multiline
    8
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a: int =
    // a comment makes things multiline
    7

let private b: string =
    // a comment makes things multiline
    ""

let internal c: int =
    // a comment makes things multiline
    8

[<Foo>]
let d: int =
    // a comment makes things multiline
    9

let e<'t> : int =
    // a comment makes things multiline
    8
"""

[<Test>]
let ``short function binding name without return type`` () =
    formatSourceString
        false
        """
let add a b = a + b
let subtract (a: int) (b:int) = a - b
let private multiply a b = a * b
let internal divide a b = a / b
let SetQuartzLogger l = LogProvider.SetCurrentLogProvider(l)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let add a b = a + b
let subtract (a: int) (b: int) = a - b
let private multiply a b = a * b
let internal divide a b = a / b
let SetQuartzLogger l = LogProvider.SetCurrentLogProvider(l)
"""

[<Test>]
let ``long function definition without return type, 1307`` () =
    formatSourceString
        false
        """
module M =
    let LongFunctionWithLotsOfParameters
        (aVeryLongParam: AVeryLongTypeThatYouNeedToUse)
        (aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse)
        (aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse)
        =
        // ... the body of the method follows
        ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
module M =
    let LongFunctionWithLotsOfParameters
        (aVeryLongParam: AVeryLongTypeThatYouNeedToUse)
        (aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse)
        (aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse)
        =
        // ... the body of the method follows
        ()
"""

[<Test>]
let ``long function definition with tuple and without return type`` () =
    formatSourceString
        false
        """
let longFunctionWithLongTupleParameter
    (aVeryLongParam: AVeryLongTypeThatYouNeedToUse,
     aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse,
     aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse)
    =
    // ... the body of the method follows
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let longFunctionWithLongTupleParameter
    (
        aVeryLongParam: AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse
    ) =
    // ... the body of the method follows
    ()
"""

[<Test>]
let ``long function definition with tuple and without return type, AlignFunctionSignatureToIndentation`` () =
    formatSourceString
        false
        """
let longFunctionWithLongTupleParameter
    (aVeryLongParam: AVeryLongTypeThatYouNeedToUse,
     aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse,
     aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse)
    =
    // ... the body of the method follows
    ()
"""
        { config with
            AlignFunctionSignatureToIndentation = true
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
let longFunctionWithLongTupleParameter
    (
        aVeryLongParam : AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongParam : AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongParam : AVeryLongTypeThatYouNeedToUse
    )
    =
    // ... the body of the method follows
    ()
"""

[<Test>]
let ``long function definition with tuple without types and without return type`` () =
    formatSourceString
        false
        """
let longFunctionWithLongTupleParameter
    (aVeryLongParam,
     aSecondVeryLongParam,
     aThirdVeryLongParam,
     aFourthVeryLongParam)
    =
    // ... the body of the method follows
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let longFunctionWithLongTupleParameter
    (
        aVeryLongParam,
        aSecondVeryLongParam,
        aThirdVeryLongParam,
        aFourthVeryLongParam
    ) =
    // ... the body of the method follows
    ()
"""

[<Test>]
let ``long function definition with return type`` () =
    formatSourceString
        false
        """
    let longFunctionWithLotsOfParametersAndReturnType (aVeryLongParam: AVeryLongTypeThatYouNeedToUse)
                                                      (aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse)
                                                      (aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse)
                                                      : ReturnType =
        // ... the body of the method follows
        ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let longFunctionWithLotsOfParametersAndReturnType
    (aVeryLongParam: AVeryLongTypeThatYouNeedToUse)
    (aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse)
    (aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse)
    : ReturnType =
    // ... the body of the method follows
    ()
"""

[<Test>]
let ``long function definition with tuple parameter and return type`` () =
    formatSourceString
        false
        """
let longFunctionWithLongTupleParameterAndReturnType (aVeryLongParam: AVeryLongTypeThatYouNeedToUse,
                                                     aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse,
                                                     aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse) : ReturnType =
        // ... the body of the method follows
        ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let longFunctionWithLongTupleParameterAndReturnType
    (
        aVeryLongParam: AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse
    ) : ReturnType =
    // ... the body of the method follows
    ()
"""

[<Test>]
let ``long function definition with tuple parameter and return type, AlignFunctionSignatureToIndentation`` () =
    formatSourceString
        false
        """
let longFunctionWithLongTupleParameterAndReturnType (aVeryLongParam: AVeryLongTypeThatYouNeedToUse,
                                                     aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse,
                                                     aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse) : ReturnType =
        // ... the body of the method follows
        ()
"""
        { config with
            AlignFunctionSignatureToIndentation = true }
    |> prepend newline
    |> should
        equal
        """
let longFunctionWithLongTupleParameterAndReturnType
    (
        aVeryLongParam: AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse
    )
    : ReturnType
    =
    // ... the body of the method follows
    ()
"""

[<Test>]
let ``space before parameter inside inner let binding, 1345`` () =
    formatSourceString
        false
        """
let func1 (l: int) = ()

type Test () =
    member this.Testing () =
        let func2 (l: int) = ()
        return 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
let func1 (l: int) = ()

type Test() =
    member this.Testing() =
        let func2 (l: int) = ()
        return 0
"""

[<Test>]
let ``function argument with parenthesis, 1470`` () =
    formatSourceString
        false
        """
let bazka (f: ((FooTypeX -> string * string list)) Bar) = failwith ""
"""
        config
    |> prepend newline
    |> should
        equal
        """
let bazka (f: ((FooTypeX -> string * string list)) Bar) = failwith ""
"""

[<Test>]
let ``value with tuple return type, 2403`` () =
    formatSourceString
        false
        """
let a : int * string = 1, ""
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a: int * string = 1, ""
"""

[<Test>]
let ``value with tuple return type, three types`` () =
    formatSourceString
        false
        """
let a : int * string * bool = 1, "", false
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a: int * string * bool = 1, "", false
"""

[<Test>]
let ``should preserve quotes around type parameters, 2875`` () =
    formatSourceString
        false
        """
let repro (a: '``QuotedWithIllegalChar<'T>``) = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let repro (a: '``QuotedWithIllegalChar<'T>``) = ()
"""

[<Test>]
let ``should preserve quotes around statically resolved type parameters`` () =
    formatSourceString
        false
        """
let inline repro (a: ^``QuotedWithIllegalChar<'T>``) = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline repro (a: ^``QuotedWithIllegalChar<'T>``) = ()
"""

[<Test>]
let ``should idempotently format large member constraints on type parameters, 2896`` () =
    let formatted =
        formatSourceString
            false
            """
let inline func
    (arg:
        'a when 'a: (member a: int) and 'a: (member b: int) and 'a: (member c: int) and 'a: (member d: int) and 'a: (member e: int))
    = 0
        """
            config
        |> prepend newline

    let expected =
        """
let inline func
    (arg:
        'a
            when 'a: (member a: int)
            and 'a: (member b: int)
            and 'a: (member c: int)
            and 'a: (member d: int)
            and 'a: (member e: int))
    =
    0
"""

    formatted |> should equal expected

    let formattedTwice = formatSourceString false formatted config |> prepend newline

    formattedTwice |> should equal expected
