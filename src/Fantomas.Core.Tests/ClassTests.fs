module Fantomas.Core.Tests.ClassTests

open NUnit.Framework
open FsUnit

open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``class signatures`` () =
    formatSourceString
        true
        """
module Heap

type Heap<'T when 'T : comparison> =
    class
    new : capacity:int -> Heap<'T>
    member Clear : unit -> unit
    member ExtractMin : unit -> 'T
    member Insert : k: 'T -> unit
    member IsEmpty : unit -> bool
    member PeekMin : unit -> 'T
    override ToString : unit -> string
    member Count : int
    end"""
        config
    |> prepend newline
    |> should
        equal
        """
module Heap

type Heap<'T when 'T: comparison> =
    class
        new: capacity: int -> Heap<'T>
        member Clear: unit -> unit
        member ExtractMin: unit -> 'T
        member Insert: k: 'T -> unit
        member IsEmpty: unit -> bool
        member PeekMin: unit -> 'T
        override ToString: unit -> string
        member Count: int
    end
"""

[<Test>]
let ``type constraints complex`` () =
    formatSourceString
        false
        """
type Class4<'T when 'T : (static member staticMethod1 : unit -> 'T) > =
    class end

type Class5<'T when 'T : (member Method1 : 'T -> int)> =
    class end

type Class6<'T when 'T : (member Property1 : int)> =
    class end

type Class7<'T when 'T : (new : unit -> 'T)>() =
   member val Field = new 'T()
    """
        config
    |> prepend newline
    |> should
        equal
        """
type Class4<'T when 'T: (static member staticMethod1: unit -> 'T)> =
    class
    end

type Class5<'T when 'T: (member Method1: 'T -> int)> =
    class
    end

type Class6<'T when 'T: (member Property1: int)> =
    class
    end

type Class7<'T when 'T: (new: unit -> 'T)>() =
    member val Field = new 'T()
"""

[<Test>]
let ``abstract classes`` () =
    formatSourceString
        false
        """
[<AbstractClass>]
type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.CenterX with get() = x and set xval = x <- xval
    member this.CenterY with get() = y and set yval = y <- yval

    abstract Area : float with get
    abstract Perimeter : float  with get
    abstract Name : string with get

    member this.Move dx dy =
       x <- x + dx
       y <- y + dy

    abstract member Rotate: float -> unit
    default this.Rotate(angle) = rotAngle <- rotAngle + angle
    """
        { config with
            MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
[<AbstractClass>]
type Shape2D(x0: float, y0: float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.CenterX
        with get () = x
        and set xval = x <- xval

    member this.CenterY
        with get () = y
        and set yval = y <- yval

    abstract Area: float with get
    abstract Perimeter: float with get
    abstract Name: string with get

    member this.Move dx dy =
        x <- x + dx
        y <- y + dy

    abstract member Rotate: float -> unit
    default this.Rotate(angle) = rotAngle <- rotAngle + angle
"""

[<Test>]
let ``abstract member declaration`` () =
    formatSourceString
        false
        """
type A =
    abstract B: ?p1:(float * int) -> unit
    abstract C: ?p1:float * int -> unit
    abstract D: ?p1:(int -> int) -> unit
    abstract E: ?p1:float -> unit
    abstract F: ?p1:float * ?p2:float -> unit
    abstract G: p1:float * ?p2:float -> unit
    abstract H: float * ?p2:float -> unit
    """
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    abstract B: ?p1: (float * int) -> unit
    abstract C: ?p1: float * int -> unit
    abstract D: ?p1: (int -> int) -> unit
    abstract E: ?p1: float -> unit
    abstract F: ?p1: float * ?p2: float -> unit
    abstract G: p1: float * ?p2: float -> unit
    abstract H: float * ?p2: float -> unit
"""

[<Test>]
let ``class declaration`` () =
    formatSourceString
        false
        """
type BaseClass = class
    val string1 : string
    new(str) = { string1 = str }
    new() = { string1 = "" }
end

type DerivedClass =
    inherit BaseClass
    val string2 : string
    new (str1, str2) = { inherit BaseClass(str1); string2 = str2 }
    new (str2) = { inherit BaseClass(); string2 = str2 }"""
        { config with MaxRecordWidth = 45 }
    |> prepend newline
    |> should
        equal
        """
type BaseClass =
    class
        val string1: string
        new(str) = { string1 = str }
        new() = { string1 = "" }
    end

type DerivedClass =
    inherit BaseClass
    val string2: string
    new(str1, str2) = { inherit BaseClass(str1); string2 = str2 }
    new(str2) = { inherit BaseClass(); string2 = str2 }
"""

[<Test>]
let ``classes and implicit constructors`` () =
    formatSourceString
        false
        """
    type MyClass2(dataIn) as self =
       let data = dataIn
       do self.PrintMessage()
       member this.PrintMessage() =
           printf "Creating MyClass2 with Data %d" data"""
        { config with
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type MyClass2(dataIn) as self =
    let data = dataIn
    do self.PrintMessage()
    member this.PrintMessage() = printf "Creating MyClass2 with Data %d" data
"""

[<Test>]
let ``classes and private implicit constructors`` () =
    formatSourceString
        false
        """
    type MyClass2 private (dataIn) as self =
       let data = dataIn
       do self.PrintMessage()
       member this.PrintMessage() =
           printf "Creating MyClass2 with Data %d" data"""
        { config with
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type MyClass2 private (dataIn) as self =
    let data = dataIn
    do self.PrintMessage()
    member this.PrintMessage() = printf "Creating MyClass2 with Data %d" data
"""

[<Test>]
let ``recursive classes`` () =
    formatSourceString
        false
        """
type Folder(pathIn: string) =
  let path = pathIn
  let filenameArray : string array = System.IO.Directory.GetFiles(path)
  member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray

and File(filename: string, containingFolder: Folder) =
   member __.Name = filename
   member __.ContainingFolder = containingFolder"""
        { config with
            MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type Folder(pathIn: string) =
    let path = pathIn
    let filenameArray: string array = System.IO.Directory.GetFiles(path)
    member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray

and File(filename: string, containingFolder: Folder) =
    member __.Name = filename
    member __.ContainingFolder = containingFolder
"""

[<Test>]
let ``classes and inheritance`` () =
    formatSourceString
        false
        """
type MyClassBase2(x: int) =
   let mutable z = x * x
   do for i in 1..z do printf "%d " i

type MyClassDerived2(y: int) =
   inherit MyClassBase2(y * 2)
   do for i in 1..y do printf "%d " i"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyClassBase2(x: int) =
    let mutable z = x * x

    do
        for i in 1..z do
            printf "%d " i

type MyClassDerived2(y: int) =
    inherit MyClassBase2(y * 2)

    do
        for i in 1..y do
            printf "%d " i
"""

[<Test>]
let ``should keep parens in class definition in the right place`` () =
    formatSourceString
        false
        """type DGMLClass() = class
    let mutable currentState = System.String.Empty
    end
    """
        config
    |> should
        equal
        """type DGMLClass() =
    class
        let mutable currentState = System.String.Empty
    end
"""

[<Test>]
let ``should keep parens in class inheritance in the right place`` () =
    formatSourceString
        false
        """type StateMachine(makeAsync) as this = class
    inherit DGMLClass()

    let functions = System.Collections.Generic.Dictionary<string, IState>()
    end
    """
        config
    |> should
        equal
        """type StateMachine(makeAsync) as this =
    class
        inherit DGMLClass()

        let functions = System.Collections.Generic.Dictionary<string, IState>()
    end
"""

[<Test>]
let ``should keep type annotations on auto properties`` () =
    formatSourceString
        false
        """type Document(id : string, library : string, name : string option) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
    member val LibraryID : string option = None with get, set
"""
        config
    |> should
        equal
        """type Document(id: string, library: string, name: string option) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
    member val LibraryID: string option = None with get, set
"""

[<Test>]
let ``should work on static auto properties`` () =
    formatSourceString
        false
        """type A() =
    static member val LastSchema = "" with get, set
"""
        config
    |> should
        equal
        """type A() =
    static member val LastSchema = "" with get, set
"""

[<Test>]
let ``member properties with type annotation`` () =
    formatSourceString
        false
        """type A() =
    member this.X with get():int = 1
    member this.Y with get():int = 1 and set (_:int):unit = ()
    member this.Z with set (_:int):unit = () and get():int = 1
"""
        config
    |> should
        equal
        """type A() =
    member this.X: int = 1

    member this.Y
        with get (): int = 1
        and set (_: int): unit = ()

    member this.Z
        with set (_: int): unit = ()
        and get (): int = 1
"""

[<Test>]
let ``class augmentation`` () =
    formatSourceString
        false
        """
type A () =
    let foo = () with
    let hello = "Hello"
    member this.X = "Member"
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A() =
    let foo = ()
    let hello = "Hello"
    member this.X = "Member"
"""

[<Test>]
let ``class inherit and augmentation`` () =
    formatSourceString
        false
        """
type A () =
    inherit B() with
    let hello = "Hello"
    member this.X = "Member"
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A() =
    inherit B()
    let hello = "Hello"
    member this.X = "Member"
"""

[<Test>]
let ``property long line`` () =
    formatSourceString
        false
        """type T() =
    member __.Property = "hello"
let longNamedFunlongNamedFunlongNamedFunlongNamedFunlongNamedFun (x:T) = x
let longNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClass = T()

System.String.Concat("a", "b" +
                            longNamedFunlongNamedFunlongNamedFunlongNamedFunlongNamedFun(longNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClass).Property)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type T() =
    member __.Property = "hello"

let longNamedFunlongNamedFunlongNamedFunlongNamedFunlongNamedFun (x: T) = x

let longNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClass =
    T()

System.String.Concat(
    "a",
    "b"
    + longNamedFunlongNamedFunlongNamedFunlongNamedFunlongNamedFun(
        longNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClasslongNamedClass
    )
        .Property
)
"""

[<Test>]
let ``indexed get long line`` () =
    formatSourceString
        false
        """open System
type Exception with
    member inline __.FirstLine =
        __.Message.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries).[0]
"""
        { config with
            MaxValueBindingWidth = 120
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> should
        equal
        """open System

type Exception with
    member inline __.FirstLine = __.Message.Split([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries).[0]
"""

[<Test>]
let ``no extra new lines between interface members, 569`` () =
    formatSourceString
        false
        """
namespace Quartz.Fsharp

module Logging =
    open Quartz.Logging
    open System

    //todo: it seems that quartz doesn't use mapped and nested context,
    //however, check if this is the best implementation for this interface
    type private QuartzLoggerWrapper(f) =
        interface ILogProvider with

            member this.OpenMappedContext(_, _) =
                { new IDisposable with
                    member this.Dispose() = () }

            member this.OpenNestedContext _ =
                { new IDisposable with
                    member this.Dispose() = () }

            member this.GetLogger _name = new Logger(f)

    let SetQuartzLoggingFunction f =
        let loggerFunction level (func: Func<string>) exc parameters =
            let wrappedFunction =
                Helpers.nullValuesToOptions (fun (x: Func<string>) -> (fun () -> x.Invoke())) func
            let wrappedException = Helpers.nullValuesToOptions id exc
            f level wrappedFunction wrappedException (parameters |> List.ofArray)

        LogProvider.SetCurrentLogProvider(QuartzLoggerWrapper(loggerFunction))

    let SetQuartzLogger l = LogProvider.SetCurrentLogProvider(l)
"""
        { config with
            MaxFunctionBindingWidth = 80 }
    |> prepend newline
    |> should
        equal
        """
namespace Quartz.Fsharp

module Logging =
    open Quartz.Logging
    open System

    //todo: it seems that quartz doesn't use mapped and nested context,
    //however, check if this is the best implementation for this interface
    type private QuartzLoggerWrapper(f) =
        interface ILogProvider with

            member this.OpenMappedContext(_, _) =
                { new IDisposable with
                    member this.Dispose() = () }

            member this.OpenNestedContext _ =
                { new IDisposable with
                    member this.Dispose() = () }

            member this.GetLogger _name = new Logger(f)

    let SetQuartzLoggingFunction f =
        let loggerFunction level (func: Func<string>) exc parameters =
            let wrappedFunction =
                Helpers.nullValuesToOptions (fun (x: Func<string>) -> (fun () -> x.Invoke())) func

            let wrappedException = Helpers.nullValuesToOptions id exc
            f level wrappedFunction wrappedException (parameters |> List.ofArray)

        LogProvider.SetCurrentLogProvider(QuartzLoggerWrapper(loggerFunction))

    let SetQuartzLogger l = LogProvider.SetCurrentLogProvider(l)
"""

[<Test>]
let ``no extra new lines between type members, 569`` () =
    formatSourceString
        false
        """
type A() =

    member this.MemberA = if true then 0 else 1

    member this.MemberB = if true then 2 else 3

    member this.MemberC = 0"""
        { config with
            MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type A() =

    member this.MemberA = if true then 0 else 1

    member this.MemberB = if true then 2 else 3

    member this.MemberC = 0
"""

[<Test>]
let ``no extra new line before nested module with attribute, 586`` () =
    formatSourceString
        false
        """
module A =
    let x = 0

    [<RequireQualifiedAccess>]
    module B =
        let y = 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
module A =
    let x = 0

    [<RequireQualifiedAccess>]
    module B =
        let y = 1
"""

[<Test>]
let ``no extra new line before abstract member with attribute, 586`` () =
    formatSourceString
        false
        """
type A =

    [<EmitConstructor>]
    abstract Create: Unit -> A

    abstract b: Unit -> Unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =

    [<EmitConstructor>]
    abstract Create: Unit -> A

    abstract b: Unit -> Unit
"""

[<Test>]
let ``no extra new line between abstract members with attribute, 586`` () =
    formatSourceString
        false
        """
type A =

    [<Emit("a")>]
    abstract a: Unit -> string

    [<Emit("b")>]
    abstract b: Unit -> string
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =

    [<Emit("a")>]
    abstract a: Unit -> string

    [<Emit("b")>]
    abstract b: Unit -> string
"""

[<Test>]
let ``string parameter to inherited class, 720`` () =
    formatSourceString
        false
        """type Child() =
  inherit Parent ""
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Child() =
    inherit Parent ""
"""

[<Test>]
let ``float parameter to inherited class`` () =
    formatSourceString
        false
        """type Child() =
  inherit Parent 7.9
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Child() =
    inherit Parent 7.9
"""

[<Test>]
let ``unit parameter to inherited class`` () =
    formatSourceString
        false
        """type Child() =
  inherit Parent ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Child() =
    inherit Parent()
"""

[<Test>]
let ``multiline string inside constructor, 1055`` () =
    formatSourceString
        false
        "
module Fantomas.Tests.ConfigTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers

[<Test>]
let ``config file in working directory should not require relative prefix, 821`` () =
    use fileFixture =
        new TemporaryFileCodeSample(\"let a  = // foo
                                                            9\")

    use configFixture =
        new ConfigurationFile(\"\"\"
[*.fs]
indent_size=2
\"\"\"                                )

    let (exitCode, output) =
        runFantomasTool fileFixture.Filename

    exitCode |> should equal 0
    output
    |> should startWith (sprintf \"Processing %s\" fileFixture.Filename)

    let result = System.IO.File.ReadAllText(fileFixture.Filename)
    result
    |> should equal \"\"\"let a = // foo
  9
\"\"\"
"
        { config with
            MaxInfixOperatorExpression = 40 }
    |> prepend newline
    |> should
        equal
        "
module Fantomas.Tests.ConfigTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers

[<Test>]
let ``config file in working directory should not require relative prefix, 821`` () =
    use fileFixture =
        new TemporaryFileCodeSample(
            \"let a  = // foo
                                                            9\"
        )

    use configFixture =
        new ConfigurationFile(
            \"\"\"
[*.fs]
indent_size=2
\"\"\"
        )

    let (exitCode, output) = runFantomasTool fileFixture.Filename

    exitCode |> should equal 0

    output
    |> should startWith (sprintf \"Processing %s\" fileFixture.Filename)

    let result = System.IO.File.ReadAllText(fileFixture.Filename)

    result
    |> should
        equal
        \"\"\"let a = // foo
  9
\"\"\"
"

[<Test>]
let ``keep abstract member keywords, 1106`` () =
    formatSourceString
        false
        """
module Example

type Foo =
    abstract member bar : int
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Example

type Foo =
    abstract member bar: int
"""

[<Test>]
let ``multiline constructor in class, 1359`` () =
    formatSourceString
        false
        """
type SomeVeryLongTypeNameWithConstructor
    (
        aVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
    )
    =
    class
    end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type SomeVeryLongTypeNameWithConstructor
    (
        aVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
    ) =
    class
    end
"""

[<Test>]
let ``long type generic type constraints with unit constructors, 1494`` () =
    formatSourceString
        false
        """
type ISingleExpressionValue<'p, 'o, 'v when 'p :> IProperty and 'o :> IOperator and 'p: equality and 'o: equality and 'v: equality> () =
    abstract Property: 'p
    abstract Operator: 'o
    abstract Value: 'v
"""
        config
    |> prepend newline
    |> should
        equal
        """
type ISingleExpressionValue<'p, 'o, 'v
    when 'p :> IProperty and 'o :> IOperator and 'p: equality and 'o: equality and 'v: equality>() =
    abstract Property: 'p
    abstract Operator: 'o
    abstract Value: 'v
"""

[<Test>]
let ``comment before multiline class member`` () =
    formatSourceString
        false
        """
type MaybeBuilder () =
    member inline __.Bind
// meh
        (value, binder : 'T -> 'U option) : 'U option =
        Option.bind binder value
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MaybeBuilder() =
    member inline __.Bind
        // meh
        (
            value,
            binder: 'T -> 'U option
        ) : 'U option =
        Option.bind binder value
"""

[<Test>]
let ``define hashes around member binding, 1753`` () =
    formatSourceString
        false
        """
[<Sealed>]
type MaybeBuilder () =
    // M<'T> * ('T -> M<'U>) -> M<'U>
#if DEBUG
    member __.Bind
#else
    member inline __.Bind
#endif
        (value, binder : 'T -> 'U option) : 'U option =
        Option.bind binder value
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Sealed>]
type MaybeBuilder() =
    // M<'T> * ('T -> M<'U>) -> M<'U>
#if DEBUG
    member __.Bind
#else
    member inline __.Bind
#endif
        (
            value,
            binder: 'T -> 'U option
        ) : 'U option =
        Option.bind binder value
"""

[<Test>]
let ``avoid vanity alignment when calling base constructor, 1442`` () =
    formatSourceString
        false
        """
type public DerivedExceptionWithLongNaaaaaaaaameException (message: string,
                                                           code: int,
                                                           originalRequest: string,
                                                           originalResponse: string) =
    inherit BaseExceptionWithLongNaaaameException(message, code, originalRequest, originalResponse)"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
type public DerivedExceptionWithLongNaaaaaaaaameException
    (
        message: string,
        code: int,
        originalRequest: string,
        originalResponse: string
    ) =
    inherit
        BaseExceptionWithLongNaaaameException(
            message,
            code,
            originalRequest,
            originalResponse
        )
"""

[<Test>]
let ``correct indentation when calling base constructor, 1942`` () =
    formatSourceString
        false
        """
type public DerivedExceptionWithLongNaaaaaaaaameException (message: string,
                                                           code: int,
                                                           originalRequest: string,
                                                           originalResponse: string) =
    inherit BaseExceptionWithLongNaaaameException(message, code, originalRequest, originalResponse)

    let myMethod () =
        ()

    override this.SomeMethod () =
        ()"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
type public DerivedExceptionWithLongNaaaaaaaaameException
    (
        message: string,
        code: int,
        originalRequest: string,
        originalResponse: string
    ) =
    inherit
        BaseExceptionWithLongNaaaameException(
            message,
            code,
            originalRequest,
            originalResponse
        )

    let myMethod () = ()

    override this.SomeMethod() = ()
"""

[<Test>]
let ``explicit class/end/with, 1940`` () =
    formatSourceString
        false
        """
type C() =
  class
   member x.P = 1
  end
  with
    member _.Run() = 1
"""
        { config with
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
type C() =
    class
        member x.P = 1
    end

    member _.Run() = 1
"""

[<Test>]
let ``static member with get unit should be formatted the same as without, 1913`` () =
    formatSourceString
        false
        """
type Subject<'a> private () =

        /// Represents and object that is both an observable sequence as well as an observer.
        /// Each notification is broadcasted to all subscribed observers.
        static member broadcast
            with get () = new System.Reactive.Subjects.Subject<'a> ()

type Subject<'a> private () =

    /// Represents and object that is both an observable sequence as well as an observer.
    /// Each notification is broadcasted to all subscribed observers.
    static member broadcast = new System.Reactive.Subjects.Subject<'a>()

"""
        config
    |> prepend newline
    |> should
        equal
        """
type Subject<'a> private () =

    /// Represents and object that is both an observable sequence as well as an observer.
    /// Each notification is broadcasted to all subscribed observers.
    static member broadcast = new System.Reactive.Subjects.Subject<'a>()

type Subject<'a> private () =

    /// Represents and object that is both an observable sequence as well as an observer.
    /// Each notification is broadcasted to all subscribed observers.
    static member broadcast = new System.Reactive.Subjects.Subject<'a>()
"""

[<Test>]
let ``comments after equals sign in read/write property`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.MyReadWriteProperty
        with get () =   //comment get 
            myInternalValue
        and set (value) =   // comment set
            myInternalValue <- value
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.MyReadWriteProperty
        with get () = //comment get
            myInternalValue
        and set (value) = // comment set
            myInternalValue <- value
"""

[<Test>]
let ``preserve override keyword in read property, 2221`` () =
    formatSourceString
        false
        """
type StreamHelper() =
    inherit Stream()

    override x.ReadAsync (dst, offset, count, tok) = 
        ()
    override x.WriteAsync (dst, offset, count, tok) = 
        ()
    override x.Flush () = ()
    override x.Seek(offset:int64, origin:SeekOrigin) =
        ()
    override x.SetLength(value:int64) =
        ()
    override x.Read(dst, offset, count) = 
        ()           
    override x.Write(src, offset, count) = 
        ()
    override x.ReadByte() =
        ()
    override x.WriteByte item =
        ()
    override x.CanRead 
        with get() =
            true
    override x.CanSeek
        with get() =
            false
    override x.CanWrite
        with get() =
            true
    override x.Length
        with get() =
            1
    override x.Position
        with get() =
            1
        and set value =
            1
    override x.Dispose disposing =
        ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type StreamHelper() =
    inherit Stream()

    override x.ReadAsync(dst, offset, count, tok) = ()
    override x.WriteAsync(dst, offset, count, tok) = ()
    override x.Flush() = ()
    override x.Seek(offset: int64, origin: SeekOrigin) = ()
    override x.SetLength(value: int64) = ()
    override x.Read(dst, offset, count) = ()
    override x.Write(src, offset, count) = ()
    override x.ReadByte() = ()
    override x.WriteByte item = ()
    override x.CanRead = true
    override x.CanSeek = false
    override x.CanWrite = true
    override x.Length = 1

    override x.Position
        with get () = 1
        and set value = 1

    override x.Dispose disposing = ()
"""

[<Test>]
let ``trivia before and keyword in SynMemberDefn.GetSet, 2372`` () =
    formatSourceString
        false
        """
{ new TaskDefinition with
    member this.Item
        with get (name: string): obj option = data.TryGet name


        and set (name: string) (v: obj option): unit =
            match v with
            | None -> data.Remove(name) |> ignore
            | Some v -> data.[name] <- v

    override this.``type``: string = "fakerun" }
"""
        config
    |> prepend newline
    |> should
        equal
        """
{ new TaskDefinition with
    member this.Item
        with get (name: string): obj option = data.TryGet name


        and set (name: string) (v: obj option): unit =
            match v with
            | None -> data.Remove(name) |> ignore
            | Some v -> data.[name] <- v

    override this.``type``: string = "fakerun" }
"""

[<Test>]
let ``interface member with anonymous record as generic type, 2396`` () =
    formatSourceString
        false
        """
// ts2fable 0.8.0
module rec Xterm

type [<AllowNullLiteral>] Terminal =
    abstract onKey: IEvent<{| key: string; domEvent: KeyboardEvent |}> with get, set
    abstract onLineFeed: IEvent<unit> with get, set
"""
        config
    |> prepend newline
    |> should
        equal
        """
// ts2fable 0.8.0
module rec Xterm

[<AllowNullLiteral>]
type Terminal =
    abstract onKey:
        IEvent<{| key: string
                  domEvent: KeyboardEvent |}> with get, set

    abstract onLineFeed: IEvent<unit> with get, set
"""

[<Test>]
let ``trivia between xml document and abstract slot, 2433`` () =
    formatSourceString
        false
        """
type Foo =
    /// Bar
    // Baz
    abstract Quux : int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo =
    /// Bar
    // Baz
    abstract Quux: int
"""

[<Test>]
let ``attribute on ExplicitCtor is lost, 2638`` () =
    formatSourceString
        false
        """
open System

type MyClass (a: int, b: int) =

    member val PropA = a with get, set
    member val PropB = b with get, set

    [<Obsolete("Do not use")>]
    new(x: int) =
        MyClass(x, x)
"""
        config
    |> prepend newline
    |> should
        equal
        """
open System

type MyClass(a: int, b: int) =

    member val PropA = a with get, set
    member val PropB = b with get, set

    [<Obsolete("Do not use")>]
    new(x: int) = MyClass(x, x)
"""
