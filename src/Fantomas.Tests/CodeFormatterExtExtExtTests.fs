module Fantomas.Tests.CodeFormatterExtExtExtTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

[<Test>]
let ``type providers``() =
    formatSourceString false """
type Northwind = ODataService<"http://services.odata.org/Northwind/Northwind.svc/">""" config
    |> prepend newline
    |> should equal """
type Northwind = ODataService<"http://services.odata.org/Northwind/Northwind.svc/">
"""

[<Test>]
let ``named arguments``() =
    formatSourceString false """
type SpeedingTicket() =
    member this.GetMPHOver(speed: int, limit: int) = speed - limit

let CalculateFine (ticket : SpeedingTicket) =
    let delta = ticket.GetMPHOver(limit = 55, speed = 70)
    if delta < 20 then 50.0 else 100.0""" config
    |> prepend newline
    |> should equal """
type SpeedingTicket() = 
    member this.GetMPHOver(speed : int, limit : int) = speed - limit

let CalculateFine(ticket : SpeedingTicket) = 
    let delta = ticket.GetMPHOver(limit = 55, speed = 70)
    if delta < 20
    then 50.0
    else 100.0
"""

[<Test>]
let ``array indices``() =
    formatSourceString false """
let array1 = [| 1; 2; 3 |]
array1.[0..2] 
array2.[2.., 0..]
array2.[..3, ..1] 
array1.[1] <- 3
    """ config
    |> prepend newline
    |> should equal """
let array1 = [|1; 2; 3|]

array1.[0..2]
array2.[2.., 0..]
array2.[..3, ..1]
array1.[1] <- 3
"""

[<Test>]
let ``array values``() =
    formatSourceString false """
let arr = [|(1, 1, 1); (1, 2, 2); (1, 3, 3); (2, 1, 2); (2, 2, 4); (2, 3, 6); (3, 1, 3);
  (3, 2, 6); (3, 3, 9)|]
    """ config
    |> prepend newline
    |> should equal """
let arr = 
    [|(1, 1, 1)
      (1, 2, 2)
      (1, 3, 3)
      (2, 1, 2)
      (2, 2, 4)
      (2, 3, 6)
      (3, 1, 3)
      (3, 2, 6)
      (3, 3, 9)|]
"""

[<Test>]
let ``comments on local let bindings``() =
    formatSourceString false """
let print_30_permut() = 

    /// declare and initialize
    let permutation : int array = Array.init n (fun i -> Console.Write(i+1); i)
    permutation
    """ config
    |> prepend newline
    |> should equal """
let print_30_permut() = 
    /// declare and initialize
    let permutation : int array = 
        Array.init n (fun i -> 
                Console.Write(i + 1)
                i)
    permutation
"""

[<Test>]
let ``multiline strings``() =
    formatSourceString false """
let alu =
        "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
        GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
        CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
        ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
        GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
        AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
  AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"B
    """ config
    |> prepend newline
    |> should equal """
let alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
        GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
        CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
        ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
        GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
        AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
  AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"B
"""

[<Test>]
let ``indexed properties``() =
    formatSourceString false """
type NumberStrings() =
   let mutable ordinals = [| "one"; |]
   let mutable cardinals = [| "first"; |]
   member this.Item
      with get index = ordinals.[index]
      and set index value = ordinals.[index] <- value
   member this.Ordinal
      with get(index) = ordinals.[index]
      and set index value = ordinals.[index] <- value
   member this.Cardinal
      with get(index) = cardinals.[index]
      and set index value = cardinals.[index] <- value""" config
    |> prepend newline
    |> should equal """
type NumberStrings() = 
    let mutable ordinals = [|"one"|]
    let mutable cardinals = [|"first"|]
    member this.Item with get index = ordinals.[index]
    member this.Item with set index value = ordinals.[index] <- value
    member this.Ordinal with get (index) = ordinals.[index]
    member this.Ordinal with set index value = ordinals.[index] <- value
    member this.Cardinal with get (index) = cardinals.[index]
    member this.Cardinal with set index value = cardinals.[index] <- value
"""

[<Test>]
let ``complex indexed properties``() =
    formatSourceString false """
open System.Collections.Generic
type SparseMatrix() =
    let mutable table = new Dictionary<int * int, float>()
    member this.Item
        with get(key1, key2) = table.[(key1, key2)]
        and set (key1, key2) value = table.[(key1, key2)] <- value

let matrix1 = new SparseMatrix()
for i in 1..1000 do
    matrix1.[i, i] <- float i * float i
    """ config
    |> prepend newline
    |> should equal """
open System.Collections.Generic

type SparseMatrix() = 
    let mutable table = new Dictionary<int * int, float>()
    member this.Item with get (key1, key2) = table.[(key1, key2)]
    member this.Item with set (key1, key2) value = table.[(key1, key2)] <- value

let matrix1 = new SparseMatrix()

for i in 1..1000 do
    matrix1.[i, i] <- float i * float i
"""

[<Test>]
let ``then blocks after constructors``() =
    formatSourceString false """
type Person(nameIn : string, idIn : int) =
    let mutable name = nameIn
    let mutable id = idIn
    do printfn "Created a person object." 
    member this.Name with get() = name and set(v) = name <- v
    member this.ID with get() = id and set(v) = id <- v
    new() = 
        Person("Invalid Name", -1)
        then
            printfn "Created an invalid person object."
            """ config
    |> prepend newline
    |> should equal """
type Person(nameIn : string, idIn : int) = 
    let mutable name = nameIn
    let mutable id = idIn
    do printfn "Created a person object."
    member this.Name with get () = name
    member this.Name with set (v) = name <- v
    member this.ID with get () = id
    member this.ID with set (v) = id <- v
    new() = 
        Person("Invalid Name", -1)
        then printfn "Created an invalid person object."
"""

[<Test>]
let ``associativity of types``() =
    formatSourceString false """
type Delegate1 = delegate of (int * int) * (int * int) -> int
type Delegate2 = delegate of int * int -> int
type Delegate3 = delegate of int -> (int -> int)
type Delegate4 = delegate of int -> int -> int
type U = U of (int * int)
    """ config
    |> prepend newline
    |> should equal """
type Delegate1 = delegate of (int * int) * (int * int) -> int
type Delegate2 = delegate of int * int -> int
type Delegate3 = delegate of int -> (int -> int)
type Delegate4 = delegate of int -> int -> int
type U = 
    | U of (int * int)
"""

[<Test>]
let ``module signatures``() =
    formatSourceString true """
module Utils

val turnTracingOn : unit -> unit
val turnTracingOff : unit -> unit
val isTraced : unit -> bool

module Random = begin
    val exponential : mean:float -> float
    val nextInt : max:int -> int
    val nextInt64 : max:int64 -> int64
    val next : max:float -> float
end""" config
    |> prepend newline
    |> should equal """
module Utils

val turnTracingOn : unit -> unit

val turnTracingOff : unit -> unit

val isTraced : unit -> bool

module Random = 
    val exponential : mean:float -> float
    
    val nextInt : max:int -> int
    
    val nextInt64 : max:int64 -> int64
    
    val next : max:float -> float
    """

[<Test>]
let ``class signatures``() =
    formatSourceString true """
module Heap

type Heap<'T when 'T : comparison> =
    class
    new : capacity:int -> Heap<'T>
    member Clear : unit -> unit
    member ExtractMin : unit -> 'T
    member Insert : k:'T -> unit
    member IsEmpty : unit -> bool
    member PeekMin : unit -> 'T
    override ToString : unit -> string
    member Count : int
    end""" config
    |> prepend newline
    |> should equal """
module Heap

type Heap<'T when 'T : comparison> = 
    class
        new : capacity:int -> Heap<'T>
        member Clear : unit -> unit
        member ExtractMin : unit -> 'T
        member Insert : k:'T -> unit
        member IsEmpty : unit -> bool
        member PeekMin : unit -> 'T
        override ToString : unit -> string
        member Count : int
    end
"""

[<Test>]
let ``record signatures``() =
    formatSourceString true """
/// Represents simple XML elements.
type Element =
    {
        /// The attribute collection.
        Attributes : IDictionary<Name,string>

        /// The children collection.
        Children : seq<INode>

        /// The qualified name.
        Name : Name
    }

    interface INode

    /// Constructs an new empty Element.
    static member Create : name: string * ?uri: string -> Element

    /// Replaces the children.
    static member WithChildren : children: #seq<#INode> -> self: Element -> Element

    /// Replaces the children.
    static member ( - ) : self: Element * children: #seq<#INode> -> Element

    /// Replaces the attributes.
    static member WithAttributes : attrs: #seq<string*string> -> self: Element -> Element

    /// Replaces the attributes.
    static member ( + ) : self: Element * attrs: #seq<string*string> -> Element

    /// Replaces the children with a single text node.
    static member WithText : text: string -> self: Element-> Element

    /// Replaces the children with a single text node.
    static member ( -- ) : self: Element * text: string -> Element""" config
    |> prepend newline
    |> should equal """
/// Represents simple XML elements.
type Element = 
    { /// The attribute collection.
      Attributes : IDictionary<Name, string>;
      /// The children collection.
      Children : seq<INode>;
      /// The qualified name.
      Name : Name }
    interface INode
    /// Constructs an new empty Element.
    static member Create : name:string * ?uri:string -> Element
    /// Replaces the children.
    static member WithChildren : children:#seq<#INode>
         -> self:Element -> Element
    /// Replaces the children.
    static member (-) : self:Element * children:#seq<#INode> -> Element
    /// Replaces the attributes.
    static member WithAttributes : attrs:#seq<string * string>
         -> self:Element -> Element
    /// Replaces the attributes.
    static member (+) : self:Element * attrs:#seq<string * string> -> Element
    /// Replaces the children with a single text node.
    static member WithText : text:string -> self:Element -> Element
    /// Replaces the children with a single text node.
    static member (--) : self:Element * text:string -> Element
"""