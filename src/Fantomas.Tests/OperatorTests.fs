module Fantomas.Tests.OperatorTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``should format prefix operators``() =
    formatSourceString false """let x = -y
let z = !!x
    """ config
    |> should equal """let x = -y
let z = !!x
"""

[<Test>]
let ``should keep triple ~~~ operator``() =
    formatSourceString false """x ~~~FileAttributes.ReadOnly
    """ config
    |> should equal """x ~~~FileAttributes.ReadOnly
"""

[<Test>]
let ``should keep single triple ~~~ operator``() =
    formatSourceString false """~~~FileAttributes.ReadOnly
    """ config
    |> should equal """~~~FileAttributes.ReadOnly
"""

[<Test>]
let ``should keep parens around ? operator definition``() =
    formatSourceString false """let (?) f s = f s
    """ config
    |> should equal """let (?) f s = f s
"""

[<Test>]
let ``should keep parens around ?<- operator definition``() =
    formatSourceString false """let (?<-) f s = f s
    """ config
    |> should equal """let (?<-) f s = f s
"""

[<Test>]
let ``should keep parens around !+ prefix operator definition``() =
    formatSourceString false """let (!+) x = Include x
    """ config
    |> should equal """let (!+) x = Include x
"""

[<Test>]
let ``should keep parens around ++ infix operator definition``() =
    formatSourceString false """let (++) x y = { x with Includes = y :: x.Includes }
    """ config
    |> should equal """let (++) x y = { x with Includes = y :: x.Includes }
"""

[<Test>]
let ``should keep parens around inlined ==> operator definition``() =
    formatSourceString false """let inline (==>) x y = f x y
    """ config
    |> should equal """let inline (==>) x y = f x y
"""

[<Test>]
let ``should keep parens around inlined operator definition``() =
    formatSourceString false """let inline (@@) path1 path2 = Path.Combine(path1, path2)
    """ config
    |> should equal """let inline (@@) path1 path2 = Path.Combine(path1, path2)
"""

[<Test>]
let ``should pattern match on quotation expression``() =
    formatSourceString false """let rec print expr =
    match expr with
    | SpecificCall <@@ (+) @@> (_, _, exprList) ->        
        print exprList.Head
        printf " + "
        print exprList.Tail.Head
    | _ -> ()""" config
    |> should equal """let rec print expr =
    match expr with
    | SpecificCall <@@ (+) @@> (_, _, exprList) ->
        print exprList.Head
        printf " + "
        print exprList.Tail.Head
    | _ -> ()
"""

[<Test>]
let ``should break on . operator``() =
    formatSourceString false """pattern.Replace(".", @"\.").Replace("$", @"\$").Replace("^", @"\^").Replace("{", @"\{").Replace("[", @"\[").Replace("(", @"\(").Replace(")", @"\)").Replace("+", @"\+")

    """ { config with MaxLineLength = 80 }
    |> prepend newline
    |> should equal """
pattern.Replace(".", @"\.").Replace("$", @"\$").Replace("^", @"\^")
       .Replace("{", @"\{").Replace("[", @"\[").Replace("(", @"\(")
       .Replace(")", @"\)").Replace("+", @"\+")
"""

// the current behavior results in a compile error since line break is before the parens and not before the .
[<Test>]
let ``should break on . operator and keep indentation``() =
    formatSourceString false """let pattern = 
    (x + y)
      .Replace(seperator + "**" + seperator, replacementSeparator + "(.|?" + replacementSeparator + ")?" )
      .Replace("**" + seperator, ".|(?<=^|" + replacementSeparator + ")" )
    """ { config with MaxLineLength = 80; MaxInfixOperatorExpression = 60 }
    |> should equal """let pattern =
    (x + y)
        .Replace
            (seperator + "**" + seperator,
             replacementSeparator + "(.|?" + replacementSeparator + ")?")
        .Replace("**" + seperator, ".|(?<=^|" + replacementSeparator + ")")
"""

[<Test>]
let ``should keep space between ( and * in *** operator definition``() =
    formatSourceString false """let inline ( ***) l1 l2 = pair l2 l1
    """ config
    |> should equal """let inline ( *** ) l1 l2 = pair l2 l1
"""

[<Test>]
let ``should keep space between ( and * in *= operator definition``() =
    formatSourceString false """let inline ( *=) l v = update (( *) v) l
    """ config
    |> should equal """let inline ( *= ) l v = update ((*) v) l
"""

[<Test>]
let ``should not add space around ? operator``() =
    formatSourceString false """let x = y?z.d?c.[2]?d.xpto()""" config
    |> should equal """let x = y?z.d?c.[2]?d.xpto()
"""

[<Test>]
let ``should understand ? as an infix operator``() =
    formatSourceString false """try 
    item.MethodInfo.Method.Invoke(null, ipa)
    |> (fun x -> x?Invoke (true))
    |> fun (t : Task) -> t.Wait()
with _ -> ()""" config
    |> should equal """try
    item.MethodInfo.Method.Invoke(null, ipa)
    |> (fun x -> x?Invoke (true))
    |> fun (t: Task) -> t.Wait()
with _ -> ()
"""

[<Test>]
let ``should not mess up ?<- operator``() =
    formatSourceString false """x?v <- 2""" config
    |> should equal """x?v <- 2
"""


[<Test>]
let ``should pipeline monadic bind``() =
    formatSourceString false """strToInt "1"
>>= strAddLong "A long argument that is ignored" "2"
>>= strAddLong "A long argument that is ignored" "2"
>>= strAddLong "A long argument that is ignored" "2"
>>= strAddLong "A long argument that is ignored" "2"
>>= strAddLong "A long argument that is ignored" "2"
>>= strAddLong "A long argument that is ignored" "2"
"""
        config
    |> should equal  """strToInt "1"
>>= strAddLong "A long argument that is ignored" "2"
>>= strAddLong "A long argument that is ignored" "2"
>>= strAddLong "A long argument that is ignored" "2"
>>= strAddLong "A long argument that is ignored" "2"
>>= strAddLong "A long argument that is ignored" "2"
>>= strAddLong "A long argument that is ignored" "2"
"""

[<Test>]
let ``should keep >>.~ operator``() =
    formatSourceString false """let (>>.~) (g : int) (h : int) : int = g + h
let output = 2 >>.~ 3
    """ config
    |> should equal """let (>>.~) (g: int) (h: int): int = g + h
let output = 2 >>.~ 3
"""

[<Test>]
let ``should not add newline before = operator after |>``() =
    formatSourceString false """1 |> max 0 = 1""" ({ config with MaxInfixOperatorExpression = 10 })
    |> should equal """1
|> max 0 = 1
"""

[<Test>]
let ``should add space around .. operator``() =
    formatSourceString false """[1..10]""" config
    |> should equal """[ 1 .. 10 ]
"""


[<Test>]
let ``should add space around .. .. operators``() =
    formatSourceString false """[10 .. -1 .. 1]""" config
    |> should equal """[ 10 .. -1 .. 1 ]
"""

[<Test>]
let ``line comment after infix function with parenthesis, 559`` () =
    formatSourceString false """let watchFiles =
        async {
            printfn "after start"
            use _ =
                !!(serverPath </> "*.fs") ++ (serverPath </> "*.fsproj") // combines fs and fsproj
                |> ChangeWatcher.run (fun changes ->
                                      printfn
                                          "FILE CHANGE %A"
                                          changes
                                      // stopFunc()
                                      //Async.Start (startFunc())
                                      )
            ()
        }
"""  config
    |> prepend newline
    |> should equal """
let watchFiles =
    async {
        printfn "after start"

        use _ =
            !!(serverPath </> "*.fs")
            ++ (serverPath </> "*.fsproj") // combines fs and fsproj
            |> ChangeWatcher.run (fun changes -> printfn "FILE CHANGE %A" changes
                // stopFunc()
                //Async.Start (startFunc())
                )

        ()
    }
"""

[<Test>]
let ``line comment after infix function with string constant, 559`` () =
    formatSourceString false """let watchFiles =
        async {
            printfn "after start"
            use _ =
                !!(serverPath </> "*.fs") ++ "*.fsproj" // combines fs and fsproj
                |> ChangeWatcher.run (fun changes ->
                                      printfn
                                          "FILE CHANGE %A"
                                          changes
                                      // stopFunc()
                                      //Async.Start (startFunc())
                                      )
            ()
        }
"""  config
    |> prepend newline
    |> should equal """
let watchFiles =
    async {
        printfn "after start"

        use _ =
            !!(serverPath </> "*.fs")
            ++ "*.fsproj" // combines fs and fsproj
            |> ChangeWatcher.run (fun changes -> printfn "FILE CHANGE %A" changes
                // stopFunc()
                //Async.Start (startFunc())
                )

        ()
    }
"""

[<Test>]
let ``short expression before and after pipe`` () =
    formatSourceString false "let a = b |> c"  config
    |> prepend newline
    |> should equal """
let a = b |> c
"""

[<Test>]
let ``long expression with pipe should be multiline`` () =
    formatSourceString false "let a = List.init 40 (fun i -> generateThing i a) |> List.map mapThingToOtherThing"  config
    |> prepend newline
    |> should equal """
let a =
    List.init 40 (fun i -> generateThing i a)
    |> List.map mapThingToOtherThing
"""

[<Test>]
let ``giraffe sample`` () =
    formatSourceString false """
let WebApp = route "/ping" >=> authorized >=> text "pong"
"""  ({ config with MaxInfixOperatorExpression = 40 })
    |> prepend newline
    |> should equal """
let WebApp =
    route "/ping"
    >=> authorized
    >=> text "pong"
"""

[<Test>]
let ``multiple short pipes`` () =
    formatSourceString false """let result = a && b |>  f |>  g |>   h
"""  config
    |> prepend newline
    |> should equal """
let result = a && b |> f |> g |> h
"""

[<Test>]
let ``pipe boolean expression`` () =
    formatSourceString false """b && c |> someLongExpressionThatShouldMoveThePipeToTheNextLine
"""  config
    |> prepend newline
    |> should equal """
b
&& c
|> someLongExpressionThatShouldMoveThePipeToTheNextLine
"""

[<Test>]
let ``two long boolean expressions`` () =
    formatSourceString false """aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa || bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
"""  config
    |> prepend newline
    |> should equal """
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
|| bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
"""

[<Test>]
let ``equal sign operator should not move to next line`` () =
    formatSourceString false """let result =
            (typ.GetInterface(typeof<System.Collections.IEnumerable>.FullName) = null)
"""  config
    |> prepend newline
    |> should equal """
let result =
    (typ.GetInterface(typeof<System.Collections.IEnumerable>.FullName) = null)
"""

[<Test>]
let ``operator before verbatim string add extra space, 736`` () =
    formatSourceString false """Target M.Tools (fun _ -> !! @"Tools\Tools.sln" |> rebuild)
"""  config
    |> prepend newline
    |> should equal """
Target M.Tools (fun _ -> !! @"Tools\Tools.sln" |> rebuild)
"""

[<Test>]
let ``function call before pipe operator, 754`` () =
    formatSourceString false "
[<Test>]
let ``attribute on module after namespace`` () =
    formatSourceString false \"\"\"namespace SomeNamespace

[<AutoOpen>]
module Types =
    let a = 5
\"\"\"  config
    |> prepend newline
    |> should equal \"\"\"
namespace SomeNamespace

[<AutoOpen>]
module Types =
    let a = 5
\"\"\"
"    config
    |> prepend newline
    |> should equal "
[<Test>]
let ``attribute on module after namespace`` () =
    formatSourceString false \"\"\"namespace SomeNamespace

[<AutoOpen>]
module Types =
    let a = 5
\"\"\"  config
    |> prepend newline
    |> should equal \"\"\"
namespace SomeNamespace

[<AutoOpen>]
module Types =
    let a = 5
\"\"\"
"

[<Test>]
let ``modulo operator on same line, 780`` () =
    formatSourceString false """let hasUnEvenAmount regex line = (Regex.Matches(line, regex).Count - Regex.Matches(line, "\\\\" + regex).Count) % 2 = 1
"""  config
    |> prepend newline
    |> should equal """
let hasUnEvenAmount regex line =
    (Regex.Matches(line, regex).Count
     - Regex.Matches(line, "\\\\" + regex).Count) % 2 = 1
"""

[<Test>]
let ``parameter after multiline string, 783`` () =
    formatSourceString false "
let ``match bang`` () =
    formatSourceString false \"\"\"
async {
    match! myAsyncFunction() with
    | Some x -> printfn \"%A\" x
    | None -> printfn \"Function returned None!\"
}\"\"\"   config
    |> prepend newline
    |> should equal \"\"\"
async {
    match! myAsyncFunction () with
    | Some x -> printfn \"%A\" x
    | None -> printfn \"Function returned None!\"
}
\"\"\"
"      config
    |> prepend newline
    |> should equal "
let ``match bang`` () =
    formatSourceString false \"\"\"
async {
    match! myAsyncFunction() with
    | Some x -> printfn \"%A\" x
    | None -> printfn \"Function returned None!\"
}\"\"\" config
    |> prepend newline
    |> should equal \"\"\"
async {
    match! myAsyncFunction () with
    | Some x -> printfn \"%A\" x
    | None -> printfn \"Function returned None!\"
}
\"\"\"
"

[<Test>]
let ``addition via function`` () =
    formatSourceString false """let a = (+) 7 8
"""  config
    |> prepend newline
    |> should equal """
let a = (+) 7 8
"""

[<Test>]
let ``lambda piped to lambda should be multiline, 942`` () =
    formatSourceString false """
let r (f : 'a -> 'b) (a : 'a) : 'b =
    fun () ->
        f a
    |> fun f -> f ()
"""  config
    |> prepend newline
    |> should equal """
let r (f: 'a -> 'b) (a: 'a): 'b =
    fun () -> f a
    |> fun f -> f ()
"""

[<Test>]
let ``combining lines breaks function precedence 488`` () =
    formatSourceString false """fun () -> ()
|> Some
"""  config
    |> prepend newline
    |> should equal """
fun () -> ()
|> Some
"""
