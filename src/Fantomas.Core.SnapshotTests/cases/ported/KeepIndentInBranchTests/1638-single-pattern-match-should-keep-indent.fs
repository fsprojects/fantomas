(*---
max_line_length = 100
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_class_constructor = true
fsharp_space_before_member = true
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_align_function_signature_to_indentation = true
fsharp_alternative_long_member_definitions = true
fsharp_multi_line_lambda_closing_newline = true
fsharp_experimental_keep_indent_in_branch = true
---*)
[<NoEquality ; NoComparison>]
type Foo<'context, 'a> =
    | Apply of ApplyCrate<'context, 'a>

and [<CustomEquality ; NoComparison>] Bar<'context, 'a> =
    internal {
        Hash : int
        Foo : Foo<'a, 'b>
    }
    member this.InnerEquals<'innerContextLongLongLong, 'd, 'e> (a : Foo<'innerContextLongLongLong, 'd>) (b : Foo<'innerContext, 'd>) (cont : bool -> 'e) : 'e =
        if a.Hash <> b.Hash then cont false
        else
        match a.Foo, b.Foo with
        | Foo.Apply a, Foo.Apply b ->
        a.Apply { new ApplyEval<_, _, _> with
                member __.Eval<'bb> (a : Foo<'innerContextLongLongLong, 'bb -> 'b> * Foo<'innerContextLongLongLong, 'bb>) =
                    let (af, av) = a
                    b.Apply { new ApplyEval<_, _, _> with
                        member __.Eval<'cb> (b : Foo<'innerContextLongLongLong, 'cb -> 'b> * Foo<'innerContextLongLongLong, 'bc>) =
                            let (bf, bv) = b
                            if typeof<'bb> = typeof<'cb> then
                                let bv = unbox<Foo<'innerContextLongLongLong, 'bb>> bv
                                this.InnerEquals av bv (fun inner ->
                                    if inner then
                                        let bv = unbox<Foo<'innerContextLongLongLong, 'bb -> 'b>> bf
                                        this.InnerEquals af bf cont
                                    else cont false
                                )
                            else cont false
                    }
        }
