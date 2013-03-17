/// The program is from http://codereview.stackexchange.com/q/20955
namespace Katas
    open System.Linq

    module NaturalSortKata =
        exception InvalidException of string

        type Comparison =
        | Equal
        | Lesser
        | Greater
            static member Compare x y =
                if x = y then
                    Equal
                elif x > y then
                    Greater
                else
                    Lesser

        type ChunckType =
        | NumberType
        | StringType
        | Unknown
            static member GetType (c : char) =
                if System.Char.IsDigit(c) then
                    NumberType
                else
                    StringType

            member this.Compare other = 
                match other with
                | ty when ty = this -> Equal
                | Unknown -> Lesser
                | NumberType when this = Unknown -> Greater
                | NumberType -> Lesser
                | StringType -> Greater

        let natualCompare (left : string) (right : string) = 
            if left = right then
                Equal
            else
                let fix str =
                    new System.String( str |> List.rev |> List.toArray )

                let gatherChunck str = 
                    let rec gather str acc =
                        match str with
                        | [] ->
                            let (ty, l) = acc
                            (ty, fix(l))
                        | fistLetter::rest ->
                            match acc with
                            | (ty, _) when ty = Unknown ->
                                let t = ChunckType.GetType(fistLetter)
                                gather rest (t, fistLetter :: [])
                            | (ty, l) when ty = ChunckType.GetType(fistLetter) ->
                                gather rest (ty, fistLetter::l)
                            | (ty, l) -> (ty, fix(l))

                    gather str (Unknown, [])

                let rec compare (left : string) (right : string) =
                    if (not (left.Any())) || (not (right.Any())) then
                        match left.Length, right.Length with
                        | llen, rlen when llen = rlen -> Equal
                        | llen, rlen when llen > rlen -> Greater
                        | llen, rlen when llen < rlen -> Lesser
                        | _ -> raise (InvalidException "Bad Data")
                    else
                        let lt, lChunk = left |> Seq.toList |> gatherChunck 
                        let rt, rChunk = right |> Seq.toList |> gatherChunck

                        match lt.Compare rt with
                        | Equal ->
                            if lChunk = rChunk then
                                let lVal = left.Replace(lChunk, "")
                                let rVal = right.Replace(rChunk, "")

                                compare lVal rVal
                            else
                                match lt with
                                | NumberType ->
                                    Comparison.Compare (System.Int64.Parse(lChunk)) (System.Int64.Parse(rChunk))
                                | _ ->
                                    Comparison.Compare lChunk rChunk
                        | _ ->
                            lt.Compare(rt)

                compare left right