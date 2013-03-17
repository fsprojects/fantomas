/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// Minor modification by Don Syme & Jomo Fisher to use null as representation
/// of Empty node.
/// Based on F# version by Robert Pickering
/// Based on ocaml version by Troestler Christophe & Isaac Gouy


[<CompilationRepresentation(CompilationRepresentationFlags
  .UseNullAsTrueValue)>]
type Tree<'T> = 
    | Empty 
    | Node of Tree<'T> * 'T * Tree<'T>

let rec make i d =
    if d = 0 then 
        Node(Empty, i, Empty)
    else
        let i2 = 2 * i
        let d = d - 1
        Node(make (i2 - 1) d, i, make i2 d)

let rec check x = 
    match x with 
    | Empty -> 0 
    | Node(l, i, r) -> i + check l - check r

let rec loopDepths maxDepth minDepth d =
    if d <= maxDepth then
        let niter = 1 <<< (maxDepth - d + minDepth)
        let mutable c = 0
        for i = 1 to niter do 
            c <- c + check (make i d) + check (make (-i) d)
        printf "%i\t trees of depth %i\t check: %i\n" (2 * niter) d c
        loopDepths maxDepth minDepth (d + 2)

[<EntryPoint>]
let main args =
    let minDepth = 4
    let maxDepth =
        let n = if args.Length > 0 then int args.[0] else 10
        max (minDepth + 2) n
    let stretchDepth = maxDepth + 1

    let c = check (make 0 stretchDepth)
    printf "stretch tree of depth %i\t check: %i\n" stretchDepth c
    let longLivedTree = make 0 maxDepth
    loopDepths maxDepth minDepth minDepth
    printf "long lived tree of depth %i\t check: %i\n" 
           maxDepth 
           (check longLivedTree)
    0

