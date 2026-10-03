module MyApp

#if DEBUG
[<Emit("console.log('%c' +  $1, 'color: ' + $0)")>]
let printInColor (color: string) (msg: string) : unit = jsNative

[<Emit("console.log('%c' +  $1, $0)")>]
let printInStyle (style: string) (msg) : unit = jsNative

[<Emit("console.info($0)")>]
let printModel model : unit = jsNative

[<Emit("console.trace()")>]
let printStackTrace () : unit = jsNative
#endif

let e2e value = Props.Data("e2e", value)
