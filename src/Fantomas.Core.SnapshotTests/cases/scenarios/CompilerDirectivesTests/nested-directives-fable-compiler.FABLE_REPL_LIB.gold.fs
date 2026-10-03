namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
    #if !FABLE_REPL_LIB
    #if FABLE_COMPILER
    #else
    #endif
    #endif

    static member Foo = ()
