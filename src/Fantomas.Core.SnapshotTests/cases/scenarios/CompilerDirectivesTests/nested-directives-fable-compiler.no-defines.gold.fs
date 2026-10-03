namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
    #if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement, fallback: ReactElement) : LazyFunctionComponent<'Props> =
        #if FABLE_COMPILER
        #else
        fun _ -> div [] [] // React.lazy is not compatible with SSR, so just use an empty div
    #endif
    #endif

    static member Foo = ()
