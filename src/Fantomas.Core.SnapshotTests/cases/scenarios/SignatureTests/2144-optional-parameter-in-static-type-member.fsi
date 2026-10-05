// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Control
    
    [<Sealed>]
    [<CompiledName("FSharpAsync")>]
    type Async =
        static member AwaitEvent: event:IEvent<'Del,'T> * ?cancelAction : (unit -> unit) -> Async<'T> when 'Del : delegate<'T,unit> and 'Del :> System.Delegate 
