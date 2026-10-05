// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.Infos

type MethInfo =
    | FSMeth of tcGlobals: TcGlobals

    /// Get the information about provided static parameters, if any
    #if NO_EXTENSIONTYPING
    #else
    member ProvidedStaticParameterInfo: (Tainted<ProvidedMethodBase> * Tainted<ProvidedParameterInfo>[]) option
#endif
