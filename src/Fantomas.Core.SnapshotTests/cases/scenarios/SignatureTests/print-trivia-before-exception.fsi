// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// The configuration of the compiler (TcConfig and TcConfigBuilder)
module internal FSharp.Compiler.CompilerConfig

open System
open Internal.Utilities
open Internal.Utilities.Library
open FSharp.Compiler
open FSharp.Compiler.AbstractIL
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.AbstractIL.ILPdbWriter
open FSharp.Compiler.DependencyManager
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.Features
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
exception FileNameNotResolved of string (*description of searched locations*)  * string * range (*filename*)

exception LoadedSourceNotFoundIgnoring of string * range (*filename*)
