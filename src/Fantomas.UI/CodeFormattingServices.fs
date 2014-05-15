namespace FSharpVSPowerTools.CodeFormatting

open System
open System.Text
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.FSharp.Compiler
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations

[<AllowNullLiteral>]
type CodeFormattingServices(editorOptionsFactory: IEditorOptionsFactoryService, 
                            editorOperationsFactoryService: IEditorOperationsFactoryService,
                            textBufferUndoManagerProvider: ITextBufferUndoManagerProvider, 
                            textDocumentFactoryService: ITextDocumentFactoryService) = 
    member val EditorOptionsFactory = editorOptionsFactory
    member val TextBufferUndoManagerProvider = textBufferUndoManagerProvider
    member val EditorOperationsFactoryService = editorOperationsFactoryService
    member val TextDocumentFactoryService = textDocumentFactoryService

module internal Disposable =
    let create (onDispose: unit -> unit) =
        { new IDisposable with
            member x.Dispose() =
                onDispose() }

module internal Cursor =
    let wait() =
        let currentCursor = System.Windows.Forms.Cursor.Current
        System.Windows.Forms.Cursor.Current <- System.Windows.Forms.Cursors.WaitCursor
        Disposable.create(fun () -> System.Windows.Forms.Cursor.Current <- currentCursor)

module internal TextUtils =
    let getFSharpPos (point: VirtualSnapshotPoint) =
        let containingLine = point.Position.GetContainingLine()
        // F# compiler line numbers start at 1
        let lineNumber = containingLine.LineNumber + 1
        let charIndex = point.Position.Position - containingLine.Start.Position
        Range.mkPos lineNumber charIndex