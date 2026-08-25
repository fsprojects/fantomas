module Fantomas.DaemonCommand

open System
open System.IO.Abstractions
open Serilog
open Fantomas.Daemon

let runDaemonCommand (fs: IFileSystem) (log: ILogger) : int =
    let daemon: FantomasDaemon =
        new FantomasDaemon(
            Console.OpenStandardOutput(),
            Console.OpenStandardInput(),
            {
                FileSystem = fs
                ReadConfiguration = EditorConfig.tryReadConfiguration
                Log = log
            }
        )

    AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> (daemon :> IDisposable).Dispose())

    daemon.WaitForClose.GetAwaiter().GetResult()
    0
