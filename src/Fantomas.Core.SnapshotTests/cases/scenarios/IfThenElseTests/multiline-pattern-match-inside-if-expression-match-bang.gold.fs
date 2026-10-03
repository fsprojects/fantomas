namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath

module Configuration =
    let SaveSchemaDir (s: string) =
        let file, config = ensureFile ()

        let node =
            config.XPathSelectElements("AltCover.Visualizer")
            |> Seq.toList
            |> Seq.head

        if
            match! (node.Attribute(XName.Get "GSettingsSchemaDir"), String.IsNullOrWhiteSpace s) with
            | (null, false) ->
                node.Add(XAttribute(XName.Get "GSettingsSchemaDir", s))
                true
            | (a, false) ->
                a.Value <- s
                true
            | (null, true) -> false
            | (a, true) ->
                a.Remove()
                true
        then
            config.Save file
