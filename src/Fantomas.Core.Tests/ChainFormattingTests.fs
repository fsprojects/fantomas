module Fantomas.Core.Tests.ChainFormattingTests

// This file encodes the specification in `chain-formatting-rationale.md`.
// Each test mirrors one before/after example from that document, at a narrow
// page width so the intended line breaks are visible. Where the spec shows a
// setting explicitly, the test uses it (default vs. MultiLineLambdaClosingNewline).
//
// These tests are the north star for the chain redesign: they describe the
// output the design must produce.

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

// ── A single call at the end — break the arguments ──────────────────────────

[<Test>]
let ``single call at the end breaks the arguments`` () =
    formatSourceString
        """
config.GetConnectionString("primary-database-readonly-replica-connection-string")
"""
        { config with MaxLineLength = 70 }
    |> prepend newline
    |> should
        equal
        """
config.GetConnectionString(
    "primary-database-readonly-replica-connection-string"
)
"""

// ── Navigation before a single call — still break the arguments ─────────────

[<Test>]
let ``navigation before a single call still breaks the arguments`` () =
    formatSourceString
        """
response.Content.Headers.GetValues("Content-Type-And-Transfer-Encoding-Header")
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
response.Content.Headers.GetValues(
    "Content-Type-And-Transfer-Encoding-Header"
)
"""

// ── A single call whose argument is a lambda ────────────────────────────────

[<Test>]
let ``single trailing lambda call, closing newline false`` () =
    formatSourceString
        """
storage.SetConfigurationSettingPublisher(fun configName publisher -> publish configName publisher)
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
storage.SetConfigurationSettingPublisher(fun configName publisher ->
    publish configName publisher)
"""

[<Test>]
let ``single trailing lambda call, closing newline true`` () =
    formatSourceString
        """
storage.SetConfigurationSettingPublisher(fun configName publisher -> publish configName publisher)
"""
        { config with
            MaxLineLength = 80
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
storage.SetConfigurationSettingPublisher(fun configName publisher ->
    publish configName publisher
)
"""

// ── Two or more calls — a pipeline ──────────────────────────────────────────

[<Test>]
let ``two or more calls form a pipeline`` () =
    formatSourceString
        """
serviceCollection.AddSingleton<IClock>(systemClock).AddOptions<MyOptions>(configureOptions)
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
serviceCollection
    .AddSingleton<IClock>(systemClock)
    .AddOptions<MyOptions>(configureOptions)
"""

// ── Navigation between calls rides at the front of the line ─────────────────

[<Test>]
let ``navigation between calls rides at the front of the line`` () =
    formatSourceString
        """
document.Body.FirstChild.AppendChild(newNode).ParentElement.RemoveChild(oldNode)
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
document.Body.FirstChild
    .AppendChild(newNode)
    .ParentElement.RemoveChild(oldNode)
"""

// ── A long pipeline with lambdas — MultiLineLambdaClosingNewline = true ──────

[<Test>]
let ``long pipeline with lambdas, closing newline true`` () =
    formatSourceString
        """
builder.FirstThing<X>(fun lambda -> processFirst lambda).SecondThing<Y>(fun next -> processSecond next).ThirdThing<Z>().Result
"""
        { config with
            MaxLineLength = 40
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
builder
    .FirstThing<X>(fun lambda ->
        processFirst lambda
    )
    .SecondThing<Y>(fun next ->
        processSecond next
    )
    .ThirdThing<Z>()
    .Result
"""

// ── A chain that ends in navigation, not a call ─────────────────────────────

[<Test>]
let ``chain that ends in navigation, not a call`` () =
    formatSourceString
        """
lookupTable.GetBucketForHash(hashOfTheKeyValue).Entries.[indexWithinTheBucket]
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
lookupTable
    .GetBucketForHash(hashOfTheKeyValue)
    .Entries.[indexWithinTheBucket]
"""

// ── A chain with no calls at all ────────────────────────────────────────────

[<Test>]
let ``chain with no calls at all fills greedily`` () =
    formatSourceString
        """
this.Configuration.Database.PrimaryConnection.Settings.IdleTimeoutInSeconds
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
this.Configuration.Database.PrimaryConnection.Settings
    .IdleTimeoutInSeconds
"""

// ── A dot-lambda body (_.…) ─────────────────────────────────────────────────

[<Test>]
let ``dot-lambda body stays tight and short`` () =
    formatSourceString
        """
"yow" |> _.Substring(0, 16).ToLower()
"""
        config
    |> prepend newline
    |> should
        equal
        """
"yow" |> _.Substring(0, 16).ToLower()
"""

[<Test>]
let ``dot-lambda body too long follows leading-dot`` () =
    formatSourceString
        """
_.Substring(0, 16).ToLower()
"""
        { config with MaxLineLength = 25 }
    |> prepend newline
    |> should
        equal
        """
_
    .Substring(0, 16)
    .ToLower()
"""

// ── Exotic and combined shapes ──────────────────────────────────────────────

// A pipeline where one call is multiline and the others are not.

[<Test>]
let ``pipeline with one multiline call, closing newline false`` () =
    formatSourceString
        """
repo.Where(fun customer -> customer.IsActive && customer.Region = targetRegion).Select(projector).ToList()
"""
        { config with MaxLineLength = 70 }
    |> prepend newline
    |> should
        equal
        """
repo
    .Where(fun customer ->
        customer.IsActive && customer.Region = targetRegion)
    .Select(projector)
    .ToList()
"""

[<Test>]
let ``pipeline with one multiline call, closing newline true`` () =
    formatSourceString
        """
repo.Where(fun customer -> customer.IsActive && customer.Region = targetRegion).Select(projector).ToList()
"""
        { config with
            MaxLineLength = 70
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
repo
    .Where(fun customer ->
        customer.IsActive && customer.Region = targetRegion
    )
    .Select(projector)
    .ToList()
"""

// An intermediate call whose tuple arguments break.

[<Test>]
let ``intermediate call whose tuple arguments break`` () =
    formatSourceString
        """
client.Post(endpointUrl, serializedRequestPayload, requestHeaders).EnsureSuccessStatusCode().Content
"""
        { config with MaxLineLength = 40 }
    |> prepend newline
    |> should
        equal
        """
client
    .Post(
        endpointUrl,
        serializedRequestPayload,
        requestHeaders
    )
    .EnsureSuccessStatusCode()
    .Content
"""

// Several navigation steps between two calls.

[<Test>]
let ``several navigation steps between two calls`` () =
    formatSourceString
        """
store.Items.Active.Filter(predicate).Results.Page.First.Render(renderContext)
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
store.Items.Active
    .Filter(predicate)
    .Results.Page.First.Render(renderContext)
"""

// Generic (type-application) methods.

[<Test>]
let ``generic type-application methods`` () =
    formatSourceString
        """
query.OfType<Customer>().Where(activePredicate).Cast<IEntityWithTimestamp>()
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
query
    .OfType<Customer>()
    .Where(activePredicate)
    .Cast<IEntityWithTimestamp>()
"""

// A chain whose receiver is itself a call.

[<Test>]
let ``chain whose receiver is itself a call`` () =
    formatSourceString
        """
getConfiguredServiceBuilder().AddLogging(loggingOptions).Build()
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
getConfiguredServiceBuilder()
    .AddLogging(loggingOptions)
    .Build()
"""

// An index between two calls.

[<Test>]
let ``index between two calls`` () =
    formatSourceString
        """
spreadsheet.GetRow(rowIndex).[targetColumnIndex].FormatWith(cultureInfo)
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
spreadsheet
    .GetRow(rowIndex)
    .[targetColumnIndex].FormatWith(cultureInfo)
"""

// ── The receiver has a vote ─────────────────────────────────────────────────
//
// Keeping a chain together is only offered when the receiver is a plain value.
// A compound receiver leads a pipeline even when there is a single trailing call.

[<Test>]
let ``plain value receiver with a single trailing call breaks the arguments`` () =
    formatSourceString
        """
config.GetConnectionString(primaryDatabaseReadonlyReplica)
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
config.GetConnectionString(
    primaryDatabaseReadonlyReplica
)
"""

[<Test>]
let ``call receiver with a single trailing call leads a pipeline`` () =
    formatSourceString
        """
getBuilder().GetConnectionString(primaryDatabaseReplica)
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
getBuilder()
    .GetConnectionString(primaryDatabaseReplica)
"""

[<Test>]
let ``parenthesised receiver with a single trailing call leads a pipeline`` () =
    formatSourceString
        """
(thing :> IProvider).GetConnectionString(primaryDbReplica)
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
(thing :> IProvider)
    .GetConnectionString(primaryDbReplica)
"""

[<Test>]
let ``generic receiver with a single trailing call leads a pipeline`` () =
    formatSourceString
        """
Animal<Identifier>.GetConnectionString(primaryDbReplica)
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
Animal<Identifier>
    .GetConnectionString(primaryDbReplica)
"""

// ── Exception: a parenthesised value that is only indexed ───────────────────

[<Test>]
let ``multiline parenthesised receiver keeps a lone dot-index welded`` () =
    formatSourceString
        """
let first =
    (line.Split([| ":" |], StringSplitOptions.RemoveEmptyEntries)).[0]
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
let first =
    (line.Split(
        [| ":" |],
        StringSplitOptions.RemoveEmptyEntries
    )).[0]
"""

[<Test>]
let ``multiline parenthesised receiver followed by a member does break`` () =
    formatSourceString
        """
let first =
    (line.Split([| ":" |], StringSplitOptions.RemoveEmptyEntries)).Length
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
let first =
    (line.Split(
        [| ":" |],
        StringSplitOptions.RemoveEmptyEntries
    ))
        .Length
"""

// ── A generic call is still a call ──────────────────────────────────────────

[<Test>]
let ``generic call is an action, so a lone trailing one breaks its arguments`` () =
    formatSourceString
        """
repository.Cast<IEntityWithTimestampAndAudit>(someArgument)
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
repository.Cast<IEntityWithTimestampAndAudit>(
    someArgument
)
"""

[<Test>]
let ``bare generic member is navigation and rides with the receiver`` () =
    formatSourceString
        """
repository.Cast<IEntity>.GetConnectionString(primaryReplica)
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
repository.Cast<IEntity>.GetConnectionString(
    primaryReplica
)
"""
