module Fantomas.Core.Tests.ChainFormattingTests

// This file encodes the specification in `docs/docs/end-users/Chains.md`.
// Most tests mirror a before/after example from that document, at a narrow
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
            MultiLineLambdaClosingNewline = true
        }
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
            MultiLineLambdaClosingNewline = true
        }
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
let ``chain with no calls at all is balanced across its lines`` () =
    formatSourceString
        """
this.Configuration.Database.PrimaryConnection.Settings.IdleTimeoutInSeconds
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
this.Configuration.Database.PrimaryConnection
    .Settings.IdleTimeoutInSeconds
"""

// ── A single trailing action is not enough on its own ───────────────────────
//
// Two further conditions guard the "break the arguments" branch: the starting value
// must be a plain value, and everything up to the method name must fit on one line.

[<Test>]
let ``a plain starting value keeps the chain together`` () =
    formatSourceString
        """
config.Settings.GetValue(theConfigurationKeyNameThatIsRatherLong)
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
config.Settings.GetValue(
    theConfigurationKeyNameThatIsRatherLong
)
"""

[<Test>]
let ``a starting value that is a call leads a pipeline, even with one action`` () =
    // Same steps and the same single trailing call as the test above; only the starting
    // value differs, and that alone decides the layout.
    formatSourceString
        """
getConfiguration().Settings.GetValue(theConfigurationKeyNameThatIsRatherLong)
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
getConfiguration()
    .Settings.GetValue(
        theConfigurationKeyNameThatIsRatherLong
    )
"""

[<Test>]
let ``a comment between the steps leaves nothing to keep together`` () =
    formatSourceString
        """
config.Settings
    // the primary one
    .GetValue(theKeyName)
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
config.Settings
    // the primary one
    .GetValue(theKeyName)
"""

// ── When a line is still too long ───────────────────────────────────────────
//
// A run of navigation that does not fit wraps before a dot, chosen so the longest
// resulting line is as short as possible. Greedy filling would leave one line packed
// to the margin and a stub behind it.

[<Test>]
let ``a long navigation run is balanced rather than filled greedily`` () =
    formatSourceString
        """
getConfiguration().Configuration.Database.PrimaryConnection.Settings.Timeouts.IdleTimeout.Duration.Total.Seconds.Value
"""
        { config with MaxLineLength = 100 }
    |> prepend newline
    |> should
        equal
        """
getConfiguration()
    .Configuration.Database.PrimaryConnection.Settings
    .Timeouts.IdleTimeout.Duration.Total.Seconds.Value
"""

[<Test>]
let ``when two splits tie, the longer first line wins`` () =
    formatSourceString
        """
builder.Connect(hostName).Configuration.Database.PrimaryConnection.Settings.Timeouts.Idle
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
builder
    .Connect(hostName)
    .Configuration.Database.PrimaryConnection
    .Settings.Timeouts.Idle
"""

[<Test>]
let ``the receiver keeps a step rather than sitting alone`` () =
    formatSourceString
        """
defineCombinationValue.Value.IsEmpty
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
defineCombinationValue.Value
    .IsEmpty
"""

[<Test>]
let ``navigation wraps to keep an intermediate call whole`` () =
    // The run leads a call in the middle of a pipeline. Wrapping the navigation is preferred,
    // so `spec` is never pushed onto a line of its own to make room for it.
    formatSourceString
        """
builder.Connect(hostName).Configuration.Database.PrimaryConnection.Settings.Apply(spec).Build()
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
builder
    .Connect(hostName)
    .Configuration.Database
    .PrimaryConnection.Settings.Apply(spec)
    .Build()
"""

[<Test>]
let ``navigation wraps to keep the terminal call whole`` () =
    // Wrapping one step earlier than strictly needed keeps `keyName` beside its method.
    formatSourceString
        """
getConfiguration().Configuration.Database.PrimaryConnection.Settings.Timeouts.GetValue(keyName)
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
getConfiguration()
    .Configuration.Database.PrimaryConnection
    .Settings.Timeouts.GetValue(keyName)
"""

[<Test>]
let ``arguments still break when no wrap can hold the whole call`` () =
    formatSourceString
        """
getConfiguration().Configuration.Database.Settings.GetValue(theConfigurationKeyNameThatIsVeryVeryLongIndeed)
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
getConfiguration()
    .Configuration.Database.Settings.GetValue(
        theConfigurationKeyNameThatIsVeryVeryLongIndeed
    )
"""

[<Test>]
let ``a call leaving the receiver's line has its own line to fit in`` () =
    // The navigation fits on the receiver's line, so the call leaves that line and claims one
    // of its own. There is nothing for the navigation to make room for, and it stays put.
    formatSourceString
        """
Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<option<option<unit>>>.GetGenericTypeDefinition().MakeGenericType(t)).Assembly
"""
        config
    |> prepend newline
    |> should
        equal
        """
Microsoft.FSharp.Reflection.FSharpType
    .GetUnionCases(typeof<option<option<unit>>>.GetGenericTypeDefinition().MakeGenericType(t))
    .Assembly
"""

[<Test>]
let ``balancing never crosses an action`` () =
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

[<Test>]
let ``a step carrying a comment opens its own line and the rest is balanced from there`` () =
    // A commented dot renders on lines of its own, so it has no width to balance with.
    // The run stops there and resumes afterwards, measured from where the comment left off.
    formatSourceString
        """
myConfiguration
    // pick the primary
    .Database.PrimaryConnection.Settings.IdleTimeoutInSeconds
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
myConfiguration
    // pick the primary
    .Database.PrimaryConnection
    .Settings.IdleTimeoutInSeconds
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
            MultiLineLambdaClosingNewline = true
        }
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

// ── Lambda arguments do not depend on the call's position ───────────────────
//
// `fun` and `function` are laid out the same way wherever their call sits, as the F#
// style guide asks ("Treat match lambda's in a similar fashion"). The eight tests below
// are the full matrix: both lambda forms, both positions, both settings.

[<Test>]
let ``fun lambda mid-pipeline keeps its opener attached`` () =
    formatSourceString
        """
builder.Configure(fun v -> handleSomeValue v |> andThenSomethingElse v).Build().Result
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
builder
    .Configure(fun v ->
        handleSomeValue v |> andThenSomethingElse v)
    .Build()
    .Result
"""

[<Test>]
let ``fun lambda as the last step keeps its opener attached`` () =
    formatSourceString
        """
builder.Build().Configure(fun v -> handleSomeValue v |> andThenSomethingElse v)
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
builder
    .Build()
    .Configure(fun v ->
        handleSomeValue v |> andThenSomethingElse v)
"""

[<Test>]
let ``match lambda mid-pipeline keeps its opener attached`` () =
    formatSourceString
        """
builder.Configure(function Some v -> handleSome v | None -> handleNone ()).Build().Result
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
builder
    .Configure(function
        | Some v -> handleSome v
        | None -> handleNone ())
    .Build()
    .Result
"""

[<Test>]
let ``match lambda as the last step keeps its opener attached`` () =
    formatSourceString
        """
builder.Build().Configure(function Some v -> handleSome v | None -> handleNone ())
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
builder
    .Build()
    .Configure(function
        | Some v -> handleSome v
        | None -> handleNone ())
"""

[<Test>]
let ``fun lambda mid-pipeline, closing newline true`` () =
    formatSourceString
        """
builder.Configure(fun v -> handleSomeValue v |> andThenSomethingElse v).Build().Result
"""
        { config with
            MaxLineLength = 60
            MultiLineLambdaClosingNewline = true
        }
    |> prepend newline
    |> should
        equal
        """
builder
    .Configure(fun v ->
        handleSomeValue v |> andThenSomethingElse v
    )
    .Build()
    .Result
"""

[<Test>]
let ``fun lambda as the last step, closing newline true`` () =
    formatSourceString
        """
builder.Build().Configure(fun v -> handleSomeValue v |> andThenSomethingElse v)
"""
        { config with
            MaxLineLength = 60
            MultiLineLambdaClosingNewline = true
        }
    |> prepend newline
    |> should
        equal
        """
builder
    .Build()
    .Configure(fun v ->
        handleSomeValue v |> andThenSomethingElse v
    )
"""

[<Test>]
let ``match lambda mid-pipeline, closing newline true`` () =
    formatSourceString
        """
builder.Configure(function Some v -> handleSome v | None -> handleNone ()).Build().Result
"""
        { config with
            MaxLineLength = 60
            MultiLineLambdaClosingNewline = true
        }
    |> prepend newline
    |> should
        equal
        """
builder
    .Configure(
        function
        | Some v -> handleSome v
        | None -> handleNone ()
    )
    .Build()
    .Result
"""

[<Test>]
let ``match lambda as the last step, closing newline true`` () =
    formatSourceString
        """
builder.Build().Configure(function Some v -> handleSome v | None -> handleNone ())
"""
        { config with
            MaxLineLength = 60
            MultiLineLambdaClosingNewline = true
        }
    |> prepend newline
    |> should
        equal
        """
builder
    .Build()
    .Configure(
        function
        | Some v -> handleSome v
        | None -> handleNone ()
    )
"""

[<Test>]
let ``match lambda as the last step, function written below the opening paren`` () =
    // The setting is the only thing that moves `function` onto its own line. The same call
    // written across more lines is still the same call, so it formats the same way.
    formatSourceString
        """
builder.Build().Configure(
    function
    | Some v -> handleSome v
    | None -> handleNone ())
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
builder
    .Build()
    .Configure(function
        | Some v -> handleSome v
        | None -> handleNone ())
"""

// ── A lambda argument that no longer fits ───────────────────────────────────
//
// Where the lambda goes is the argument's business, not the chain's, so a call reached through
// a dot is laid out exactly like the same call without one. The F# style guide asks for
// everything up to the arrow on one line, and rejects parameters aligned under the opening
// parenthesis, because that column depends on the length of the name in front of it.

[<Test>]
let ``lambda moves to its own line when everything up to the arrow does not fit`` () =
    formatSourceString
        """
let dotted ifaces =
    ifaces
    |> List.tryPick (fun (SynInterfaceImpl(interfaceTy = ty; withKeyword = withRange)) -> Some(ty, withRange))

let undotted ifaces =
    ifaces
    |> pickFromList (fun (SynInterfaceImpl(interfaceTy = ty; withKeyword = withRange)) -> Some(ty, withRange))
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let dotted ifaces =
    ifaces
    |> List.tryPick
        (fun (SynInterfaceImpl(interfaceTy = ty; withKeyword = withRange)) ->
            Some(ty, withRange))

let undotted ifaces =
    ifaces
    |> pickFromList
        (fun (SynInterfaceImpl(interfaceTy = ty; withKeyword = withRange)) ->
            Some(ty, withRange))
"""

[<Test>]
let ``lambda parameters take a line each when they do not fit after the lambda moved down`` () =
    formatSourceString
        """
let dotted () =
    Cfg.register (fun (aVeryLongParameterName: AnEquallyLongTypeName) (anotherLongParameterName: AnotherTypeName) -> body ())

let undotted () =
    registerWith (fun (aVeryLongParameterName: AnEquallyLongTypeName) (anotherLongParameterName: AnotherTypeName) -> body ())
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let dotted () =
    Cfg.register
        (fun
            (aVeryLongParameterName: AnEquallyLongTypeName)
            (anotherLongParameterName: AnotherTypeName) -> body ())

let undotted () =
    registerWith
        (fun
            (aVeryLongParameterName: AnEquallyLongTypeName)
            (anotherLongParameterName: AnotherTypeName) -> body ())
"""

[<Test>]
let ``lambda that still does not fit after moving down keeps its closing parenthesis on the body`` () =
    formatSourceString
        """
let dotted () =
    storage.SetConfigurationSettingPublisher(fun configName publisher -> publishTheConfigurationValue configName publisher andThenSomethingElse)

let undotted () =
    storageSetConfigurationSettingPublisher (fun configName publisher -> publishTheConfigurationValue configName publisher andThenSomethingElse)
"""
        { config with MaxLineLength = 70 }
    |> prepend newline
    |> should
        equal
        """
let dotted () =
    storage.SetConfigurationSettingPublisher
        (fun configName publisher ->
            publishTheConfigurationValue
                configName
                publisher
                andThenSomethingElse)

let undotted () =
    storageSetConfigurationSettingPublisher
        (fun configName publisher ->
            publishTheConfigurationValue
                configName
                publisher
                andThenSomethingElse)
"""

[<Test>]
let ``lambda that fits on one line after moving down keeps its closing parenthesis`` () =
    formatSourceString
        """
let dotted () =
    storage.SetConfigurationSettingPublisher(fun configName publisher -> publish configName publisher)

let undotted () =
    storageSetConfigurationSettingPublisher (fun configName publisher -> publish configName publisher)
"""
        { config with MaxLineLength = 70 }
    |> prepend newline
    |> should
        equal
        """
let dotted () =
    storage.SetConfigurationSettingPublisher
        (fun configName publisher -> publish configName publisher)

let undotted () =
    storageSetConfigurationSettingPublisher
        (fun configName publisher -> publish configName publisher)
"""

// ── A lambda argument to a call that is not the last step ───────────────────
//
// The call above is the last step of its chain, which is what lets the `(` move down with the
// lambda. A call with a step behind it cannot do that: the gap makes `a.Foo (x).Bar()`, which
// passes `(x).Bar()` to `Foo` instead of calling `Bar` on the result. So the `(` stays against
// the member name and the break happens behind it.
//
// Hanging the parameters under `(fun` is the other way to keep the `(` where it is, and the
// style guide rules it out: the column would be the length of the member name. The shape the
// guide asks for instead, parameters indented one level, is not valid F# below a `(` that sits
// mid-line.

[<Test>]
let ``lambda argument to an intermediate call keeps its opening parenthesis, 3432`` () =
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls(fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) -> metadata.Add(key, value))
        .Create()
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls(
            fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) ->
                metadata.Add(key, value))
        .Create()
"""

[<Test>]
let ``lambda argument to an intermediate call, member behind it`` () =
    // A member rather than a call behind the lambda reparses without a diagnostic: the
    // `.Value` would silently become part of the argument.
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls(fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) -> metadata.Add(key, value))
        .Value
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls(
            fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) ->
                metadata.Add(key, value))
        .Value
"""

[<Test>]
let ``lambda argument to an intermediate call, closing newline true`` () =
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls(fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) -> metadata.Add(key, value))
        .Create()
"""
        { config with
            MaxLineLength = 80
            MultiLineLambdaClosingNewline = true
        }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls(
            fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) ->
                metadata.Add(key, value)
        )
        .Create()
"""

[<Test>]
let ``multiline parameter of a lambda argument to an intermediate call`` () =
    // A single parameter that is multiline by itself moves the lambda down for the same reason
    // an opener that does not fit does, so it arrives at the same rule.
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls(fun { Path = path; Key = key; Value = value; Attempt = attempt } -> metadata.Add(key, value))
        .Create()
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls(
            fun
                {
                    Path = path
                    Key = key
                    Value = value
                    Attempt = attempt
                } -> metadata.Add(key, value))
        .Create()
"""

[<Test>]
let ``multiline parameter of a lambda argument to an intermediate call, closing newline true`` () =
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls(fun { Path = path; Key = key; Value = value; Attempt = attempt } -> metadata.Add(key, value))
        .Create()
"""
        { config with
            MaxLineLength = 80
            MultiLineLambdaClosingNewline = true
        }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls(
            fun
                {
                    Path = path
                    Key = key
                    Value = value
                    Attempt = attempt
                } -> metadata.Add(key, value)
        )
        .Create()
"""

[<Test>]
let ``type arguments on an intermediate call taking a lambda`` () =
    // Type arguments lift the call out of the chain and lengthen the opener, but neither is what
    // decides this: the rule is the same one the plain member above answers to.
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls<StepPath * WellKnownStepMetadata>(fun (path: StepPath) (key: WellKnownStepMetadata) -> metadata.Add key)
        .Create()
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls<StepPath * WellKnownStepMetadata>(
            fun (path: StepPath) (key: WellKnownStepMetadata) ->
                metadata.Add key)
        .Create()
"""

[<Test>]
let ``type arguments on an intermediate call taking a lambda, closing newline true`` () =
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls<StepPath * WellKnownStepMetadata>(fun (path: StepPath) (key: WellKnownStepMetadata) -> metadata.Add key)
        .Create()
"""
        { config with
            MaxLineLength = 80
            MultiLineLambdaClosingNewline = true
        }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Calls<StepPath * WellKnownStepMetadata>(
            fun (path: StepPath) (key: WellKnownStepMetadata) ->
                metadata.Add key
        )
        .Create()
"""

[<Test>]
let ``generic intermediate call taking a lambda, 3432`` () =
    formatSourceString
        """
let instanceMetadata =
            Mock<IInstanceApi>()
                .Calls<StepPath * AttemptNumber * JobNumber * DateTime option * WellKnownStepMetadata * string>(fun
                                                                                                                    (path,
                                                                                                                     _,
                                                                                                                     _,
                                                                                                                     _,
                                                                                                                     key,
                                                                                                                     value) ->
                    path |> shouldEqual (StepPath.Parse "/Foo")
                    metadata.Add (key, value)
                )
                .Create ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let instanceMetadata =
    Mock<IInstanceApi>()
        .Calls<StepPath * AttemptNumber * JobNumber * DateTime option * WellKnownStepMetadata * string>(
            fun (path, _, _, _, key, value) ->
                path |> shouldEqual (StepPath.Parse "/Foo")
                metadata.Add(key, value))
        .Create()
"""

// The counterparts of the four above, with the lambda call as the last step of the chain. There
// the `(` is free to move down with the argument, because nothing follows it to be swallowed, so
// these keep the layout they always had. They are here to mark where the rule above stops.

[<Test>]
let ``lambda argument to the last call keeps moving down`` () =
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls(fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) -> metadata.Add(key, value))
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls
            (fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) ->
                metadata.Add(key, value))
"""

[<Test>]
let ``type arguments on the last call taking a lambda`` () =
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls<StepPath * WellKnownStepMetadata>(fun (path: StepPath) (key: WellKnownStepMetadata) -> metadata.Add key)
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls<StepPath * WellKnownStepMetadata>
            (fun (path: StepPath) (key: WellKnownStepMetadata) ->
                metadata.Add key)
"""

[<Test>]
let ``lambda argument to the last call, closing newline true`` () =
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls(fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) -> metadata.Add(key, value))
"""
        { config with
            MaxLineLength = 80
            MultiLineLambdaClosingNewline = true
        }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls
            (fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) ->
                metadata.Add(key, value)
            )
"""

[<Test>]
let ``type arguments on the last call taking a lambda, closing newline true`` () =
    formatSourceString
        """
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls<StepPath * WellKnownStepMetadata>(fun (path: StepPath) (key: WellKnownStepMetadata) -> metadata.Add key)
"""
        { config with
            MaxLineLength = 80
            MultiLineLambdaClosingNewline = true
        }
    |> prepend newline
    |> should
        equal
        """
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls<StepPath * WellKnownStepMetadata>
            (fun (path: StepPath) (key: WellKnownStepMetadata) ->
                metadata.Add key
            )
"""
