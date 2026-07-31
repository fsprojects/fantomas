---
category: Contributors
categoryindex: 2
index: 19
---
# Chains

A *chain* is one of the few shapes where Fantomas has a real choice to make about line breaks.
This page states the rules it follows and the reasoning behind each one, in plain language and without reference to any internal types.

## Status: a proposal, backed by an implementation

The [F# style guide](https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting) currently says very little about how to lay out a long chain.
The rules described here are meant to fill that gap and to eventually become part of that guide.
This page lives under Contributors for that reason: until the rules are officially adopted, they are a proposal we are testing, not settled guidance to hand to end-users.

As noted in the [Fantomas style guide page](../end-users/StyleGuide.html), the style itself is not decided in the Fantomas repository.
Those conversations happen at [fsharp/fslang-design](https://github.com/fsharp/fslang-design#style-guide), and they go much better when there is something concrete to react to.
A written proposal invites arguments about hypothetical snippets.
A proposal that is already implemented lets everyone run it over a real code base and see what it does to code they care about.

That is the order of work here: implement the rules in Fantomas first, use the implementation to find the awkward cases and settle them, then pitch the result upstream.
So treat this page as the current best answer rather than a settled one.
If you disagree with a rule, the discussion belongs at [fsharp/fslang-design](https://github.com/fsharp/fslang-design#style-guide), and having the implementation in hand is exactly what makes that discussion productive.

## A reminder of how Fantomas works

Fantomas does not edit your text.
Think of it like a word processor: it re-types your entire file from scratch, following its own rules.

When it re-types a piece of code, it asks one question first:

> Does this fit on the current line?

If the answer is yes, it stays on one line and there is nothing more to decide.
Everything below is about what happens when the answer is **no**.

## What counts as a chain

A chain is a starting value followed by a series of steps, where **every step is reached through a dot**.

```fsharp
document.Body.FirstChild.AppendChild(newNode)
```

Here `document` is the starting value, and `.Body`, `.FirstChild` and `.AppendChild(newNode)` are the steps.

The dot is what matters. If there is no dot, there is no step:

```fsharp
xs[i]           // not a chain, there is no dot
f (args)        // not a chain, there is no dot
arr.[i]         // a chain, this indexing syntax does have a dot
```

An expression without any dots is laid out by other rules, not the ones on this page.

## Two decisions, not one

Formatting a chain settles two questions that have nothing to do with each other:

1. **Where do the line breaks go?** A style decision, and the bulk of this page.
2. **May a space sit between a method name and its `(`?** Mostly *not* a style decision, and the shorter of the two, so it is settled first.

## Only the last call may have a space before its parentheses

Fantomas has settings that ask for a space before the parentheses of a call, [`space_before_uppercase_invocation`](../end-users/Configuration.html) and [`space_before_lowercase_invocation`](../end-users/Configuration.html).

In a chain, **those settings apply to the final call only**. Every earlier call stays tight, whatever the settings say:

```fsharp
// both examples with space_before_uppercase_invocation = true

obj.Bar ()          // a call on its own: the setting applies

a.Foo(x).Bar (y)    // in a chain: only the final .Bar takes the space,
                    // the intermediate .Foo stays tight
```

This is not Fantomas being inconsistent. A space in the middle of a chain changes what the code *means*:

```fsharp
a.Foo (x).Bar()   // parsed as a.Foo ((x).Bar())
a.Foo(x).Bar()    // parsed the way you intended
```

The parser reads `(x).Bar()` as a single parenthesised argument handed to `a.Foo`. So for every call except the last one, tightness is a grammar requirement rather than a preference, and no setting can override it.

The same constraint turns up wherever an expression has to stay glued to its neighbour. In `getBuilder().Build()` the receiver keeps its own `()` tight, in `x.Foo()[0]` the indexed call keeps its own `()` tight, and a `?` access does the same. In each case a space there would rebind the parentheses to the wrong thing. (The *final* call is still free to take a space: with the setting on, that first example formats as `getBuilder().Build ()`.)

There is one place where even the last call stays tight: the body of a `_.` shorthand lambda, covered in the final section of this page.

Everything from here on is about the first question, where the line breaks go.

## Two kinds of step

Once Fantomas has to break a chain, it sorts the steps into two weights.

**Navigation** is a step that just gets you somewhere:

```fsharp
.Name           // a plain member
.[0]            // a short index
.Cast<T>        // short type arguments
```

**Action** is a step where something happens, meaning a call:

```fsharp
.Foo(x)
.Bar()
.Cast<T>()      // a generic call is still a call
```

A call is always an action, no matter how short it is.
Type arguments make no difference to this: what makes `.Cast<T>()` an action is the `()`, not the `<T>`.

That is the one pair worth keeping straight:

```fsharp
.Cast<T>        // navigation, this only names something
.Cast<T>()      // action, this calls something
```

An index or a set of type arguments is navigation while it stays on one line, and becomes an action if its contents grow big enough to need several lines.

## The rule for line breaks

```text
Does the whole chain fit on one line?
├── Yes  Leave it on one line.
└── No   Is there exactly one action, and is it the last step?
         ├── Yes  Keep everything up to the method name together,
         │        and break the call's ARGUMENTS.
         └── No   Give each action its own line, led by its dot.
```

In one sentence:

> A single `receiver.Method(args)` breaks its arguments, like any other call.
> As soon as there are two or more calls, the chain is a pipeline and each call gets its own line.

Navigation is never worth a line of its own. It rides at the front of the line belonging to the action it introduces.

## Examples

The examples below assume a narrower [max line length](../end-users/Configuration.html) than the default, so the breaks are visible on this page.

### One call at the end: the arguments break

```fsharp
config.GetConnectionString(
    "primary-database-readonly-replica-connection-string"
)
```

There is a single action and it is the last step, so `config.GetConnectionString(` stays together and only the argument moves.
This is exactly how an ordinary call breaks. Having a receiver in front of it changes nothing.

### Navigation in front of a single call: still just the arguments

```fsharp
response.Content.Headers.GetValues(
    "Content-Type-And-Transfer-Encoding-Header"
)
```

`.Content` and `.Headers` are navigation, so there is still only one action.
The chain is not a pipeline and the navigation stays with the receiver.

### Two or more calls: a pipeline

```fsharp
serviceCollection
    .AddSingleton<IClock>(systemClock)
    .AddOptions<MyOptions>(configureOptions)
```

Two actions, so each one gets its own line led by its dot.

### Navigation between calls rides along

```fsharp
document.Body.FirstChild
    .AppendChild(newNode)
    .ParentElement.RemoveChild(oldNode)
```

`.Body` and `.FirstChild` lead the first line.
`.ParentElement` is navigation introducing `.RemoveChild(oldNode)`, so it rides at the front of that line instead of claiming one of its own.

### A chain that ends in navigation

```fsharp
lookupTable
    .GetBucketForHash(hashOfTheKeyValue)
    .Entries.[indexWithinTheBucket]
```

There is only one call here, but it is **not** the last step.
Breaking only the arguments would strand `.Entries.[indexWithinTheBucket]`, so the pipeline layout is used instead.

### A chain with no calls at all

```fsharp
this.Configuration.Database.PrimaryConnection.Settings
    .IdleTimeoutInSeconds
```

With no actions to lead any lines, the steps simply fill the line and break before a dot when the line is full.

### A receiver that is itself a call

```fsharp
getConfiguredServiceBuilder()
    .AddLogging(loggingOptions)
    .Build()
```

The opening call stays glued to its `()` and acts as the starting value.
It has to stay glued: a space there would change what the code means.

## Arguments are not the chain's business

It is worth stating the boundary explicitly, because it is what keeps the rules above so short:

> The chain rules decide where lines break **between** steps.
> They say nothing about what happens **inside** a call's parentheses.

Everything between `(` and `)` is laid out by the ordinary rules for call arguments, exactly as it would be if that call had no receiver in front of it.
A chain never overrides them.
That is the same idea as "break the call's arguments" in the rule above: at that point Fantomas stops making chain decisions and hands the argument to the normal machinery.

The practical consequence is that **every setting that governs argument layout keeps working unchanged inside a chain**.
The one you are most likely to notice is [`multi_line_lambda_closing_newline`](../end-users/Configuration.html), which decides where the closing `)` lands when a lambda argument needs several lines.

With the default (`false`), the `)` trails the last line of the lambda:

```fsharp
storage.SetConfigurationSettingPublisher(fun configName publisher ->
    publish configName publisher)
```

With `true`, it drops to its own line:

```fsharp
storage.SetConfigurationSettingPublisher(fun configName publisher ->
    publish configName publisher
)
```

Because the setting belongs to the argument and not to the chain, it is honoured wherever the call sits.
Above it was the single trailing call. Here it is three calls inside a pipeline, with the same `true` setting:

```fsharp
builder
    .FirstThing<X>(fun lambda ->
        processFirst lambda
    )
    .SecondThing<Y>(fun next ->
        processSecond next
    )
    .ThirdThing<Z>()
    .Result
```

For the same reason, each call decides independently whether it needs several lines.
A long call does not drag the short ones open:

```fsharp
repo
    .Where(fun customer ->
        customer.IsActive && customer.Region = targetRegion)
    .Select(projector)
    .ToList()
```

### The one place position does matter

Match lambdas, written `(function`, are the single shape where a call's position in the chain changes the layout.

`(function` is kept together only when all of the following hold: the call is the **last** step, `multi_line_lambda_closing_newline` is `false`, and you did not already break after the `(` yourself.

```fsharp
builder
    .Build()
    .Configure(function
        | Some v -> handleSome v
        | None -> handleNone ())
```

If any one of those is not true, `function` moves down to its own line and the closing `)` goes to its own line too:

```fsharp
builder
    .Configure(
        function
        | Some v -> handleSome v
        | None -> handleNone ()
    )
    .Build()
    .Result
```

The reason position is involved at all: mid-pipeline, the clauses would run on underneath a line that still continues with further steps, which reads poorly.
So this is a readability judgement rather than a rule forced by the language.

It is also a good illustration of the boundary above rather than an exception to it.
`multi_line_lambda_closing_newline` is still being respected here; it simply has a visible effect on the opener as well as on the closing `)` for this particular argument shape.
And the `(` itself never leaves `.Configure`, for the parsing reason given at the end of this page.

## The `_.` shorthand lambda

F# lets you write `_.Property` as a short lambda. Fantomas treats it as a chain whose starting value is `_`:

```fsharp
"yow" |> _.Substring(0, 16).ToLower()
```

There is one thing that makes it special, and it concerns [the space before a call](#only-the-last-call-may-have-a-space-before-its-parentheses).
Everywhere else the *last* call of a chain may take a space when a setting asks for one. Here it may not: the F# compiler requires the body of a `_.` lambda to stay atomic, so `ToLower ()` would not compile.
Fantomas therefore keeps it tight even with [`space_before_uppercase_invocation`](../end-users/Configuration.html) enabled. This was [issue 3364](https://github.com/fsprojects/fantomas/issues/3364).

Apart from that, it follows the same rule as any other chain.
The example above has two calls, so if it does not fit, it becomes a pipeline:

```fsharp
_
    .Substring(0, 16)
    .ToLower()
```

<fantomas-nav previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>
