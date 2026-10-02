# Multiline patterns: a walkthrough

Issues: [#2670](https://github.com/fsprojects/fantomas/issues/2670), [#1351](https://github.com/fsprojects/fantomas/issues/1351), [#1303](https://github.com/fsprojects/fantomas/issues/1303), [#1507](https://github.com/fsprojects/fantomas/issues/1507), with [#3446](https://github.com/fsprojects/fantomas/issues/3446) in mind.

This document follows two layouts for a multiline pattern. Both indent the content one level from
the `|` and differ only in where the closing bracket goes:

- **content**: the closer goes on a line of its own, on the content column. This is the direction.
- **trailing**: the closer follows the last item. This is the alternative, with two known problems
  (section 8).

**How to read the code blocks.** Every leading space is drawn as `·`, so columns can be counted. A
line past the page width ends in `◀ over N`. Every block is real output of the prototype, generated
and pasted in by a script, and every block was checked valid and idempotent. Most samples use
`max_line_length = 60` to force the multiline forms.

## 1. What the style guides give us

Neither guide says anything about a multiline pattern. G-Research is silent. Microsoft has two
sentences that set the direction:

> Pattern matching formatting should be consistent with expression formatting.

> These same formatting conventions apply to pattern matching.

And for long arguments in expressions:

> If argument expressions are long, use newlines and indent one level, rather than indenting to the left-parenthesis.

Both layouts follow that last rule: the content goes one level in, never under the opening
parenthesis. Where the closer goes is not covered by the guide for patterns, and section 2 is why it
cannot simply be copied from expressions.

## 2. What the parser forbids

A closing bracket on the column of the `|` ends the clause. This holds for parentheses, lists,
arrays, records and named fields alike, in `match`, `function` and `try/with`:

```fsharp
match x with
| SomeCase(
····a,
····b
) -> body          error FS0010: Incomplete structured construct ... Expected '->'
```

So the expression rule (closer back on the start column) has nowhere valid to put the closer at the
root of a clause. Both layouts avoid the question. **content** puts the closer on the content
column, which is always right of the bar. **trailing** never gives the closer a line of its own.

A second rule: inside brackets, the `|` of an or-pattern on a new line has to sit strictly right of
the first alternative. On the same column it fails. Section 4.6 shows how both layouts handle it.

## 3. The rules

1. **Brackets** (parenthesised tuples, named fields, records, lists, arrays): content on the next
   whole indent level right of the line the opening bracket is on, one item per line. Then the
   closer goes on the content column (**content**) or right after the last item (**trailing**).
2. **Curried arguments**: one per line, one level in, as in a multiline application.
3. **`::` chains**: the operator leads each line, as infix operators do in expressions.
4. **Bare tuples** at the root of a clause keep today's layout, one item per line on the column the
   pattern starts on.
5. **Nested or-patterns**: the alternatives after the first go one level in, the way `&` patterns
   already do.
6. **The arrow counts**: when `pattern ->` does not fit on the line, the root pattern takes its long
   form. That is what #2670 is about.

## 4. Samples

Each sample shows today's output first, then **content**, then **trailing**.

### 4.1 Named fields (#2670)

Today:

```fsharp
match x with
| SynExpr.ObjExpr(
····objType = objType
····argOptions = argOptions
····extraImpls = extraImpls123456780) -> body
| _ -> ()
```

content:

```fsharp
match x with
| SynExpr.ObjExpr(
····objType = objType
····argOptions = argOptions
····extraImpls = extraImpls123456780
····) -> body
| _ -> ()
```

trailing:

```fsharp
match x with
| SynExpr.ObjExpr(
····objType = objType
····argOptions = argOptions
····extraImpls = extraImpls123456780) -> body
| _ -> ()
```

### 4.2 Nesting (#1303)

Today:

```fsharp
let (|IsAnswered|_|) =
····function
····| CaseEvent(caseId,
················timestamp,
················CaseEventType.AnsweredQuestion(questionId,
···············································healthcareProviders)) ->   ◀ over 60
········Some(caseId, timestamp)
····| _ -> None
```

content. Each closer sits on the content column of its own bracket:

```fsharp
let (|IsAnswered|_|) =
····function
····| CaseEvent(
········caseId,
········timestamp,
········CaseEventType.AnsweredQuestion(
············questionId,
············healthcareProviders
············)
········) -> Some(caseId, timestamp)
····| _ -> None
```

trailing:

```fsharp
let (|IsAnswered|_|) =
····function
····| CaseEvent(
········caseId,
········timestamp,
········CaseEventType.AnsweredQuestion(
············questionId,
············healthcareProviders)) -> Some(caseId, timestamp)
····| _ -> None
```

### 4.3 Deep nesting (#1351), default width 120

Today:

```fsharp
match ast with
| Some(ParsedInput.ImplFile(ParsedImplFileInput(
····modules = [ SynModuleOrNamespace.SynModuleOrNamespace(
····················decls = [ SynModuleDecl.Let(bindings = [ SynBinding.Binding(expr = SynExpr.Do(_, keywordRange, _)) ]) ]) ]))) ->   ◀ over 120
····Assert.AreEqual(expectedRangeStart, keywordRange.Start) |> ignore
| _ -> failwith "Could not find SynExpr.Do"
```

content:

```fsharp
match ast with
| Some(ParsedInput.ImplFile(ParsedImplFileInput(
····modules = [
········SynModuleOrNamespace.SynModuleOrNamespace(
············decls = [ SynModuleDecl.Let(bindings = [ SynBinding.Binding(expr = SynExpr.Do(_, keywordRange, _)) ]) ]
············)
········]
····))) -> Assert.AreEqual(expectedRangeStart, keywordRange.Start) |> ignore
| _ -> failwith "Could not find SynExpr.Do"
```

trailing. The page width is exceeded, see section 8:

```fsharp
match ast with
| Some(ParsedInput.ImplFile(ParsedImplFileInput(
····modules = [
········SynModuleOrNamespace.SynModuleOrNamespace(
············decls = [ SynModuleDecl.Let(bindings = [ SynBinding.Binding(expr = SynExpr.Do(_, keywordRange, _)) ]) ])]))) ->   ◀ over 120
····Assert.AreEqual(expectedRangeStart, keywordRange.Start) |> ignore
| _ -> failwith "Could not find SynExpr.Do"
```

### 4.4 Records and `::` (#1507)

Today, name-sensitive and past the page width:

```fsharp
match tokens with
| {
······TokenInfo = {
······················TokenName = "LPAREN"
······················CharClass = FSharpTokenCharKind.Delimiter   ◀ over 60
··················}
··} :: {
···········TokenInfo = {
···························TokenName = "HASH"
···························CharClass = FSharpTokenCharKind.Delimiter   ◀ over 60
·······················}
·······} :: rest -> Some()
| _ -> None
```

content:

```fsharp
match tokens with
| {
····TokenInfo = {
········TokenName = "LPAREN"
········CharClass = FSharpTokenCharKind.Delimiter
········}
····}
··:: {
····TokenInfo = {
········TokenName = "HASH"
········CharClass = FSharpTokenCharKind.Delimiter
········}
····}
··:: rest -> Some()
| _ -> None
```

trailing:

```fsharp
match tokens with
| {
····TokenInfo = {
········TokenName = "LPAREN"
········CharClass = FSharpTokenCharKind.Delimiter}}
··:: {
····TokenInfo = {
········TokenName = "HASH"
········CharClass = FSharpTokenCharKind.Delimiter}}
··:: rest -> Some()
| _ -> None
```

### 4.5 Curried arguments

Today:

```fsharp
match x with
| ActivePatternWithManyArgs firstArgumentPattern secondArgumentPattern thirdArgumentPattern ->   ◀ over 60
····someFunctionCall
········firstArgumentPattern
········secondArgumentPattern
| _ -> ()
```

content and trailing are the same here, as there is no closer:

```fsharp
match x with
| ActivePatternWithManyArgs
····firstArgumentPattern
····secondArgumentPattern
····thirdArgumentPattern ->
····someFunctionCall
········firstArgumentPattern
········secondArgumentPattern
| _ -> ()
```

### 4.6 Or-patterns in a list

Today:

```fsharp
match args with
| [ LongPatIndentifierOne
·····| LongPatIndentifierTwo
·····| LongPatIndentifierThree ] -> args
| _ -> failwith "meh"
```

content:

```fsharp
match args with
| [
····LongPatIndentifierOne
········| LongPatIndentifierTwo
········| LongPatIndentifierThree
····] -> args
| _ -> failwith "meh"
```

trailing:

```fsharp
match args with
| [
····LongPatIndentifierOne
········| LongPatIndentifierTwo
········| LongPatIndentifierThree] -> args
| _ -> failwith "meh"
```

### 4.7 A long body, `when`, and an or-pattern of two multiline cases

Today:

```fsharp
match x with
| SomeVeryLongUnionCaseName(firstArgumentPattern,
····························secondArgumentPattern) ->
····someFunctionCall
········firstArgumentPattern
········secondArgumentPattern
| SomeVeryLongUnionCaseName(firstArgumentPattern,
····························secondArgumentPattern) when
····firstArgumentPattern > secondArgumentPattern
····->
····body
| SomeVeryLongUnionCaseName(firstArgumentPattern,
····························secondArgumentPattern)
| OtherVeryLongUnionCaseName(firstArgumentPattern,
·····························secondArgumentPattern) -> body
| _ -> ()
```

content:

```fsharp
match x with
| SomeVeryLongUnionCaseName(
····firstArgumentPattern,
····secondArgumentPattern
····) ->
····someFunctionCall
········firstArgumentPattern
········secondArgumentPattern
| SomeVeryLongUnionCaseName(
····firstArgumentPattern,
····secondArgumentPattern
····) when firstArgumentPattern > secondArgumentPattern ->
····body
| SomeVeryLongUnionCaseName(
····firstArgumentPattern,
····secondArgumentPattern
····)
| OtherVeryLongUnionCaseName(
····firstArgumentPattern,
····secondArgumentPattern
····) -> body
| _ -> ()
```

trailing:

```fsharp
match x with
| SomeVeryLongUnionCaseName(
····firstArgumentPattern,
····secondArgumentPattern) ->
····someFunctionCall
········firstArgumentPattern
········secondArgumentPattern
| SomeVeryLongUnionCaseName(
····firstArgumentPattern,
····secondArgumentPattern) when
····firstArgumentPattern > secondArgumentPattern
····->
····body
| SomeVeryLongUnionCaseName(
····firstArgumentPattern,
····secondArgumentPattern)
| OtherVeryLongUnionCaseName(
····firstArgumentPattern,
····secondArgumentPattern) -> body
| _ -> ()
```

### 4.8 A comment after an item

Today:

```fsharp
match x with
| SomeVeryLongUnionCaseName(firstArgumentPattern, // the first one   ◀ over 60
····························secondArgumentPattern) -> body
| _ -> ()
```

content:

```fsharp
match x with
| SomeVeryLongUnionCaseName(
····firstArgumentPattern, // the first one
····secondArgumentPattern
····) -> body
| _ -> ()
```

trailing:

```fsharp
match x with
| SomeVeryLongUnionCaseName(
····firstArgumentPattern, // the first one
····secondArgumentPattern) -> body
| _ -> ()
```

### 4.9 Outside a match: let, member, lambda, try/with

The same pattern code serves every place a pattern appears. The `let` here already uses the binding
shape from section 6.

Today:

```fsharp
let (SomeVeryLongUnionCaseName(firstArgumentPattern,
·······························secondArgumentPattern)) =
····x

type T() =
····member this.Foo
········(SomeVeryLongUnionCaseName(firstArgumentPattern,
···································secondArgumentPattern))
········=
········()

xs
|> List.map
····(fun
········(SomeVeryLongUnionCaseName(firstArgumentPattern,
···································secondArgumentPattern)) ->   ◀ over 60
········a)

try
····()
with
| SomeVeryLongExceptionName(firstArgumentPattern,
····························secondArgumentPattern) -> ()
| OtherVeryLongExceptionName(firstArgumentPattern,
·····························secondArgumentPattern) -> ()
```

content:

```fsharp
let
····(SomeVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern
········))
····=
····x

type T() =
····member this.Foo
········(SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern
············))
········=
········()

xs
|> List.map
····(fun
········(SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern
············)) -> a)

try
····()
with
| SomeVeryLongExceptionName(
····firstArgumentPattern,
····secondArgumentPattern
····) -> ()
| OtherVeryLongExceptionName(
····firstArgumentPattern,
····secondArgumentPattern
····) -> ()
```

trailing:

```fsharp
let
····(SomeVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern))
····=
····x

type T() =
····member this.Foo
········(SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern))
········=
········()

xs
|> List.map
····(fun
········(SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern)) -> a)

try
····()
with
| SomeVeryLongExceptionName(
····firstArgumentPattern,
····secondArgumentPattern) -> ()
| OtherVeryLongExceptionName(
····firstArgumentPattern,
····secondArgumentPattern) -> ()
```

### 4.10 A `function` inside an application

Today:

```fsharp
let y =
····foo (function
········| SomeVeryLongUnionCaseName(firstArgumentPattern,
····································secondArgumentPattern,
····································third) -> body
········| _ -> ())
```

content:

```fsharp
let y =
····foo (function
········| SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern,
············third
············) -> body
········| _ -> ())
```

trailing:

```fsharp
let y =
····foo (function
········| SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern,
············third) -> body
········| _ -> ())
```

### 4.11 Indent size 2

Today:

```fsharp
match x with
| SomeVeryLongUnionCaseName(firstArgumentPattern,
····························secondArgumentPattern,
····························thirdArgumentPattern) -> body
| _ -> ()
```

content:

```fsharp
match x with
| SomeVeryLongUnionCaseName(
··firstArgumentPattern,
··secondArgumentPattern,
··thirdArgumentPattern
··) -> body
| _ -> ()
```

trailing:

```fsharp
match x with
| SomeVeryLongUnionCaseName(
··firstArgumentPattern,
··secondArgumentPattern,
··thirdArgumentPattern) -> body
| _ -> ()
```

## 5. MultilineBracketStyle

`Aligned` is the default. This section is about `Cramped` and `Stroustrup`.

### 5.1 What the style changes in expressions

Only records, lists and arrays. Parenthesised arguments (`Case(a, b)`, `Case(field = a)`) look the
same in all three styles. Here are the same expressions in each style.

Cramped:

```fsharp
let a =
····SynExpr.ObjExpr(
········objType = objType,
········argOptions = argOptions,
········extraImpls = extraImpls123456780
····)

let b =
····CaseEvent(
········caseId,
········timestamp,
········CaseEventType.AnsweredQuestion(
············questionId,
············healthcareProviders
········)
····)

let c =
····[ firstElementPatternLong
······secondElementPatternLong
······thirdElementPatternLong ]

let d =
····{ TokenInfo =
········{ TokenName = "LPAREN"
··········CharClass = FSharpTokenCharKind.Delimiter } }
····:: rest

f
····{ TokenInfo =
········{ TokenName = "LPAREN"
··········CharClass = FSharpTokenCharKind.Delimiter } }
```

Aligned:

```fsharp
let a =
····SynExpr.ObjExpr(
········objType = objType,
········argOptions = argOptions,
········extraImpls = extraImpls123456780
····)

let b =
····CaseEvent(
········caseId,
········timestamp,
········CaseEventType.AnsweredQuestion(
············questionId,
············healthcareProviders
········)
····)

let c =
····[
········firstElementPatternLong
········secondElementPatternLong
········thirdElementPatternLong
····]

let d =
····{
········TokenInfo =
············{
················TokenName = "LPAREN"
················CharClass = FSharpTokenCharKind.Delimiter
············}
····}
····:: rest

f
····{
········TokenInfo =
············{
················TokenName = "LPAREN"
················CharClass = FSharpTokenCharKind.Delimiter
············}
····}
```

Stroustrup:

```fsharp
let a =
····SynExpr.ObjExpr(
········objType = objType,
········argOptions = argOptions,
········extraImpls = extraImpls123456780
····)

let b =
····CaseEvent(
········caseId,
········timestamp,
········CaseEventType.AnsweredQuestion(
············questionId,
············healthcareProviders
········)
····)

let c = [
····firstElementPatternLong
····secondElementPatternLong
····thirdElementPatternLong
]

let d =
····{
········TokenInfo = {
············TokenName = "LPAREN"
············CharClass = FSharpTokenCharKind.Delimiter
········}
····}
····:: rest

f {
····TokenInfo = {
········TokenName = "LPAREN"
········CharClass = FSharpTokenCharKind.Delimiter
····}
}
```

The styles differ in two places: whether the opening bracket hugs what is in front of it
(`TokenInfo = {` or `TokenInfo =` with `{` on the next line), and where the closer goes (trailing
in Cramped, on a line of its own in the other two).

### 5.2 A parser rule that takes Aligned off the table for records

A record pattern cannot put a field value on the line after its `=`. A simple name, a
parenthesised pattern and a record all fail, at any column, in `match` and in `let`:

```fsharp
match t with
| { TokenInfo =
············name } -> ()      error: Unexpected start of structured construct in pattern
```

The same break in an expression parses, and so does a named field
(`Case(
····modules =
········[ ... ])`). So for a nested record in a record pattern, the
Aligned expression shape (`TokenInfo =`, then `{` on its own line) is impossible. Every style has to
hug: `TokenInfo = {`.

As a result, **Aligned and Stroustrup produce the same patterns** in the prototype. Both get the
layouts from section 3, and every sample in section 4 is identical in the two styles.

### 5.3 What happens per style

| | parenthesised args, named fields | records | lists, arrays |
|---|---|---|---|
| Aligned | content / trailing | content / trailing | content / trailing |
| Stroustrup | content / trailing | content / trailing | content / trailing |
| Cramped | content / trailing | unchanged, cramped | unchanged, cramped |

Cramped is a trailing layout by nature: `{ A = a` and then `··B = b }`, with the closer after the
last item. The prototype leaves Cramped records and lists as they are. Parentheses follow the
chosen mode in every style, as they do in expressions.

Today, lists and arrays in patterns are cramped in *every* style. The prototype changes that for
Aligned and Stroustrup, so it is a visible change for users of the default style.

### 5.4 Records and `::` (#1507) per style

Stroustrup, today:

```fsharp
match tokens with
| {
······TokenInfo = {
······················TokenName = "LPAREN"
······················CharClass = FSharpTokenCharKind.Delimiter   ◀ over 60
··················}
··} :: {
···········TokenInfo = {
···························TokenName = "HASH"
···························CharClass = FSharpTokenCharKind.Delimiter   ◀ over 60
·······················}
·······} :: rest -> Some()
| _ -> None
```

Stroustrup, content (identical to Aligned):

```fsharp
match tokens with
| {
····TokenInfo = {
········TokenName = "LPAREN"
········CharClass = FSharpTokenCharKind.Delimiter
········}
····}
··:: {
····TokenInfo = {
········TokenName = "HASH"
········CharClass = FSharpTokenCharKind.Delimiter
········}
····}
··:: rest -> Some()
| _ -> None
```

Stroustrup, trailing:

```fsharp
match tokens with
| {
····TokenInfo = {
········TokenName = "LPAREN"
········CharClass = FSharpTokenCharKind.Delimiter}}
··:: {
····TokenInfo = {
········TokenName = "HASH"
········CharClass = FSharpTokenCharKind.Delimiter}}
··:: rest -> Some()
| _ -> None
```

Cramped, today:

```fsharp
match tokens with
| { TokenInfo = { TokenName = "LPAREN"
··················CharClass = FSharpTokenCharKind.Delimiter } } :: { TokenInfo = { TokenName = "HASH"   ◀ over 60
···················································································CharClass = FSharpTokenCharKind.Delimiter } } :: rest ->   ◀ over 60
····Some()
| _ -> None
```

Cramped, content and trailing. These are identical, since only the record layout is involved:

```fsharp
match tokens with
| { TokenInfo = { TokenName = "LPAREN"
··················CharClass = FSharpTokenCharKind.Delimiter } }   ◀ over 60
··:: { TokenInfo = { TokenName = "HASH"
·····················CharClass = FSharpTokenCharKind.Delimiter } }   ◀ over 60
··:: rest -> Some()
| _ -> None
```

### 5.5 Deep nesting (#1351) per style, width 120

Stroustrup, content:

```fsharp
match ast with
| Some(ParsedInput.ImplFile(ParsedImplFileInput(
····modules = [
········SynModuleOrNamespace.SynModuleOrNamespace(
············decls = [ SynModuleDecl.Let(bindings = [ SynBinding.Binding(expr = SynExpr.Do(_, keywordRange, _)) ]) ]
············)
········]
····))) -> Assert.AreEqual(expectedRangeStart, keywordRange.Start) |> ignore
| _ -> failwith "Could not find SynExpr.Do"
```

Stroustrup, trailing:

```fsharp
match ast with
| Some(ParsedInput.ImplFile(ParsedImplFileInput(
····modules = [
········SynModuleOrNamespace.SynModuleOrNamespace(
············decls = [ SynModuleDecl.Let(bindings = [ SynBinding.Binding(expr = SynExpr.Do(_, keywordRange, _)) ]) ])]))) ->   ◀ over 120
····Assert.AreEqual(expectedRangeStart, keywordRange.Start) |> ignore
| _ -> failwith "Could not find SynExpr.Do"
```

Cramped, today:

```fsharp
match ast with
| Some(ParsedInput.ImplFile(ParsedImplFileInput(
····modules = [ SynModuleOrNamespace.SynModuleOrNamespace(
····················decls = [ SynModuleDecl.Let(bindings = [ SynBinding.Binding(expr = SynExpr.Do(_, keywordRange, _)) ]) ]) ]))) ->   ◀ over 120
····Assert.AreEqual(expectedRangeStart, keywordRange.Start) |> ignore
| _ -> failwith "Could not find SynExpr.Do"
```

Cramped, content:

```fsharp
match ast with
| Some(ParsedInput.ImplFile(ParsedImplFileInput(
····modules = [ SynModuleOrNamespace.SynModuleOrNamespace(
····················decls = [ SynModuleDecl.Let(
································bindings = [ SynBinding.Binding(expr = SynExpr.Do(_, keywordRange, _)) ]
································) ]
····················) ]
····))) -> Assert.AreEqual(expectedRangeStart, keywordRange.Start) |> ignore
| _ -> failwith "Could not find SynExpr.Do"
```

Cramped, trailing:

```fsharp
match ast with
| Some(ParsedInput.ImplFile(ParsedImplFileInput(
····modules = [ SynModuleOrNamespace.SynModuleOrNamespace(
····················decls = [ SynModuleDecl.Let(
································bindings = [ SynBinding.Binding(expr = SynExpr.Do(_, keywordRange, _)) ]) ]) ]))) ->
····Assert.AreEqual(expectedRangeStart, keywordRange.Start) |> ignore
| _ -> failwith "Could not find SynExpr.Do"
```

### 5.6 Or-patterns in a list per style

Stroustrup, today (cramped, as in every style):

```fsharp
match args with
| [ LongPatIndentifierOne
·····| LongPatIndentifierTwo
·····| LongPatIndentifierThree ] -> args
| _ -> failwith "meh"
```

Stroustrup, content:

```fsharp
match args with
| [
····LongPatIndentifierOne
········| LongPatIndentifierTwo
········| LongPatIndentifierThree
····] -> args
| _ -> failwith "meh"
```

Cramped, content (unchanged from today):

```fsharp
match args with
| [ LongPatIndentifierOne
·····| LongPatIndentifierTwo
·····| LongPatIndentifierThree ] -> args
| _ -> failwith "meh"
```

### 5.7 What this leaves open

1. **Cramped keeps name-sensitive columns.** A cramped record or list aligns its items after the
   opening bracket. Inside a record pattern the bracket follows `TokenInfo = `, so the column
   depends on the field name. Expressions avoid this by breaking after `=`, which section 5.2 rules
   out for record patterns. Anything nested inside a cramped bracket inherits that column, which is
   where #1351's `bindings` lands on column 32. Under Cramped, #1351 and #1507 still run past the
   page width. #3446 already says Cramped is not worth rescuing: the bracket itself should open on
   a column the indent size explains, and whatever sits inside it is accepted as it is.
2. **Stroustrup expressions close on the line start, content closes on the content column.**
   Compare `TokenInfo = {` … `}` in 5.1 (closer under `TokenInfo`) with 5.4 (closer under
   `TokenName`). In Stroustrup, the content layout is a deliberate departure from the expression
   shape. With trailing, the question does not come up.
3. **Test coverage.** The changed tests are in the Aligned, pattern matching, tuple and chain
   suites. None of the Stroustrup suites has a multiline pattern, and the Cramped pattern tests do
   not change. Both styles need tests of their own once the layout is settled.

## 6. Let bindings

### 6.1 The shape

Today, a long pattern behind `let` hangs off the keyword:

```fsharp
let (SomeVeryLongUnionCaseName(firstArgumentPattern,
·······························secondArgumentPattern)) =
····x
```

With only the pattern layout changed (content), the closers and the `=` pile up on one line:

```fsharp
let (SomeVeryLongUnionCaseName(
····firstArgumentPattern,
····secondArgumentPattern
····)) =
····x
```

The proposal: when `let pattern =` does not fit on the line, the pattern gets a line of its own,
one level in, and the `=` and the body follow on that same column.

### 6.2 What the parser allows

The `=` cannot go back to the column of `let`. That fails in every context I tried: top level,
nested in a function, in a module followed by another declaration, in a class, after `use`, `let!`,
`let rec … and`, `mutable`, an attribute, a type annotation, in a match arm and in `let … in`:

```fsharp
let
····(SomeCase(
········a,
········b
········))
=                  error FS0010: Incomplete structured construct ... in binding
····x
```

With the `=` on the pattern's column, every one of those contexts parses (`let`, `use`, `let!`,
`and!`, `let rec … and`, `mutable`, `inline`, attributes, classes, modules, match arms, `let … in`,
type annotations, record patterns).

This also matches the style guide's long function form, where the `=` goes on a line of its own on
the parameters' column:

```fsharp
····let longFunctionWithLotsOfParameters
········(aVeryLongParam: AVeryLongTypeThatYouNeedToUse)
········(aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse)
········=
········// ... the body of the method follows
```

One context is out of scope: `let private (pattern)` does not parse even on a single line, because
F# reads `private (` as the start of an operator name. Fantomas cannot format it today either.

### 6.3 The rules

1. When `let`, its modifiers, the pattern, the return type and ` =` fit on one line, nothing
   changes.
2. Otherwise the modifiers (`mutable`, `inline`, `rec`, an access modifier) stay behind the
   keyword, and the pattern goes on the next line, one level in.
3. The pattern gets that whole line first. It only takes its long form if it does not fit there,
   and the return type counts towards that, as the arrow does in a match clause.
4. The `=` goes on a line of its own on the pattern's column, then the body follows on the same
   column. A comment after the `=` stays behind it.

### 6.4 Samples

content:

```fsharp
let
····(SomeVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern
········))
····=
····x

let
····(SomeVeryLongUnionCase(
········firstArgumentPattern,
········secondArgument
········))
····=
····someFunctionCall firstArgumentPattern

let
····(
········var1withAVeryLongLongLongName,
········var2withAVeryLongLongLongName
········)
····=
····someFunc 1, someFunc 2

let mutable
····(CaseName(
········firstArgumentPattern,
········secondArgumentPattern,
········third
········))
····=
····x

let
····(SomeVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern,
········thirdArgumentPattern,
········fourthArgumentPattern
········))
····=
····x

let
····{
········FirstFieldName = firstFieldName
········SecondFieldName = secondFieldName
········Third = third
········}
····=
····x

let f () =
····let
········(SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern
············))
········=
········x

····use
········(SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern
············))
········=
········x

····firstArgumentPattern

type T() =
····let
········(SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern
············)): Foo
········=
········x

····member _.A = 1

let rec
····(SomeVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern
········))
····=
····x

and
····(OtherVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern
········))
····=
····y

let
····(SomeVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern
········))
····= // comment
····x
```

trailing:

```fsharp
let
····(SomeVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern))
····=
····x

let
····(SomeVeryLongUnionCase(
········firstArgumentPattern,
········secondArgument))
····=
····someFunctionCall firstArgumentPattern

let
····(
········var1withAVeryLongLongLongName,
········var2withAVeryLongLongLongName)
····=
····someFunc 1, someFunc 2

let mutable
····(CaseName(
········firstArgumentPattern,
········secondArgumentPattern,
········third))
····=
····x

let
····(SomeVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern,
········thirdArgumentPattern,
········fourthArgumentPattern))
····=
····x

let
····{
········FirstFieldName = firstFieldName
········SecondFieldName = secondFieldName
········Third = third}
····=
····x

let f () =
····let
········(SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern))
········=
········x

····use
········(SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern))
········=
········x

····firstArgumentPattern

type T() =
····let
········(SomeVeryLongUnionCaseName(
············firstArgumentPattern,
············secondArgumentPattern)): Foo
········=
········x

····member _.A = 1

let rec
····(SomeVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern))
····=
····x

and
····(OtherVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern))
····=
····y

let
····(SomeVeryLongUnionCaseName(
········firstArgumentPattern,
········secondArgumentPattern))
····= // comment
····x
```

A pattern that fits once it has a line of its own (content, trailing is the same):

```fsharp
let mutable
····(CaseName(firstArgumentPattern, secondArgumentPattern))
····=
····x

let
····(CaseName(
········firstArgumentPattern,
········secondArgumentPattern
········)): SomeType
····=
····x
```

Cramped, content. The record keeps its cramped layout inside the new shape:

```fsharp
let internal sepSemi (ctx: Context) =
····let
········{ Config = { SpaceBeforeSemicolon = before
·····················SpaceAfterSemicolon = after } }
········=
········ctx

····()
```

### 6.5 Decisions

1. **A pattern that fits on a line of its own stays on one line there.** The pattern moves to its
   own line first and only takes its long form when it does not fit there:

   ```fsharp
   let mutable
   ····(CaseName(firstArgumentPattern, secondArgumentPattern))
   ····=
   ····x
   ```

   Breaking the pattern behind the keyword (`let mutable (CaseName(`) is not used.

2. **The body always gets a line of its own after the `=`**, even when it is short. `····= x` is
   not used.

The prototype already does both.

Not prototyped yet: `let!` and `and!` in computation expressions, and `member val`. They go through
other generators. The parser accepts the same shape for `let!` and `and!`.

## 7. How the code gets there

The prototype changes only `src/Fantomas.Core/CodePrinter.fs`. It is saved as
`pattern-layout-experiment/prototype.patch` (section 11.2).

1. **A floor just right of the bar, not a lock.** `genPatInClause` already set `AtColumn`, the
   column a new line cannot go left of, to the pattern column (bar + 2). The prototype moves that
   floor to bar + 1: content only has to clear the bar. With an indent size of 4 nothing changes,
   the first level is bar + 4 either way. With an indent size of 2 the first level becomes
   bar + 2, the pattern's own column, instead of bar + 4 (section 4.11). The calls that go are the
   `atCurrentColumn` and `atCurrentColumnIndent` *inside* `genPat` (tuples, curried arguments,
   named fields, records, lists). Each of them rebases every nested indent onto a column that
   depends on a name.

   Two places keep their own column, because they line things up on purpose: the items of a
   Cramped list (after the opening bracket), and a `::` chain that carries a comment, which keeps
   today's layout under the start of the chain.

2. **One helper for every bracket**, `genPatBracketLong opening items closing`. The content goes
   on the first whole indent level right of the column the opening line starts on
   (`max Indent AtColumn`). In a clause that is the first level right of the bar. Everywhere else
   it is the next level. The column is set with `atIndentLevel`, so it is scoped: nothing after the bracket
   inherits it. Then the closer either joins the items on that column, or trails the last one.

   The scoping is the #3446 part. A first attempt used plain `indent`/`unindent`, and the writer's
   `doNewline` raised `Indent` to `AtColumn` after the first closer. The next record in a `::`
   chain then indented from column 6 instead of 4. That is the "rebases every relative indent below
   it" effect that #3446 describes.

3. **Long forms that mirror expressions**: parenthesised tuples, named fields, lists and arrays
   (`Aligned` and `Stroustrup`; `Cramped` keeps its shape), records, curried arguments, `::`
   chains, nested or-patterns.

4. **The arrow counts.** If `pattern ->` does not fit, the root pattern is forced into its long
   form. Only the root: the children still decide for themselves.

5. **Let bindings.** A bare tuple in a `let` gets parentheses added when it goes multiline. That
   path had its own aligned layout, so the first and the second format disagreed. It now uses the
   same bracket helper.

6. **The binding shape (section 6)** lives in the value branch of `genBinding`. A
   `futureNlnCheck` decides whether the head fits behind the keyword. If it does not, the head is
   written with `indentSepNlnUnindent`: pattern, return type, `=`, body, each on that column.

## 8. Problems specific to trailing

1. **Nested fit (#1351, section 4.3).** Every fit decision inside the pattern measures its own
   content, but not the closers that will trail it (`])]))) ->`). Fixing this means each nested
   decision has to know the width of everything that follows it. Fantomas' short-expression checks
   do not work that way. With **content**, a closer has a line of its own and the problem does not
   arise.
2. **A line comment after the last item (test 2953).** The closer cannot follow a `// comment`.
   The comment moves behind `) ->`, and the second format joins everything onto one line: not
   idempotent. Trailing would need a fallback: a closer on its own line whenever the last item
   carries a line comment.

## 9. Open questions for content

1. **Closer, arrow and body on one column.** When the body does not fit after the arrow, the
   closer and the body start on the same column (section 4.7):
   `····) ->` followed by `····someFunctionCall`. This is the same situation as a multiline
   `when` today, which puts `->` on a line of its own.
2. **Single-argument nesting.** `Some(SynExpr.App(` stays on one line and closes with `))`. The
   expression side would break after `Some(`.
3. **Curried arguments before the arrow (section 4.5).** The pattern ends without a closer, so the
   body lands on the column of the arguments.
4. **Cost of the arrow check.** It measures every clause pattern a second time. It needs a
   benchmark, or a cheaper guard.
5. **`when`.** The arrow check reserves room for ` ->` even when ` when` follows, which is slightly
   too strict.

## 10. Evidence

- 12 samples × 7 configurations (default, width 60, G-Research, indent 2, keep indent in branch,
  Cramped, Stroustrup) are valid and idempotent in both modes. Under Cramped, #1351 and #1507 still
  overflow (section 5.7), but less than they do today. **content** stays within the page width
  except for one overflow that `main` also has (a type application). **trailing** adds the #1351
  overflow, at width 120 and at width 60 with indent 2.
- `Fantomas.Core.Tests`, 3213 tests:
  - **content**: 14 change. All are the new shapes: no invalid code, no idempotency failure, no
    lost trivia. One of them is a Cramped test with a record pattern in a `let` (section 6.4).
  - **trailing**: 15 change: the 14 of content, plus the idempotency failure in section 8.

## 11. Picking this up again

### 11.1 Where things stand

The prototype was reverted. It lives on as `pattern-layout-experiment/prototype.patch`, taken
against commit `6e286d12a` on `main`. Nothing was committed, and no tests were updated.

Decisions taken so far:

- **content** is the direction: the closer goes on a line of its own, on the content column.
  **trailing** remains the alternative, with the two problems in section 8 unsolved.
- **align** (the closer on the pattern column, bar + 2) and **indent** (the whole pattern one level
  in from the bar, so the content lands on bar + 8) were tried and dropped after review. Do not
  investigate these further. What came up while reviewing them: `align` gives the closer a column
  that is not a whole indent level, the kind of fixed column #3446 wants gone, and `indent` reads
  as a double indent on every multiline pattern.
- The content clears the bar by one indent level, not the pattern column. With an indent size of 2
  it lands on the pattern's own column (section 4.11).
- Let bindings use the shape in section 6, with the two decisions in 6.5.
- `foo (function` keeps hugging: the style guide prescribes this for a match lambda that is the last
  argument ("Treat match lambda's in a similar fashion"), and it is an expression rule, not a
  pattern rule. Changing it would be a separate proposal.
- Aligned and Stroustrup produce the same patterns. Cramped records and lists keep their layout
  (section 5).

### 11.2 Restoring the prototype

```bash
git apply pattern-layout-experiment/prototype.patch
dotnet build src/Fantomas.Core.Tests/
```

`PAT_MODE=content` (the default) or `PAT_MODE=trailing` selects the closer. The switch exists for the
experiment only and must not survive into a real change.

If `main` moved on and the patch no longer applies, section 7 describes every part of it.

### 11.3 Tests that change

With content, 14 tests change, all to the new shapes:

- `AlignedMultilineBracketStyleTests.fs`: record destructuring in let binding
- `AlignedMultilineBracketStyleTests.fs`: record inside pattern match, 1238
- `AlignedMultilineBracketStyleTests.fs`: SynPat.Record in pattern match with bracketOnSeparateLine
- `ChainFormattingTests.fs`: multiline parameter of a lambda argument to an intermediate call
- `ChainFormattingTests.fs`: multiline parameter of a lambda argument to an intermediate call, closing newline true
- `CrampedMultilineBracketStyleTests.fs`: multiline SynPat.Record in let binding destructuring
- `PatternMatchingTests.fs`: or pattern in list with when clause, 1522
- `PatternMatchingTests.fs`: triple or in array, long
- `PatternMatchingTests.fs`: triple or in list, long
- `PatternMatchingTests.fs`: trivia in SynArgPats, 2541
- `TupleTests.fs`: destructed tuple with comment after equals
- `TupleTests.fs`: multiline SynPat.Tuple should have parenthesis, 824
- `TupleTests.fs`: multiline SynPat.Tuple with existing parenthesis should not add additional parenthesis
- `TupleTests.fs`: removes type annotation without parens multiline, 2942

With trailing, 15 change: the same 14, plus `comment lost after named pat pair, 2953` in
`PatternMatchingTests.fs`, which fails idempotency (section 8).

### 11.4 Next steps

1. Settle the open questions in section 9.
2. Take the proposal to fslang-design. Neither style guide covers multiline patterns, so this is
   new guidance rather than a bug fix (see `docs/docs/end-users/StyleGuide.md`).
3. Remove the `PAT_MODE` switch and the trailing branch.
4. Update the 14 tests, and add tests for the issues: #2670, #1351, #1303, #1507.
5. Add Stroustrup and Cramped pattern tests: none of the Stroustrup suites has a multiline pattern
   today (section 5.7).
6. Cover `let!`, `and!` and `member val` with the binding shape (section 6).
7. Benchmark the arrow check and the binding check: both measure the pattern a second time.
8. Run `FormatChanged` and `AnalyzeChanged`, and write the changelog entries.

## 12. Reproducing

Everything lives in `pattern-layout-experiment/`, next to this document. It is untracked.

- `run.fsx` formats every sample in `samples/` under seven configurations (default, width 60,
  G-Research, indent 2, keep indent in branch, Cramped, Stroustrup) and reports validity,
  idempotency and overflow. `cfg=<name>` limits the configurations, and file name prefixes limit
  the samples: `PAT_MODE=content dotnet fsi pattern-layout-experiment/run.fsx s2 cfg=w60`.
- `probe.fsx probes/p1.txt` checks hand-written shapes against the parser, without formatting them.
  `p1` to `p3` hold the clause and or-pattern probes (section 2), `p4` and `p5` the record field
  probes (section 5.2), `p6` to `p9` the let binding probes (section 6).
- `gen.fsx <outDir> [cramped|stroustrup] [srcDir]` formats the samples in `doc-samples/` (or
  `expr-samples/`) and writes them with visible whitespace.
- `doc_template.md` is the source of this document. `render.fsx` fills its `{sample:mode}`
  placeholders from `out/` and writes `pattern-layout-walkthrough.md`.
- `regenerate.sh --main` produces the "today" blocks: run it *before* applying the patch.
  `regenerate.sh` then produces the content and trailing blocks and renders the document.
- Build `src/Fantomas.Core.Tests` itself before `dotnet test --no-build`. Its copy of
  `Fantomas.Core.dll` is not refreshed by building `src/Fantomas`.
