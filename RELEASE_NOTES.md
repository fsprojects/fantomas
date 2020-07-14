### 4.0.0-alpha-014 - 07/2020
* WIP for [#705](https://github.com/fsprojects/fantomas/issues/705)
* FCS 36.0.3
* Replaced json configuration with .editorconfig

### 3.3.0 - 02/2020

* Support for `and!`. [#690](https://github.com/fsprojects/fantomas/issues/690)
* Support for new slice syntax. [#691](https://github.com/fsprojects/fantomas/issues/691)
* Support for check style flag [#642](https://github.com/fsprojects/fantomas/issues/642)
* Update FCS to 34.1 [#699](https://github.com/fsprojects/fantomas/pull/699)
* Allow to configure spaces before and after semicolon. [#653](https://github.com/fsprojects/fantomas/issues/653)
* Fix Problem with --config and directory names containing ".". [#694](https://github.com/fsprojects/fantomas/issues/694)
* Fix Space is removed after Foo.Create. [#676](https://github.com/fsprojects/fantomas/issues/676)
* Fix Error in formatting nested else if construction. [#675](https://github.com/fsprojects/fantomas/issues/675)
* Fix Unbalanced and misplaced #if directives after formatting. [#635](https://github.com/fsprojects/fantomas/issues/635)
* Fix Stack overflow when using fantomas 3.2.0-beta-002. [#630](https://github.com/fsprojects/fantomas/issues/630)
* Fix --help and --version return exit code 1. [#612](https://github.com/fsprojects/fantomas/issues/612)
* Fix Line comment disappears after format. [#598](https://github.com/fsprojects/fantomas/issues/598)
* Fix Stack overflow for global tool on OSX. [#591](https://github.com/fsprojects/fantomas/issues/591)
* Fix Page width is not respected when formatting a function signature. [#495](https://github.com/fsprojects/fantomas/issues/495)
* Update README with link to YouTube videos series. [#672](https://github.com/fsprojects/fantomas/pull/672)

### 3.2.0 - 02/2020

* Added support for settings configuration file. [#354](https://github.com/fsprojects/fantomas/issues/354)
* Use Argu for commandline argument parsing. [#607](https://github.com/fsprojects/fantomas/pull/607)
* Fix Unicode null escapes are *still* unescaped. [#632](https://github.com/fsprojects/fantomas/issues/632)
* Fix Back ticks are removed from enum. [#626](https://github.com/fsprojects/fantomas/issues/626)
* Fix Pipe is removed when DU type name matches record type name. [#641](https://github.com/fsprojects/fantomas/issues/641)
* Fix fantomas --version should return version. [#625](https://github.com/fsprojects/fantomas/issues/625)
* Fix Extra newline between attribute and function. [#611](https://github.com/fsprojects/fantomas/issues/611)
* Fix Invalid code produced when formatting type alias for struct tuple. [#605](https://github.com/fsprojects/fantomas/issues/605)
* Fix Extra newlines repeatedly being added inside an object expression. [#601](https://github.com/fsprojects/fantomas/issues/601)
* Fix Empty line added on each format. [#597](https://github.com/fsprojects/fantomas/issues/597)
* Fix Error when formatting DU with single choice and attribute. [#596](https://github.com/fsprojects/fantomas/issues/596)
* Fix Unwanted new line after elif expression. [#588](https://github.com/fsprojects/fantomas/issues/588)
* Fix Unwanted new line added. [#586](https://github.com/fsprojects/fantomas/issues/586)
* Fix Empty lines in multi-line string get moved. [#577](https://github.com/fsprojects/fantomas/issues/577)
* Fix Error when combining #if directive with async block and let. [#576](https://github.com/fsprojects/fantomas/issues/576)
* Fix DllImport not detected when using additional attribute. [#574](https://github.com/fsprojects/fantomas/issues/574)
* Fix Comment in async block gets moved. [#573](https://github.com/fsprojects/fantomas/issues/573)
* Fix Enum comments removed. [#572](https://github.com/fsprojects/fantomas/issues/572)
* Fix Fantomas keeps adding new lines between two interface member implementations. [#569](https://github.com/fsprojects/fantomas/issues/569)
* Fix Unindented DU case causes compile error. [#567](https://github.com/fsprojects/fantomas/issues/567)
* Fix Erroneous whitespace in chained accessors. [#566](https://github.com/fsprojects/fantomas/issues/566)
* Fix Comments inside type signatures break formatting. [#565](https://github.com/fsprojects/fantomas/issues/565)
* Fix Hash symbol in signatures requires parens to remain. [#564](https://github.com/fsprojects/fantomas/issues/564)
* Fix Stack overflow in Strict mode. [#562](https://github.com/fsprojects/fantomas/issues/562)
* Fix Accessibility modifiers in DUs. [#561](https://github.com/fsprojects/fantomas/issues/561)
* Fix Line comment place after lambda instead of infix function. [#559](https://github.com/fsprojects/fantomas/issues/559)
* Fix Sequence expression inside computation expression outputs uncompilable code. [#553](https://github.com/fsprojects/fantomas/issues/553)
* Fix Comment after [ is not preserved. [#551](https://github.com/fsprojects/fantomas/issues/551)
* Fix Record update indentation incorrect around comments. [#537](https://github.com/fsprojects/fantomas/issues/537)
* Fix Formatting document continuously adds new lines each time it's called. [#535](https://github.com/fsprojects/fantomas/issues/535)
* Fix Comments like `(fun arg -> // comment` are lost. [#534](https://github.com/fsprojects/fantomas/issues/534)
* Fix KeepNewlineAfter not respected in let binding. [#524](https://github.com/fsprojects/fantomas/issues/524)
* Fix Improve formatting of lambda between parenthesis. [#523](https://github.com/fsprojects/fantomas/issues/523)
* Fix Crash when using --keepNewlineAfter. [#513](https://github.com/fsprojects/fantomas/issues/513)
* Fix Over-aggresive folding breaks nested lambda expressions. [#486](https://github.com/fsprojects/fantomas/issues/486)
* Fix Add FormatASTRangeAsync to API. [#454](https://github.com/fsprojects/fantomas/issues/454)
* Fix Intrinsic type extension member signatures are erased. [#413](https://github.com/fsprojects/fantomas/issues/413)
* Fix Inconsistencies in if formatting. [#135](https://github.com/fsprojects/fantomas/issues/135)

### 3.1.0 - 11/2019
* Fix invalid code generated after multiline string when other expressions exist on same line. [#545](https://github.com/fsprojects/fantomas/issues/545)
* Fix Trivia before elif generates invalid code due to missing indentation. [#527](https://github.com/fsprojects/fantomas/issues/527)
* Fix Don't add additional newline between two and blocks. [#520](https://github.com/fsprojects/fantomas/issues/520)
* Fix Print line comment after `{` [#517](https://github.com/fsprojects/fantomas/issues/517)
* Fix Formatting document removes '#if DEBUG' and '#endif'. [#512](https://github.com/fsprojects/fantomas/issues/512)
* Fix Some unicode control characters are incorrectly formatted. [#506](https://github.com/fsprojects/fantomas/issues/506)
* Fix New empty line inserted preceding module attribute. [#505](https://github.com/fsprojects/fantomas/issues/505)
* Fix Weird indentation/breaks with lambda in pipeline. [#503](https://github.com/fsprojects/fantomas/issues/503)
* Fix Sufficiently indent match case bodies for other indentation lengths than 4. [#502](https://github.com/fsprojects/fantomas/issues/502)
* Fix `--noSpaceBeforeColon` doesn't work. [#499](https://github.com/fsprojects/fantomas/issues/499)
* Fix Invalid code produced when wrapping method call to new line. [#498](https://github.com/fsprojects/fantomas/issues/498)
* Fix Indexer usage fails to parse. [#497](https://github.com/fsprojects/fantomas/issues/497)
* Use FCS 33.0.0. [pull/568](https://github.com/fsprojects/fantomas/pull/568)
* Use dotnet tools [pull/558](https://github.com/fsprojects/fantomas/pull/558)
* Add `--maxIfThenElseShortWidth` option, see [documentation](https://github.com/fsprojects/fantomas/blob/master/docs/Documentation.md)
  
### 3.0.0 - 10/2019
* Use FCS 32.0.0. [490b121af427ec4f6eba94f6d6d08cf3f91e04c8](https://github.com/fsprojects/fantomas/pull/434/commits/490b121af427ec4f6eba94f6d6d08cf3f91e04c8)
* Deprecate PreserveEndOfLine feature. [#390](https://github.com/fsprojects/fantomas/issues/390)
* Upgrade to .NET Core 3.0 and deprecate dotnet-fantomas. [b13aa00a57541be5f6182dc65ee27dc81174ab15](https://github.com/fsprojects/fantomas/pull/434/commits/b13aa00a57541be5f6182dc65ee27dc81174ab15)
* F# 4.7 support. [9ab8f007446d2e8311a204a9c8a73d758a189939](https://github.com/fsprojects/fantomas/pull/434/commits/9ab8f007446d2e8311a204a9c8a73d758a189939)
* KeepNewlineAfter setting. [#449](https://github.com/fsprojects/fantomas/issues/449)
* Refactored API [#454](https://github.com/fsprojects/fantomas/issues/454)
* Fix Adding parentheses around expressions can lead to incorrect indentation. [#385](https://github.com/fsprojects/fantomas/issues/385)
* Fix Indentation removed when combining comments and compiler directives. [#382](https://github.com/fsprojects/fantomas/issues/382)
* Fix Fantomas removes module and namespace if it is only 1 word (without dots). [#380](https://github.com/fsprojects/fantomas/issues/380)
* Fix Indentation incorrect for code with chained fluent interface method calls. [#379](https://github.com/fsprojects/fantomas/issues/379)
* Fix Incorrect indentation when space around delimiter is false. [#362](https://github.com/fsprojects/fantomas/issues/362)
* Fix Meaningful spaces can be collapsed in record-with expressions. [#353](https://github.com/fsprojects/fantomas/issues/353)
* Fix CLI arguments not accepted. [#334](https://github.com/fsprojects/fantomas/issues/334)
* Fix Calls to constructor from inherited class leads to wrong indentation (breaks build). [#326](https://github.com/fsprojects/fantomas/issues/326)
* Fix Indent level context lost in record initialization -- causes compilation failure. [#313](https://github.com/fsprojects/fantomas/issues/313)
* Fix Semi-colons may or may not be insterted in list literals. [#312](https://github.com/fsprojects/fantomas/issues/312) 
* Fix Handling of blank lines is idiosyncratic. [#311](https://github.com/fsprojects/fantomas/issues/311)
* Fix Over-length line not folded (inside #if block). [#309](https://github.com/fsprojects/fantomas/issues/309)
* Fix With --preserverEOL, multi-line lambdas are not correctly formatted. [#307](https://github.com/fsprojects/fantomas/issues/307)
* Fix Reformatting #if blocks controlling attributes changes the meaning of the code. [#304](https://github.com/fsprojects/fantomas/issues/304)
* Fix Some spacing is still lost in and around #if blocks. [#303](https://github.com/fsprojects/fantomas/issues/303)      
* Fix Weird formattiing behavior. [#287](https://github.com/fsprojects/fantomas/issues/287)
* Fix #if blocks result in code being moved around incorrectly. [#282](https://github.com/fsprojects/fantomas/issues/282) 
* Fix Inline replacement. [#278](https://github.com/fsprojects/fantomas/issues/278)
* Fix No new line after long name when copying record with "with". [#155](https://github.com/fsprojects/fantomas/issues/155)
* Fix Formatting of multi-line list literals. [#133](https://github.com/fsprojects/fantomas/issues/133)
* Fix Problems with very long lines and/or files. [#119](https://github.com/fsprojects/fantomas/issues/119)
* Fix Adjust default configuration to be more F# idiomatic. [#61](https://github.com/fsprojects/fantomas/issues/61)       
* Fix Excessive line breaking. [#43](https://github.com/fsprojects/fantomas/issues/43)
* Fix [Trivia] Line comment after `then` breaks code. [#451](https://github.com/fsprojects/fantomas/issues/451)
* Fix Bug report from fantomas-ui. [#450](https://github.com/fsprojects/fantomas/issues/450)
* Fix Publish 3.0.0 to NuGet.org with a preview flag. [#448](https://github.com/fsprojects/fantomas/issues/448)
* Fix Include directive with `__SOURCE_DIRECTORY__` is removed and replace. [#447](https://github.com/fsprojects/fantomas/issues/447)
* Fix Formatting if expressions not according to style-guide. [#446](https://github.com/fsprojects/fantomas/issues/446)   
* Fix PreserveEndOfLine+SpaceAroundDelimiter add an unnecessary space before closing brace. [#443](https://github.com/fsprojects/fantomas/issues/443)
* Fix Record option with attribute gets an additional space with PreserveEndOfLine. [#442](https://github.com/fsprojects/fantomas/issues/442)
* Fix Quotation escapes removed - Bug report from fantomas-ui. [#440](https://github.com/fsprojects/fantomas/issues/440)
* Fix Fantomas fails in Fake script. [#439](https://github.com/fsprojects/fantomas/issues/439)
* Fix Configuration options for "Fabulous compatibility"?. [#437](https://github.com/fsprojects/fantomas/issues/437)
* Fix Using fantomas with dotnet-format. [#430](https://github.com/fsprojects/fantomas/issues/430)
* Fix Change space before colon default to false. [#429](https://github.com/fsprojects/fantomas/issues/429)
* Fix global.json specifies outdated dotnet sdk. [#426](https://github.com/fsprojects/fantomas/issues/426)
* Fix Errors after formatting secondary constructors. [#423](https://github.com/fsprojects/fantomas/issues/423)
* Fix Wrong attribute and xml doc placement on reformat with PreserveEOL. [#422](https://github.com/fsprojects/fantomas/issues/422)
* Fix When running the tool for a fairly large script file (1000 lines) nothing happens. [#416](https://github.com/fsprojects/fantomas/issues/416)
* Fix Is Fantomas still supported for VS? Couldn't find it?. [#415](https://github.com/fsprojects/fantomas/issues/415)    
* Fix the required library libhostfxr.so could not be found. [#412](https://github.com/fsprojects/fantomas/issues/412)    
* Fix Latest FCS breaks fantomas. [#410](https://github.com/fsprojects/fantomas/issues/410)
* Fix Compiled operators names are replaced with source names. [#409](https://github.com/fsprojects/fantomas/issues/409)  
* Fix Wrong anon module formatting when filename starts with a digit. [#408](https://github.com/fsprojects/fantomas/issues/408)
* Fix Raw method names with `/` are formatted improperly. [#406](https://github.com/fsprojects/fantomas/issues/406)       
* Fix Attributes followed by unit literals aren't formatted properly. [#405](https://github.com/fsprojects/fantomas/issues/405)
* Fix Wrongly removed "with" for member on record, PreserveEndOfLine=true. [#388](https://github.com/fsprojects/fantomas/issues/388)
* Fix IndentSpaceNum is ignored if PreserveEndOfLine is enable. [#387](https://github.com/fsprojects/fantomas/issues/387) 
* Fix An option to preserve empty lines between logical blocks. [#496](https://github.com/fsprojects/fantomas/issues/496) 
* Fix Bug report from fantomas-ui. [#491](https://github.com/fsprojects/fantomas/issues/491)
* Fix `finally` is duplicated, moved. [#487](https://github.com/fsprojects/fantomas/issues/487)
* Fix Multiple #if cases causes failure (3.0.0 beta3 and beta4). [#484](https://github.com/fsprojects/fantomas/issues/484)
* Fix Significant spaces lost (v3.0.0-beta4). [#483](https://github.com/fsprojects/fantomas/issues/483)
* Fix #if/#endif lost with v3.0.0-beta-004. [#482](https://github.com/fsprojects/fantomas/issues/482)
* Fix Exception when code for no defines is empty. [#480](https://github.com/fsprojects/fantomas/issues/480)
* Fix Stackoverflow exception in AstTransformer. [#479](https://github.com/fsprojects/fantomas/issues/479)
* Fix [Trivia] Significant spacing added after let binding in function. [#478](https://github.com/fsprojects/fantomas/issues/478)
* Fix Incorrect replacement of `override` with `member`. [#477](https://github.com/fsprojects/fantomas/issues/477)        
* Fix [Trivia] Fantomas removes the parentheses around Fable's string field access syntax. [#476](https://github.com/fsprojects/fantomas/issues/476)
* Fix [Trivia] Additional line added after very specific case. [#475](https://github.com/fsprojects/fantomas/issues/475)  
* Fix Multiple extension members cause additional lines to be printed. [#473](https://github.com/fsprojects/fantomas/issues/473)
* Fix Long text lines cause out of range exception in 3.0.0-beta. [#472](https://github.com/fsprojects/fantomas/issues/472)
* Fix Class member attributes cause additional lines. [#471](https://github.com/fsprojects/fantomas/issues/471)
* Fix 'with' incorrectly removed. [#469](https://github.com/fsprojects/fantomas/issues/469)
* Fix Online UI tool doesn't understand F# 4.6's {| |}. [#467](https://github.com/fsprojects/fantomas/issues/467)
* Fix Exception handling 'with' clause using drop-through is malformed. [#465](https://github.com/fsprojects/fantomas/issues/465)
* Fix Unicode null escapes are unescaped (v3.0.0. beta1, beta2). [#464](https://github.com/fsprojects/fantomas/issues/464)
* Fix Multiline record not on new line after DU constructor. [#462](https://github.com/fsprojects/fantomas/issues/462)    
* Fix Feature request: Prefix generic type parameters. [#460](https://github.com/fsprojects/fantomas/issues/460)
* Fix Fantomas hangs indefinitely when run. [#459](https://github.com/fsprojects/fantomas/issues/459)
* Fix record mutation: first field after `with` should be placed in a new line. [#457](https://github.com/fsprojects/fantomas/issues/457)
* Fix for i in 1..-1..0 do: should add space before `-`. [#456](https://github.com/fsprojects/fantomas/issues/456)
* Fix Incorrect handling of attributes in static method. [#452](https://github.com/fsprojects/fantomas/issues/452)


#### 3.0.0-beta-006 - 10/2019
* FCS 32
* Partial F# 4.7 support

#### 3.0.0-beta-005 - 09/2019
* Move to .NETCore 3 global tool, deprecated net461
* Deprecated dotnet-fantomas tool
* Bug fixes

#### 3.0.0-beta-004 - 09/2019
* Restructured library API
* Improved performance

#### 3.0.0-beta-003 - 09/2019
* More trivia fixes
* FCS 31

#### 3.0.0-beta-002 - 07/2019
* More fixes with hash directives

#### 3.0.0-beta-001 - 07/2019
* Use FCS 28.0.0 and net461. [#436](https://github.com/fsprojects/fantomas/pull/436)
* Deprecated `PreserveEndOfLine` setting in favor of Trivia. [#434](https://github.com/fsprojects/fantomas/pull/434)
* Added support for formatting multiple code path in defines.

#### 2.9.2 - 02-2019 
* Fix PreserveEndOfLine introduces additional newlines. [#360](https://github.com/fsprojects/fantomas/issues/360)
* Fix Extra newline is introduced when file ends with multiline comment. [#363](https://github.com/fsprojects/fantomas/issues/363)
* Fix Fantomas shouldn't remove parens when using the dynamic operator (?). [#369](https://github.com/fsprojects/fantomas/issues/369)
* Fix Extra semicolons in list with PreserveEndOfLine. [#371](https://github.com/fsprojects/fantomas/issues/371)
* Use qualified name for inputPath and outputPath. [#376](https://github.com/fsprojects/fantomas/pull/376)
* Added Nightly nuget feed. [#375](https://github.com/fsprojects/fantomas/pull/375)
* Moved solution file to root folder. [#377](https://github.com/fsprojects/fantomas/pull/377)
* Fix Multiple attributes indented wrongly with PreserveEndOfLine. [#370](https://github.com/fsprojects/fantomas/issues/370)
* Fix pattern matched unions are formatted badly. [#283](https://github.com/fsprojects/fantomas/issues/283)
* Fix wrong indentation when accessing member of constructed record. [#383](https://github.com/fsprojects/fantomas/issues/383)
* Fix latest fantomas breaks Falanx indentation. [#384](https://github.com/fsprojects/fantomas/issues/384)
* Add support for SynExpr.Set(_,_,_). [#368](https://github.com/fsprojects/fantomas/issues/368)
* Fix Recurse option with globally installed dotnet-tool traverses 'obj' directories. [#341](https://github.com/fsprojects/fantomas/issues/341)
* Fix creates invalid F# for string handling operations. [#365](https://github.com/fsprojects/fantomas/issues/365)
* Use FAKE 5. [#261](https://github.com/fsprojects/fantomas/issues/261)
* Added FAKE 5 sample. [#402](https://github.com/fsprojects/fantomas/issues/402)


#### 2.9.1 - 11-2018
* Added instructions for vscode and online website. [#333](https://github.com/fsprojects/fantomas/pull/333)
* Removed trailing spaces for each line, after formatting. [#328](https://github.com/fsprojects/fantomas/issues/328)
* Allow easy build/format/build cycle of external projects. [#337](https://github.com/fsprojects/fantomas/pull/337)
* Fix `in` is removed from binding when PreserveEndOfLine is true. [#340](https://github.com/fsprojects/fantomas/issues/340)
* Fix unnecessary conversion from 'YieldOrReturn' to 'YieldOrReturnFrom', by update of FCS. [#339](https://github.com/fsprojects/fantomas/issues/339)
* Fix Lazy<'T> is incorrectly rewritten. [#335](https://github.com/fsprojects/fantomas/issues/335)
* Fix Fluent API with comments breaks code. [#331](https://github.com/fsprojects/fantomas/issues/331)
* Update to .NET Core 2.1 [#350](https://github.com/fsprojects/fantomas/issues/350)
* Removed unused open statements. [#352](https://github.com/fsprojects/fantomas/pull/352)
* Added regression test for Implicit module is added to resulting code. [#355](https://github.com/fsprojects/fantomas/pull/355)
* Fix wrong comment placement. [#289](https://github.com/fsprojects/fantomas/issues/289)

#### 2.9.0 - 10-2018
* Improved README. [#243](https://github.com/fsprojects/fantomas/issues/243)
* Bad split of chained method call expression. [#246](https://github.com/fsprojects/fantomas/issues/246)
* rec modifier removed for namespaces and modules. [#292](https://github.com/fsprojects/fantomas/issues/292)
* Over-enthusiastic removal of parentheses [#249](https://github.com/fsprojects/fantomas/issues/249)
* Broken reformat of "if" inside call (fantomas-tool 2.8.0) [#288](https://github.com/fsprojects/fantomas/issues/288)
* Support struct tuple. [#224](https://github.com/fsprojects/fantomas/issues/224)
* Support match! [#262](https://github.com/fsprojects/fantomas/issues/262)
* Fix for AST formatting regression. [#321](https://github.com/fsprojects/fantomas/issues/321)
* Upgrade to .NET 4.5.2 [#325](https://github.com/fsprojects/fantomas/pull/325)

#### 2.8.1 - 09-2018
* Force parameter is *true* by default. [#267](https://github.com/fsprojects/fantomas/issues/267)
* Formatting compiler directives with inactive code is incorrect. [#270](https://github.com/fsprojects/fantomas/issues/270)
* Fix "Fantômas" mistranslation in README. [#273](https://github.com/fsprojects/fantomas/pull/273)
* Fix for preserve EOL feature. [#275](https://github.com/fsprojects/fantomas/pull/275)
* rec keyword is removed from recursive modules [#274](https://github.com/fsprojects/fantomas/issues/274)
* Access modifiers in method signatures in signature files are not formatted correctly. [#284](https://github.com/fsprojects/fantomas/issues/284)
* `#if FOO || BAR => #if FOO` [#280](https://github.com/fsprojects/fantomas/issues/280)
* `override` becomes `member` in interface implementations. [#263](https://github.com/fsprojects/fantomas/issues/263)
* Operator >>.~ incorrectly formatted. [#291](https://github.com/fsprojects/fantomas/issues/291)
* Bad choice of line break location in boolean equality expression. [#248](https://github.com/fsprojects/fantomas/issues/248)
* Pipe operator inside quotation expression leads to wrong indentation. [#256](https://github.com/fsprojects/fantomas/issues/256)
* broken indent by pipe formatting. [#269](https://github.com/fsprojects/fantomas/issues/269)

#### 2.8.0 - 07-2018
* Wrong indentation of `else` after comment [#241](https://github.com/dungpa/fantomas/issues/241)
* Change Content to None [#238](https://github.com/dungpa/fantomas/issues/238)
* Formatting of code with a pipe and a lambda expression [#211](https://github.com/dungpa/fantomas/issues/211)
* Added support for a global dotnet cli tool [#252](https://github.com/dungpa/fantomas/issues/252)
* Fix for chopped of members [#239](https://github.com/dungpa/fantomas/issues/239)
* Added option to preserve blank lines [#143](https://github.com/dungpa/fantomas/issues/143)

#### 2.7.1 - 03-05-2018
* Hotfix for runtime problem when using dotnet cli tool

#### 2.7.0 - 02-05-2018
* Upgrade to .NET Core 2.0
* Published as `clitool` 
* Upgrade to FCS 22.0.3
* Single case DUs on same line [#234](https://github.com/dungpa/fantomas/pull/234)
* Removed whitespaces around type provider arguments [#235](https://github.com/dungpa/fantomas/pull/235)

#### 2.6.1 - 22-04-2017
* Upgrade to FCS 11.0.4

#### 2.5.0 - 19-02-2017
* Upgrade to FCS 10.0.0

#### 2.4.0 - 24-10-2016
* Upgrade to FCS 8.0.0

#### 2.3.0 - 10-07-2016
* Upgrade to FCS 5.0.0

#### 2.2.0 - 24-04-2016
* Handle record types with private fields [#197](https://github.com/dungpa/fantomas/pull/197)
* Create a separate CLI NuGet package [#196](https://github.com/dungpa/fantomas/pull/196)
* Do not print out module names if not necessary [#196](https://github.com/dungpa/fantomas/pull/196)

#### 2.1.0 - 01-04-2016
* Upgrade to FCS 2.0.0.8

#### 2.0.2 - 15-11-2015
* Add a new public API using static members. Deprecate old functions.
* Fix https://github.com/fsprojects/VisualFSharpPowerTools/issues/1151
* Fix https://github.com/fsprojects/VisualFSharpPowerTools/issues/1143

#### 1.11.0 - 13-09-2015
* Fix https://github.com/fsprojects/VisualFSharpPowerTools/issues/366 [#177](https://github.com/dungpa/fantomas/pull/177)
* Migrate to FCS 1.4.0.5

#### 1.10.0 - 29-08-2015
* Improve formatting of bind operator [#175](https://github.com/dungpa/fantomas/pull/175)

#### 1.9.0 - 19-08-2015
* Fix https://github.com/fsprojects/VisualFSharpPowerTools/issues/1050 ([#172](https://github.com/dungpa/fantomas/pull/172))

#### 1.8.0-beta - 19-07-2015
* Migrate to F# 4.0 ([#170](https://github.com/dungpa/fantomas/pull/170))

#### 1.7.0 - 10-06-2015
* Print attributes on member arguments ([#168](https://github.com/dungpa/fantomas/pull/168))
* Do not misrecognize "then" blocks in explicit constructors ([#168](https://github.com/dungpa/fantomas/pull/168))
* Suppress whitespaces inside dot access ([#168](https://github.com/dungpa/fantomas/pull/168))
* Insert brackets around tuples in type test patterns ([#168](https://github.com/dungpa/fantomas/pull/168))
* Fix desugar patterns' bug exposed by FsCheck ([#167](https://github.com/dungpa/fantomas/pull/167))

#### 1.6.0 - 25-10-2014
* Add FAKE task helper

#### 1.5.0 - 18-09-2014
* Bugfix release

#### 1.4.0 - 01-07-2014
* Bugfix release

#### 1.3.0 - 17-05-2014
* Bugfix release

#### 1.2.0 - 21-04-2014
* Bugfix release

#### 1.1.0 - 29-03-2014
* Bugfix release

#### 1.0.5 - 07-01-2014
* Fully support F# 3.1
* Compatible with F# on Mono
* Handle external functions
* Improve support of multiline strings
* Implement various bug fixes 

#### 1.0.4 - 16-11-2013
* Implement various bug fixes

#### 1.0.3 - 04-10-2013
* Implement various bug fixes
* Synchronize version numbers with NuGet packages

#### 0.7.1 - 11-09-2013
* Support Visual Studio 2013 (not support F# 3.1 constructs yet)

#### 0.7.0 - 16-08-2013
* Implement formatting cursor positions
* Implement reordering of open statements
* Enhance indentation of anonymous functions
* Add line breaks for nested let bindings
* Implement various bug fixes

#### 0.5.0 - 16-07-2013
* Improve formatting of signatures
* Improve UI interaction
* Enhance spacing of function applications and arguments
* Implement various bug fixes

#### 0.4.1 - 02-07-2013
* Initial release
