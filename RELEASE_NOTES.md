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
