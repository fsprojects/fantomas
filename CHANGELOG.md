# Changelog

## [4.6.5] - 2022-02-18

### Changed
* Use [KeepAChangelog](https://keepachangelog.com/en/1.0.0/#how) instead of `RELEASE_NOTES.md`. [#2095](https://github.com/fsprojects/fantomas/pull/2095)

### Fixed
* Error formatting defines #if (!DEBUG) ... #endif (no #else). [#2098](https://github.com/fsprojects/fantomas/issues/2098)
* Single block comment in namespace is disappearing. [#1951](https://github.com/fsprojects/fantomas/issues/1951)
* Unexpected newline added before let bang. [#1932](https://github.com/fsprojects/fantomas/issues/1932)
* Creating anonymous record based on a function call with a list arg fails. [#1749](https://github.com/fsprojects/fantomas/issues/1749)
* Access modifier is lost in extern declaration. [#1213](https://github.com/fsprojects/fantomas/issues/1213)
* Unexpected indentation in if-else when using keep_if_then_in_same_line=true. [#1160](https://github.com/fsprojects/fantomas/issues/1160)

### Documentation
* Add additional information about the style guide in CONTRIBUTING.md. [#2078](https://github.com/fsprojects/fantomas/pull/2078)
* Fix FSharp compiler docs link for SynExpr. [#2082](https://github.com/fsprojects/fantomas/pull/2082)
* Fix FSharp compiler docs link for SynExpr.DotGet expression. [#2084](https://github.com/fsprojects/fantomas/pull/2084)
* Pre push hook to ensure code is formatted on upload. [#2085](https://github.com/fsprojects/fantomas/pull/2085)
* Explain what's needed to close issues, that aren't reproducible anymore. [#2092](https://github.com/fsprojects/fantomas/pull/2092)

## [4.6.4] - 2022-02-11

### Fixed
* type declaration loses 'when' qualifier. [#2075](https://github.com/fsprojects/fantomas/issues/2075)
* Comments in anonymous record values are deleted. [#2067](https://github.com/fsprojects/fantomas/issues/2067)
* Comment after [] is lost, more common in records. [#2043](https://github.com/fsprojects/fantomas/issues/2043)
* Named argument should have a space. [#1877](https://github.com/fsprojects/fantomas/issues/1877)

## [4.6.3] - 2022-02-08

### Documented
* Running fake build locally. [#2056](https://github.com/fsprojects/fantomas/issues/2056)
* Replace Gitter with Discord. [#2070](https://github.com/fsprojects/fantomas/pull/2070)

### Fixed
* Spaces are lost in multi range expression. [#2071](https://github.com/fsprojects/fantomas/issues/2071)

## [4.6.2] - 2022-02-06

### Fixed
* Comment in method chain gets deleted. [#2062](https://github.com/fsprojects/fantomas/issues/2062)
* Idempotency and correctness problem when using multiple if statements as list comprehension. [#2055](https://github.com/fsprojects/fantomas/issues/2055)
* Comment inside LongIdentWithDots not preserved. [#2027](https://github.com/fsprojects/fantomas/issues/2027)
* Consecutive #load statements are merged into one line if namespace is declared below. [#2014](https://github.com/fsprojects/fantomas/issues/2014)
* Line of code including a backward composition operator causes error. [#1998](https://github.com/fsprojects/fantomas/issues/1998)

## [4.6.1] - 2022-02-01

### Fixed
* Type parameter comment lost in formatting. [#2052](https://github.com/fsprojects/fantomas/issues/2052)
* Unicode string containing "\000" is replaced by null character when being formatted. [#2050](https://github.com/fsprojects/fantomas/issues/2050)
* Idempotency problem when comment after opening bracket in Elmish expression without children. [#2037](https://github.com/fsprojects/fantomas/issues/2037)
* Default settings in .editorconfig files. [#2030](https://github.com/fsprojects/fantomas/issues/2030)
* "cannot determine if synExpr Paren" when Lambda after If expression. [#2015](https://github.com/fsprojects/fantomas/issues/2015)
* Comment removed in do binding. [#1875](https://github.com/fsprojects/fantomas/issues/1875)
* Comment after arrow is missing in SynExpr.Lambda. [#1870](https://github.com/fsprojects/fantomas/issues/1870)
* else if turned into elif when KeepIndentInBranch = true. [#1818](https://github.com/fsprojects/fantomas/issues/1818)
* Comment after closing brace in nested record is lost with default settings. [#1172](https://github.com/fsprojects/fantomas/issues/1172)
* Inconsistent spacing of multiline object expressions in lists with default settings. [#1170](https://github.com/fsprojects/fantomas/issues/1170)
* type definition in signature file wrapped with hash directives. [#1115](https://github.com/fsprojects/fantomas/issues/1115)
* Strict mode strips literal strings. [#560](https://github.com/fsprojects/fantomas/issues/560)

## [4.6.0] - 2022-01-14

### Changed
* Unify Fantomas versions across editors. [#1844](https://github.com/fsprojects/fantomas/issues/1844)
* Update formatting types in signatures (See [fslang-design](https://github.com/fsharp/fslang-design/issues/644)). [#1994](https://github.com/fsprojects/fantomas/pull/1994)
* Read editorconfig settings before overwriting settings with request configuration. [#2006](https://github.com/fsprojects/fantomas/pull/2006)
* Expose configuration documentation in Fantomas.Daemon [#1956](https://github.com/fsprojects/fantomas/pull/1956)
* Update FCS to 41.0.1 [#1949](https://github.com/fsprojects/fantomas/pull/1949)
* Wider default fsharp_max_value_binding_width. [#1947](https://github.com/fsprojects/fantomas/pull/1947)
* Update FCS to 41.0.0-preview.21472.3 [#1927](https://github.com/fsprojects/fantomas/pull/1927)
* Update record should indent from the curly brace instead of the identifier. [#1876](https://github.com/fsprojects/fantomas/issues/1876)
* Update style of lambda argument. [#1871](https://github.com/fsprojects/fantomas/issues/1871)
* Update to FCS 40.0.1-preview.21352.5

### Added
* Editor.config: insert_final_newline. [#2002](https://github.com/fsprojects/fantomas/issues/2002)

### Fixed
* Comment lost between attribute and nested module [#2016](https://github.com/fsprojects/fantomas/issues/2016)
* Incorrect new array indexing formatting. [#1985](https://github.com/fsprojects/fantomas/issues/1985)
* Idempotency problem when exception definition used in signature file. [#1974](https://github.com/fsprojects/fantomas/issues/1974)
* Create parsingOption via CodeFormatterImpl helper. [#1946](https://github.com/fsprojects/fantomas/pull/1946)
* Explicit call of active pattern fails. [#1937](https://github.com/fsprojects/fantomas/issues/1937)
* Idempotency problem when destructing a record inside a lambda argument. [#1922](https://github.com/fsprojects/fantomas/issues/1922)
* Exception with boolean constant in #if. [#2013](https://github.com/fsprojects/fantomas/issues/2013)

## [4.5.12] - 2022-01-08

### Fixed
* Strings containing spaces at end of line change meaning. [#1941](https://github.com/fsprojects/fantomas/issues/1941)
* Explicit class/end/with loses members. [#1940](https://github.com/fsprojects/fantomas/issues/1940)
* Idempotency problem when static member with get. [#1913](https://github.com/fsprojects/fantomas/issues/1913)

## [4.5.11] - 2021-12-29

### Fixed
* KeepIndentInBranch not being respected. [#2003](https://github.com/fsprojects/fantomas/issues/2003)

## [4.5.10] - 2021-12-04

### Fixed
* Fantomas writes even when not necessary. [#1984](https://github.com/fsprojects/fantomas/issues/1984)

## [4.5.9] - 2021-11-24

### Fixed
* Operator application to some literals doesn't preserve spacing. [#1979](https://github.com/fsprojects/fantomas/issues/1979)

## [4.5.8] - 2021-11-19

### Fixed
* Fantomas is unable to format valid F# (.net 6.0) program. [#1969](https://github.com/fsprojects/fantomas/issues/1969)
* Attributes on static members of recursive types formats incorrectly. [#1962](https://github.com/fsprojects/fantomas/issues/1962)
* val mutable in signature loses 'mutable'. [#1954](https://github.com/fsprojects/fantomas/issues/1954)
* Literals in signatures lose values. [#1953](https://github.com/fsprojects/fantomas/issues/1953)
* Attribute on member of mutually dependent types fails to validate. [#1918](https://github.com/fsprojects/fantomas/issues/1918)
* Wrong code is generated for member attribute in recursive type. [#1898](https://github.com/fsprojects/fantomas/issues/1898)
* Attribute on type function incorrectly placed for 'and' types. [#1874](https://github.com/fsprojects/fantomas/issues/1874)

## [4.5.7] - 2021-11-07

### Fixed
* Formatting power operator in code quotation pattern match fails. [#1945](https://github.com/fsprojects/fantomas/issues/1945)

## [4.5.6] - 2021-11-03

### Fixed
* Offside code created when base constructor wraps across lines. [#1942](https://github.com/fsprojects/fantomas/issues/1942)
* Extra space in val and member bindings in signature files. [#1934](https://github.com/fsprojects/fantomas/issues/1934)

## [4.5.5] - 2021-10-27

### Fixed
* Provide more information when string merge failed. [#1904](https://github.com/fsprojects/fantomas/issues/1904)
* Comment gets duplicated. [#1912](https://github.com/fsprojects/fantomas/issues/1912)
* Vanity alignment used when splitting line in match block. [#1901](https://github.com/fsprojects/fantomas/issues/1901)
* Unexpected loss of newline in closing bracket. [#1835](https://github.com/fsprojects/fantomas/issues/1835)
* Relative patterns in .fantomasignore don't match any files. [#1713](https://github.com/fsprojects/fantomas/issues/1713)

## [4.5.4] - 2021-10-05

### Fixed
* Documentation contains reference to infix multiline formatter even though it's removed. [#1884](https://github.com/fsprojects/fantomas/issues/1884)
* Newline was introduced in Sequential with LetBang. [#1882](https://github.com/fsprojects/fantomas/issues/1882)
* Vanity alignment used when calling base constructor. [#1442](https://github.com/fsprojects/fantomas/issues/1442)

## [4.5.3] - 2021-09-07

### Fixed
* indented #if directive inside another non-indented #if directive drops significant whitespace. [#1866](https://github.com/fsprojects/fantomas/issues/1866)

## [4.5.2] - 2021-08-11

### Fixed
* Dropped comment with function with type parameter. [#1861](https://github.com/fsprojects/fantomas/issues/1861)
* Comment dropped in a multi-option match. [#1855](https://github.com/fsprojects/fantomas/issues/1855)
* StackoverflowException when formatted long triple-quoted strings. [#1837](https://github.com/fsprojects/fantomas/issues/1837)
* Comment removed in match. [#1677](https://github.com/fsprojects/fantomas/issues/1677)

## [4.5.1] - 2021-07-24

### Fixed
* StackOverflow exceptions when collecting ColMultilineItem list. [#1839](https://github.com/fsprojects/fantomas/issues/1839)
* Overly indented members on a record type with accessibility modifier. [#1824](https://github.com/fsprojects/fantomas/issues/1824)
* MultiLineLambdaClosingNewline not respected with function keyword. [#1823](https://github.com/fsprojects/fantomas/issues/1823)
* Comment is lost at the end of a match. [#1822](https://github.com/fsprojects/fantomas/issues/1822)

### Changed
* Honor .fantomasignore file when processing a folder. [#1834](https://github.com/fsprojects/fantomas/pull/1834)

## [4.5.0] - 2021-07-07

### Added
* Always place bar in front of discriminated union. [#1750](https://github.com/fsprojects/fantomas/issues/1750)
* Support multiple files/dirs as command-line arguments. [#1696](https://github.com/fsprojects/fantomas/issues/1696)
* Disable empty line between mutually recursive type. [#1658](https://github.com/fsprojects/fantomas/issues/1658)
* BlankLinesAroundNestedMultilineExpressions. [#1587](https://github.com/fsprojects/fantomas/pull/1587)

### Changed
* Initial support of KeepIndentInBranch. [#1361](https://github.com/fsprojects/fantomas/issues/1361)
* Update to FCS 39 [#1479](https://github.com/fsprojects/fantomas/pull/1479)

### Fixed
* Collect empty define block as single trivia. [#1528](https://github.com/fsprojects/fantomas/pull/1528)
* Refactor ASTTransformer. [#1497](https://github.com/fsprojects/fantomas/pull/1497)
* replace genTypeByLookup with Trivia. [#594](https://github.com/fsprojects/fantomas/issues/594)
* Space between identifier and then is lost. [#1816](https://github.com/fsprojects/fantomas/issues/1816)
* KeepIndentInBranch causing offside error in if condition. [#1812](https://github.com/fsprojects/fantomas/issues/1812)
* Indentation warnings for multiline match expression. [#1774](https://github.com/fsprojects/fantomas/issues/1774)
* Extra indentation on match-case block. [#1234](https://github.com/fsprojects/fantomas/issues/1234)
* Fantomas introduces meaningless match block. [#1806](https://github.com/fsprojects/fantomas/issues/1806)
* Lazy causes indentation to produce invalid F#. [#1805](https://github.com/fsprojects/fantomas/issues/1805)
* Nested Fluent API produces wrong code: misses indentation. [#1804](https://github.com/fsprojects/fantomas/issues/1804)
* Tuple should be consider short branch in KeepIndentInBranch setting. [#1800](https://github.com/fsprojects/fantomas/issues/1800)
* Fantomas adds "of" to a union case when it is seemingly _too long_. [#1796](https://github.com/fsprojects/fantomas/issues/1796)
* Function application in if expression should remain on single line. [#1795](https://github.com/fsprojects/fantomas/issues/1795)
* Conditional code is not printed. [#1794](https://github.com/fsprojects/fantomas/issues/1794)
* Records in list in a pattern match always have a semicolon. [#1793](https://github.com/fsprojects/fantomas/issues/1793)
* parenthesis expression in then should further indent inner expression. [#1777](https://github.com/fsprojects/fantomas/issues/1777)
* Wild cards in lambda. [#1789](https://github.com/fsprojects/fantomas/issues/1789)
* Soundness regression in 4.5.0-beta-001. Nested lambdas are stripped. [#1782](https://github.com/fsprojects/fantomas/issues/1782)
* KeepIndentInBranch not respected when returning short function application. [#1779](https://github.com/fsprojects/fantomas/issues/1779)
* Hash directive not printed above recursive binding. [#1776](https://github.com/fsprojects/fantomas/issues/1776)
* Fantomas adds space before colon when SpaceBeforeColon is false. [#1773](https://github.com/fsprojects/fantomas/issues/1773)
* Long return expression not formatted multiline. [#1771](https://github.com/fsprojects/fantomas/issues/1771)
* Add support for LibraryOnlyStaticOptimization. [#1769](https://github.com/fsprojects/fantomas/issues/1769)
* fsharp_keep_indent_in_branch not respected for multiline infix expression. [#1768](https://github.com/fsprojects/fantomas/issues/1768)
* Inline lambda in argument list causes problems for argument chopping and lambda alignment. [#1028](https://github.com/fsprojects/fantomas/issues/1028)
* Difference in behavior between signature and implementation files for single case DU. [#973](https://github.com/fsprojects/fantomas/issues/973)
* Concatenation of multi-line """ strings mis-indented. [#639](https://github.com/fsprojects/fantomas/issues/639)
* Comment after arrow lost with KeepIndentInBranch. [#1759](https://github.com/fsprojects/fantomas/issues/1759)
* Multiline if/then/else in infix expression. [#1757](https://github.com/fsprojects/fantomas/issues/1757)
* Newline and comment trivia are mixed up. [#1468](https://github.com/fsprojects/fantomas/issues/1468)
* Inline lambda in argument list causes problems for argument chopping and lambda alignment. [#1028](https://github.com/fsprojects/fantomas/issues/1028)
* Difference in behavior between signature and implementation files for single case DU. [#973](https://github.com/fsprojects/fantomas/issues/973)
* Concatenation of multi-line """ strings mis-indented. [#639](https://github.com/fsprojects/fantomas/issues/639)
* Trivia before SynPat.Paren. [#1753](https://github.com/fsprojects/fantomas/issues/1753)
* Idempotency problem when trailing comment is present. [#1538](https://github.com/fsprojects/fantomas/issues/1538)
* Offside error after formatting with MultiLineLambdaClosingNewline. [#1741](https://github.com/fsprojects/fantomas/issues/1741)
* Pipe character missing in single-case try/with. [#1571](https://github.com/fsprojects/fantomas/issues/1571)
* Multiline if condition in KeepIndentInBranch leads to warnings. [#1729](https://github.com/fsprojects/fantomas/issues/1729)
* KeepIndentInBranch not respected for values. [#1728](https://github.com/fsprojects/fantomas/issues/1728)
* KeepIndentInBranch not respected. [#1717](https://github.com/fsprojects/fantomas/issues/1717)
* Bad interaction between KeepIndentInBranch and MultiLambdaClosingNewLine. [#1715](https://github.com/fsprojects/fantomas/issues/1715)
* KeepIndentInBranch not being respected. [#1714](https://github.com/fsprojects/fantomas/issues/1714)
* Breaking method chain in the middle of an if statement causes offside error. [#1712](https://github.com/fsprojects/fantomas/issues/1712)
* Offside error when a comment appears before a match statement in pipeline. [#1711](https://github.com/fsprojects/fantomas/issues/1711)
* Line comment after short pattern match clause causes additional parenthesis. [#1721](https://github.com/fsprojects/fantomas/issues/1721)
* Appending two lists with @ fails. [#1719](https://github.com/fsprojects/fantomas/issues/1719)
* Leading block comment makes type multiline. [#1718](https://github.com/fsprojects/fantomas/issues/1718)
* List concat chain using operators fails to format. [#1188](https://github.com/fsprojects/fantomas/issues/1188)
* Unexpected newline after inner let binding. [#1709](https://github.com/fsprojects/fantomas/issues/1709)
* Currying a pair gets spread over multiple lines. [#1700](https://github.com/fsprojects/fantomas/issues/1700)
* Additional parentheses are introduced when match is piped. [#1698](https://github.com/fsprojects/fantomas/issues/1698)
* \t in string replaced by ASCII 9. [#1695](https://github.com/fsprojects/fantomas/issues/1695)
* Idempotency problem with nested else if. [#1648](https://github.com/fsprojects/fantomas/issues/1648)
* Idempotency problem with ifdef in a function. [#1646](https://github.com/fsprojects/fantomas/issues/1646)
* string interpolation part crossing max line length introduces new line and lots of whitespace. [#1511](https://github.com/fsprojects/fantomas/issues/1511)
* Splitting generic type parameters over multiple lines sometimes puts the break in an invalid place. [#1687](https://github.com/fsprojects/fantomas/issues/1687)
* Idempotency problem related to comments in with. [#1686](https://github.com/fsprojects/fantomas/issues/1686)
* Upcast requires a line break but is not given one. [#1685](https://github.com/fsprojects/fantomas/issues/1685)
* Idempotency problem when trying to format a type with method chaining in it. [#1681](https://github.com/fsprojects/fantomas/issues/1681)
* Line comments after null value used as argument in function call are removed. [#1676](https://github.com/fsprojects/fantomas/issues/1676)
* Idempotency problem when trying to format a type with method chaining in it. [#1681](https://github.com/fsprojects/fantomas/issues/1681)
* Comment after constant got moved to the next line. [#1671](https://github.com/fsprojects/fantomas/issues/1671)
* Idempotency problem when ending with a comment. [#1649](https://github.com/fsprojects/fantomas/issues/1649)
* end_of_line not respecting when file has ifdef. [#1673](https://github.com/fsprojects/fantomas/issues/1673)
* Attributes in a recursive type get misplaced. [#1668](https://github.com/fsprojects/fantomas/issues/1668)
* alternative_long_member_definitions docs. [#1666](https://github.com/fsprojects/fantomas/issues/1666)
* Stack overflow on macOS for long pipelines. [#1453](https://github.com/fsprojects/fantomas/issues/1453)
* Comment inside empty Elmish children is lost. [#1179](https://github.com/fsprojects/fantomas/issues/1179)
* Long .Setup line in Moq code results in broken indentation. [#1662](https://github.com/fsprojects/fantomas/issues/1662)
* Don't introduce parenthesis around SynPat.IsInst. [#1660](https://github.com/fsprojects/fantomas/issues/1660)
* Offside error splitting long line. [#1651](https://github.com/fsprojects/fantomas/issues/1651)
* Offside errors after formatting. [#1650](https://github.com/fsprojects/fantomas/issues/1650)
* Idempotency problem with with block. [#1647](https://github.com/fsprojects/fantomas/issues/1647)
* Formatting code makes Interpolated verbatim strings to non-verbatim strings, which breaks the code. [#1645](https://github.com/fsprojects/fantomas/issues/1645)
* Comment stripped in a record with semi-colons. [#1643](https://github.com/fsprojects/fantomas/issues/1643)
* Another shape which isn't respecting KeepIndentInBranch. [#1638](https://github.com/fsprojects/fantomas/issues/1638)
* Incorrect code when function type parameter would break over line. [#1637](https://github.com/fsprojects/fantomas/issues/1637)
* Multiline type parameter arguments inside indentation. [#1611](https://github.com/fsprojects/fantomas/issues/1611)
* Fantomas add extra parenthesis in desugared lambda. [#1631](https://github.com/fsprojects/fantomas/issues/1631)
* Mutually recursive functions break with function invocation above definition. [#1628](https://github.com/fsprojects/fantomas/issues/1628)
* Removal of bar in one-case DU. [#1563](https://github.com/fsprojects/fantomas/issues/1563)
* Multiline type signature is not unindent. [#1624](https://github.com/fsprojects/fantomas/issues/1624)
* Failure to unindent with KeepIndentInBranch. [#1621](https://github.com/fsprojects/fantomas/issues/1621)
* Some of newlines in string interpolation is deleted. [#1613](https://github.com/fsprojects/fantomas/issues/1613)
* Idempotency problem when piping just before the keyword in. [#1610](https://github.com/fsprojects/fantomas/issues/1610)
* Idempotency problem with lazy. [#1609](https://github.com/fsprojects/fantomas/issues/1609)
* Idempotency issue with let … in. [#1608](https://github.com/fsprojects/fantomas/issues/1608)
* Idempotency problem when splitting && over multiple lines. [#1607](https://github.com/fsprojects/fantomas/issues/1607)
* Idempotency problem when splitting if-clause over multiple lines. [#1606](https://github.com/fsprojects/fantomas/issues/1606)
* Idempotency problem with recursive types in FSI. [#1605](https://github.com/fsprojects/fantomas/issues/1605)
* Idempotency problem with comments at the end of code. [#1604](https://github.com/fsprojects/fantomas/issues/1604)
* Block comment in Elmish expression removed. [#1601](https://github.com/fsprojects/fantomas/issues/1601)
* System.Exception: was not expecting token DOLLAR. [#1598](https://github.com/fsprojects/fantomas/issues/1598)
* Define before opening bracket. [#1597](https://github.com/fsprojects/fantomas/issues/1597)
* Swap internal and inline in signature file. [#1590](https://github.com/fsprojects/fantomas/issues/1590)
* member laced with conditional. [#1589](https://github.com/fsprojects/fantomas/issues/1589)
* if expression is not indented. [#1588](https://github.com/fsprojects/fantomas/issues/1588)
* Fantomas throws an exception with custom operator (>??). [#1533](https://github.com/fsprojects/fantomas/issues/1533)
* Unexpected identifier in lambda expression when using pre processor directives. [#1484](https://github.com/fsprojects/fantomas/issues/1484)
* FormatException: Unexpected symbol '|' for DU case under pre-processor directive. [#1483](https://github.com/fsprojects/fantomas/issues/1483)
* End-of-line comments lost when formatting multiline type function signature. [#1287](https://github.com/fsprojects/fantomas/issues/1287)
* Comments are sometimes removed unexpectedly during formatting. [#1276](https://github.com/fsprojects/fantomas/issues/1276)
* max_line_length not respected in mutliline infix expression in if. [#1584](https://github.com/fsprojects/fantomas/issues/1584)
* Const() stripped from string literals break. [#1574](https://github.com/fsprojects/fantomas/issues/1574)
* Conversion of & to byref is invalid in extern function declaration. [#1567](https://github.com/fsprojects/fantomas/issues/1567)
* Quote character in a comment results in removing code inside preprocessor directive. [#1504](https://github.com/fsprojects/fantomas/issues/1504)
* Documentation comment for primary class constructor is removed. [#1286](https://github.com/fsprojects/fantomas/issues/1286)
* A comment before an anonymous function gets swallowed up. [#1190](https://github.com/fsprojects/fantomas/issues/1190)
* KeepIndentInBranch not being respected?. [#1569](https://github.com/fsprojects/fantomas/issues/1569)
* Split of very long function call in if body. [#1564](https://github.com/fsprojects/fantomas/issues/1564)
* fsi extension loses docstring in mutually recursive type. [#1562](https://github.com/fsprojects/fantomas/issues/1562)
* .fsi extension: attribute followed by docstring loses the docstring. [#1561](https://github.com/fsprojects/fantomas/issues/1561)
* .fsi extension causes first DU case's docstring to be lost. [#1560](https://github.com/fsprojects/fantomas/issues/1560)
* KeepIfThenInSameLine breaks function indentation. [#1559](https://github.com/fsprojects/fantomas/issues/1559)
* All SynExpr should start on next line and indent. [#1556](https://github.com/fsprojects/fantomas/issues/1556)
* Missing indentation when using pattern matching via anonymous functions in a pipeline. [#614](https://github.com/fsprojects/fantomas/issues/614)
* Required type arguments are removed with DotGet lambda. [#1550](https://github.com/fsprojects/fantomas/issues/1550)
* Fantomas removes the format string from string interpolation. [#1549](https://github.com/fsprojects/fantomas/issues/1549)
* Missing in keyword. [#1548](https://github.com/fsprojects/fantomas/issues/1548)
* Pattern inside when clause. [#1545](https://github.com/fsprojects/fantomas/issues/1545)
* Hash defines in let binding. [#1543](https://github.com/fsprojects/fantomas/issues/1543)
* Comments are stripped at the end of a vertical list. [#1541](https://github.com/fsprojects/fantomas/issues/1541)
* Idempotency problem when match is follow by pipe. [#1532](https://github.com/fsprojects/fantomas/issues/1532)
* Idempotency problem when exceptions in signature file. [#1531](https://github.com/fsprojects/fantomas/issues/1531)
* Exception - detect of multiple defines when define surrounds a DU member. [#1503](https://github.com/fsprojects/fantomas/issues/1503)
* DotGet infix expression. [#1529](https://github.com/fsprojects/fantomas/issues/1529)
* SynPat.Or should have the same indent. [#1522](https://github.com/fsprojects/fantomas/issues/1522)
* DotGet with parenthesis. [#1521](https://github.com/fsprojects/fantomas/issues/1521)
* Trivia regressions around SynConst. [#1518](https://github.com/fsprojects/fantomas/issues/1518)
* Long signatures have additonal newline inserted and don't respect the indent from the config. [#1515](https://github.com/fsprojects/fantomas/issues/1515)
* Bad formatting when using elmish style + empty arrays. [#1510](https://github.com/fsprojects/fantomas/issues/1510)
* \xHH escapes in string literal are expanded. [#1508](https://github.com/fsprojects/fantomas/issues/1508)
* Indentation of pattern match clause. [#1501](https://github.com/fsprojects/fantomas/issues/1501)
* Class parameters expands unit for long lines. [#1494](https://github.com/fsprojects/fantomas/issues/1494)
* Comments inside Elmish gets repeated. [#1347](https://github.com/fsprojects/fantomas/issues/1347)
* Extra space throws exception. [#1476](https://github.com/fsprojects/fantomas/issues/1476)
* comment deleted on reformat. [#1343](https://github.com/fsprojects/fantomas/issues/1343)

## [4.4.0] - 2021-02-25

### Changed
* Revisit SynExpr.IfThenElse. [#1258](https://github.com/fsprojects/fantomas/issues/1258)
* Target netcoreapp3.1 for fantomas-tool.
* Stricter trivia selection. [#1304](https://github.com/fsprojects/fantomas/pull/1304)

### Fixed
* Idempotency problem when function argument's type annotation requires brackets. [#1470](https://github.com/fsprojects/fantomas/issues/1470)
* Inconsistency about when fantomas decides to split `()` (unit) to the next line. [#1469](https://github.com/fsprojects/fantomas/issues/1469)
* Unexpected newline between hash directives. [#1464](https://github.com/fsprojects/fantomas/issues/1464)
* Oscillating newlines in custom computation expression. [#1463](https://github.com/fsprojects/fantomas/issues/1463)
* Violation of "avoid name-sensitive alignments" clause. [#1422](https://github.com/fsprojects/fantomas/issues/1422)
* Incorrectly combines tokens when formatting. [#1407](https://github.com/fsprojects/fantomas/issues/1407)
* string interpolation with multi-line string causes literal part to change. [#1451](https://github.com/fsprojects/fantomas/issues/1451)
* `when` clause in try-with block gets split and causes compiler warnings about indentation. [#1406](https://github.com/fsprojects/fantomas/issues/1406)
* Long line breaks match. [#1403](https://github.com/fsprojects/fantomas/issues/1403)
* Long line causes offside error. [#1402](https://github.com/fsprojects/fantomas/issues/1402)
* Nested matches format into something invalid. [#1400](https://github.com/fsprojects/fantomas/issues/1400)
* Shortening big `if` clause still creates compiler warnings. [#1390](https://github.com/fsprojects/fantomas/issues/1390)
* “FS0058: Possible incorrect indentation” on function composition after running Fantomas. [#1341](https://github.com/fsprojects/fantomas/issues/1341)
* Typed App followed by chained lambda should not add space. [#1448](https://github.com/fsprojects/fantomas/issues/1448)
* TypedApp should not have a space when chained. [#1447](https://github.com/fsprojects/fantomas/issues/1447)
* Unexpected newline after short match expression. [#1445](https://github.com/fsprojects/fantomas/issues/1445)
* Space after chain lambda function is not allowed. [#1440](https://github.com/fsprojects/fantomas/issues/1440)
* Formatting error with MultilineBlockBracketsOnSameColumn. [#1396](https://github.com/fsprojects/fantomas/issues/1396)
* fsharp_space_before_uppercase_invocation=true breaks method calls. [#1437](https://github.com/fsprojects/fantomas/issues/1437)
* Crash regression on 4.4.0-beta-003. [#1438](https://github.com/fsprojects/fantomas/issues/1438)
* MultiLineLambdaClosingNewline concats lambda arguments. [#1427](https://github.com/fsprojects/fantomas/issues/1427)
* `member val` causes invalid code to be generated. [#1426](https://github.com/fsprojects/fantomas/issues/1426)
* Surround return type annotations with white space [F# style guide]. [#1420](https://github.com/fsprojects/fantomas/issues/1420)
* Lists concatene onto one line invalidly. [#1405](https://github.com/fsprojects/fantomas/issues/1405)
* Accessibility modifier on record causes unindentation of following type. [#1404](https://github.com/fsprojects/fantomas/issues/1404)
* Invalid addition of a space after constructor invocation. [#1401](https://github.com/fsprojects/fantomas/issues/1401)
* "Inline" is incorrectly stripped out in FSI file. [#1399](https://github.com/fsprojects/fantomas/issues/1399)
* Multiple type checks in a `try/with` get collapsed. [#1395](https://github.com/fsprojects/fantomas/issues/1395)
* Short line length and member constraint leads to invalid code. [#1394](https://github.com/fsprojects/fantomas/issues/1394)
* Object expression newline gets added/removed. [#1388](https://github.com/fsprojects/fantomas/issues/1388)
* Arrays of constructors with lots of arguments gets dedented too much. [#1382](https://github.com/fsprojects/fantomas/issues/1382)
* Format in pre-commit hook. [#1207](https://github.com/fsprojects/fantomas/issues/1207)
* Shortening an 'if' condition causes compilation warnings about indentation. [#1374](https://github.com/fsprojects/fantomas/issues/1374)
* Some escapes are unexpectedly modified in character literal patterns. [#1372](https://github.com/fsprojects/fantomas/issues/1372)
* Fantomas formats with an error for very long DU case match. [#1364](https://github.com/fsprojects/fantomas/issues/1364)
* Fantomas errors out on `new Foo ""`. [#1363](https://github.com/fsprojects/fantomas/issues/1363)
* Aesthetics of long members in a type declaration. [#1362](https://github.com/fsprojects/fantomas/issues/1362)
* Comment on first constructor argument gets removed. [#1350](https://github.com/fsprojects/fantomas/issues/1350)
* “FS0058: Possible incorrect indentation” around if/then/else after running Fantomas. [#1349](https://github.com/fsprojects/fantomas/issues/1349)
* Failing to format file should return an exit code different than 0. [#1340](https://github.com/fsprojects/fantomas/issues/1340)
* Shorter MaxLineLength with long variable name yields invalid F# code according to fantomas. [#1241](https://github.com/fsprojects/fantomas/issues/1241)
* MultilineBlockBracketsOnSameColumn should be honored inside match block. [#1238](https://github.com/fsprojects/fantomas/issues/1238)
* Update constructor formatting to match MS Style guide. [#1359](https://github.com/fsprojects/fantomas/issues/1359)
* Violation of name-sensitive alignments. [#1358](https://github.com/fsprojects/fantomas/issues/1358)
* Pattern matching breaks code when expression is long and somewhat complex. [#1352](https://github.com/fsprojects/fantomas/issues/1352)
* Inserts extra newline everytime formatter runs. [#1346](https://github.com/fsprojects/fantomas/issues/1346)
* Functions looses space before parameter if func is defined inside method. [#1345](https://github.com/fsprojects/fantomas/issues/1345)
* Required backslash removed in string interpolation. [#1344](https://github.com/fsprojects/fantomas/issues/1344)
* Swallows comment before #nowarn directive. [#1220](https://github.com/fsprojects/fantomas/issues/1220)
* Swallows comment inside `with` block (of a try-with). [#1219](https://github.com/fsprojects/fantomas/issues/1219)
* Program.fs inside full path can lead to invalid AST. [#1337](https://github.com/fsprojects/fantomas/issues/1337)
* Formatting of long parameter lists. [#657](https://github.com/fsprojects/fantomas/issues/657)
* DotGet inside If expression not correct on second format. [#1329](https://github.com/fsprojects/fantomas/issues/1329)
* Pipe is indented too far. [#1327](https://github.com/fsprojects/fantomas/issues/1327)
* IfThenElse piped leads to invalid code. [#1324](https://github.com/fsprojects/fantomas/issues/1324)
* Multiline when condition in pattern match needs to be further indented. [#1320](https://github.com/fsprojects/fantomas/issues/1320)
* Add comma in front of expression in tuple with if/then/else. [#1319](https://github.com/fsprojects/fantomas/issues/1319)
* New line before for loop not preserved. [#1317](https://github.com/fsprojects/fantomas/issues/1317)
* Newline before set expression is lost. [#1314](https://github.com/fsprojects/fantomas/issues/1314)
* Newline after let bang is missing. [#1313](https://github.com/fsprojects/fantomas/issues/1313)
* Revisit place parameters on a new line for long definitions. [#1307](https://github.com/fsprojects/fantomas/issues/1307)
* static member should only have a single indent. [#1300](https://github.com/fsprojects/fantomas/issues/1300)
* Elmish-like syntax using yields with interspersed let statements breaks the code. [#1191](https://github.com/fsprojects/fantomas/issues/1191)
* The 'member' keyword gets deleted in 'abstract member' declarations. [#1106](https://github.com/fsprojects/fantomas/issues/1106)
* Don't indent too far. [#659](https://github.com/fsprojects/fantomas/issues/659)
* static member should only have a single indent. [#1300](https://github.com/fsprojects/fantomas/issues/1300)
* Named string argument to type provider requires a space prior to '@', which Fantomas removes. [#1209](https://github.com/fsprojects/fantomas/issues/1209)
* Don't indent too far. [#659](https://github.com/fsprojects/fantomas/issues/659)
* spaces removed from string. [#1290](https://github.com/fsprojects/fantomas/issues/1290)

## [4.3.0] - 2020-12-17

### Added
* Clarify constructors. [#1217](https://github.com/fsprojects/fantomas/issues/1217)
* MultiLineLambdaClosingNewline. [#1221](https://github.com/fsprojects/fantomas/issues/1221)
* Disable Elmish syntax. [#1198](https://github.com/fsprojects/fantomas/issues/1198)

### Changed
* Support user-provided end-of-line characters. [#1231](https://github.com/fsprojects/fantomas/issues/1231)
* Add option to make expressions multiline based on number of subexpressions rather than character length. [#1143](https://github.com/fsprojects/fantomas/issues/1143)
* Update to FCS 38. [#1240](https://github.com/fsprojects/fantomas/pull/1240)

### Fixed
* Comment after let binding breaks downstream output. [#1284](https://github.com/fsprojects/fantomas/issues/1284)
* Comments are removed before and after empty array literals. [#1281](https://github.com/fsprojects/fantomas/issues/1281)
* Use a safe filename when formatting from CodeFormatter. [#1279](https://github.com/fsprojects/fantomas/issues/1279)
* Incorrect movement of a comma. [#966](https://github.com/fsprojects/fantomas/issues/966)
* Bracket indentation is incorrect. [#1271](https://github.com/fsprojects/fantomas/issues/1271)
* tuple with match formats to invalid code. [#1269](https://github.com/fsprojects/fantomas/issues/1269)
* Incorrect movement of a comma. [#966](https://github.com/fsprojects/fantomas/issues/966)
* Not adding a space even when all SpaceBefore* settings are enabled. [#964](https://github.com/fsprojects/fantomas/issues/964)
* Multiline if condition can have incorrect indentation error. [#1267](https://github.com/fsprojects/fantomas/issues/1267)
* multiline do bang gives a warning. [#1265](https://github.com/fsprojects/fantomas/issues/1265)
* regressions. [#1264](https://github.com/fsprojects/fantomas/pull/1264)
* multiline yield bang in list should be further indented. [#1254](https://github.com/fsprojects/fantomas/issues/1254)
* Or pipe in destructured record should not be splitted. [#1252](https://github.com/fsprojects/fantomas/issues/1252)
* Swap order of private and inline. [#1250](https://github.com/fsprojects/fantomas/issues/1250)
* Comment is lost in enum. [#1247](https://github.com/fsprojects/fantomas/issues/1247)
* Nested if/else/then in short mode. [#1243](https://github.com/fsprojects/fantomas/issues/1243)
* Something doesn't add up in fix for 303. [#1093](https://github.com/fsprojects/fantomas/issues/1093)
* Fantomas format is "unstable/oscillates" after upcast operator: adds/removes empty line. [#1227](https://github.com/fsprojects/fantomas/issues/1227)
* Misplaces `=` in function signature so it's invalid F# code. [#1218](https://github.com/fsprojects/fantomas/issues/1218)
* Additional newline is added between if/elif and for loop. [#1211](https://github.com/fsprojects/fantomas/issues/1211)
* Let binding in hash directive disappears. [#1205](https://github.com/fsprojects/fantomas/issues/1205)
* Downcast operator doesn't get a new line. [#1203](https://github.com/fsprojects/fantomas/issues/1203)
* Implicit unit else results in extra lines with each reformat. [#1187](https://github.com/fsprojects/fantomas/issues/1187)
* Incorrect formatting of function parameter application with lambdas. [#1201](https://github.com/fsprojects/fantomas/issues/1201)
* Lambda functions in fluent API calls should indent further. [#970](https://github.com/fsprojects/fantomas/issues/970)
* Hard to read code when using Thoth.Json. [#685](https://github.com/fsprojects/fantomas/issues/685)
* --check should ignore the line endings. [#1196](https://github.com/fsprojects/fantomas/issues/1196)
* Format entire return type on the line. [#1181](https://github.com/fsprojects/fantomas/issues/1181)
* Only add one `in` keyword in LetOrUse. [#1176](https://github.com/fsprojects/fantomas/issues/1176)
* Multiline SynPat.Record in pattern match is formatted as a mixture of single/multiline styles. [#1173](https://github.com/fsprojects/fantomas/issues/1173)
* Inconsistent indentation of multiline records with internal keyword when fsharp_multiline_block_brackets_on_same_column is on/off. [#1171](https://github.com/fsprojects/fantomas/issues/1171)
* Lambda argument splits in awkward way. [#1164](https://github.com/fsprojects/fantomas/issues/1164)
* Multiline expression should be on next line. [#1158](https://github.com/fsprojects/fantomas/issues/1158)
* Missing in keyword makes code invalid. [#1114](https://github.com/fsprojects/fantomas/issues/1114)
* Invalid code after format. [#1032](https://github.com/fsprojects/fantomas/issues/1032)
* Space before ^ SRTP type is removed in function call. [#984](https://github.com/fsprojects/fantomas/issues/984)
* Shouldn't remove space after colon. [#908](https://github.com/fsprojects/fantomas/issues/908)
* Crash when formatting with config file. [#824](https://github.com/fsprojects/fantomas/issues/824)
* Formatting typeof generic static constraint fails to compile. [#803](https://github.com/fsprojects/fantomas/issues/803)
* Indenting of record definition when internal. [#658](https://github.com/fsprojects/fantomas/issues/658)

## [4.2.0] - 2020-09-25

### Added
* MaxDotGetExpressionWidth. [#501](https://github.com/fsprojects/fantomas/issues/501)

### Fixed
* Confusing symmetry between infix operators. [#988](https://github.com/fsprojects/fantomas/issues/988)
* Comment before closing parenthesis is lost. [#1146](https://github.com/fsprojects/fantomas/issues/1146)

## [4.1.1] - 2020-09-17

### Fixed
* No newline between module and first declaration. [#1139](https://github.com/fsprojects/fantomas/issues/1139)
* additional new lines added after each call to format. [#1137](https://github.com/fsprojects/fantomas/issues/1137)
* Generics error when breaking line. [#1134](https://github.com/fsprojects/fantomas/issues/1134)
* Comments on DUs parameterized by functions are dropped. [#1128](https://github.com/fsprojects/fantomas/issues/1128)
* Preserve underscore in number. [#1120](https://github.com/fsprojects/fantomas/issues/1120)

## [4.1.0] - 2020-09-10

### Changed
* Ignore files by `.fantomasignore` file. [#420](https://github.com/fsprojects/fantomas/issues/420)
* Limit trivia by AST MainNode name. [#992](https://github.com/fsprojects/fantomas/pull/992)
* Lead by example. [#666](https://github.com/fsprojects/fantomas/issues/666)
* Verify all unit test whether the formatted code is valid. [#842](https://github.com/fsprojects/fantomas/issues/842)

### Fixed
* Comments get dropped from the end of multi-line records. [#1124](https://github.com/fsprojects/fantomas/issues/1124)
* Functions in nested modules which follow a type consisting of only one member of a DU are un-nested from that module. [#1123](https://github.com/fsprojects/fantomas/issues/1123)
* After discriminated union in module wrongly indented. [#1122](https://github.com/fsprojects/fantomas/issues/1122)
* Extra whitespace with a type which has an attribute. [#1116](https://github.com/fsprojects/fantomas/issues/1116)
* Adds newline between comments and (all but the first) attribute. [#1108](https://github.com/fsprojects/fantomas/issues/1108)
* Extra whitespace lines added in fsi files at the end of a nested module declaration. [#1105](https://github.com/fsprojects/fantomas/issues/1105)
* Extra new line is added before attributes. [#1097](https://github.com/fsprojects/fantomas/issues/1097)
* Comments after closing brace are lost. [#1096](https://github.com/fsprojects/fantomas/issues/1096)
* Comment after Or operator lost. [#1095](https://github.com/fsprojects/fantomas/issues/1095)
* Trivia before bar is being repeated. [#1083](https://github.com/fsprojects/fantomas/issues/1083)
* Comment after arrow is being duplicated. [#1082](https://github.com/fsprojects/fantomas/issues/1082)
* Allow line break after return to avoid excessive indenting/aligning. [#1062](https://github.com/fsprojects/fantomas/issues/1062)
* Name of static members are removed/empty. [#1059](https://github.com/fsprojects/fantomas/issues/1059)
* When using parenthesis in type definition, it will sometimes keep adding additional () for each time fantomas i run. [#1057](https://github.com/fsprojects/fantomas/issues/1057)
* Multiline string in use expression. [#1055](https://github.com/fsprojects/fantomas/issues/1055)
* Issue #246 has returned in v4.0.0. [#1051](https://github.com/fsprojects/fantomas/issues/1051)
* Indentation after multiple hash directives is off. [#1026](https://github.com/fsprojects/fantomas/issues/1026)
* if/then/else indented too far. [#1054](https://github.com/fsprojects/fantomas/issues/1054)
* Single AST node should contain trivia. [#1031](https://github.com/fsprojects/fantomas/issues/1031)
* Formatter adds extra newlines between type and any subsequent val in .fsi files. [#1029](https://github.com/fsprojects/fantomas/issues/1029)
* Comments in if/then/else statements are sometimes deleted. [#1019](https://github.com/fsprojects/fantomas/issues/1019)
* Moves type name around when writing constrained type defintions. [#1018](https://github.com/fsprojects/fantomas/issues/1018)
* Line break before bracket on long method call followed by member access causes semantic change. [#994](https://github.com/fsprojects/fantomas/issues/994)
* Long union case should be split over multiple lines. [#972](https://github.com/fsprojects/fantomas/issues/972)
* FSI formatting does the wrong thing with comments on single-case DU. [#965](https://github.com/fsprojects/fantomas/issues/965)
* Invalid unit test ``should break lines on multiline if conditions``. [#863](https://github.com/fsprojects/fantomas/issues/863)
* Abstract member declarations don't follow page width. [#435](https://github.com/fsprojects/fantomas/issues/435)

## [4.0.0] - 2020-08-27

### Changed
* Add initial support of String Interpolation. [#998](https://github.com/fsprojects/fantomas/issues/998)
* Extract FakeHelpers and EditorConfig to Fantomas.Extras project [#986](https://github.com/fsprojects/fantomas/issues/986)
* Update FCS to 37. [#996](https://github.com/fsprojects/fantomas/pull/996)
* Update FCS to 36. [#899](https://github.com/fsprojects/fantomas/pull/899), [#961](https://github.com/fsprojects/fantomas/pull/961)
* Replaced json configuration with .editorconfig. [#650](https://github.com/fsprojects/fantomas/issues/650)
* Sunset setting ReorderOpenDeclaration. [#645](https://github.com/fsprojects/fantomas/issues/645)
* Sunset setting KeepNewlineAfter. [#737](https://github.com/fsprojects/fantomas/issues/737)
* Renamed setting IndentSpaceNum to IndentSize. [#940](https://github.com/fsprojects/fantomas/pull/940)
* Renamed setting PageWidth to MaxLineLength. [#940](https://github.com/fsprojects/fantomas/pull/940)
* Removed all style configuration options from CLI tool. [#704](https://github.com/fsprojects/fantomas/issues/704)
* Added [Benchmarks](https://fsprojects.github.io/fantomas/). [#867](https://github.com/fsprojects/fantomas/issues/867)
* Split up setting SpaceBeforeArgument to multiple settings. [#649](https://github.com/fsprojects/fantomas/pull/649)
  - SpaceBeforeParameter
  - SpaceBeforeLowercaseInvocation
  - SpaceBeforeUppercaseInvocation
  - SpaceBeforeClassConstructor
  - SpaceBeforeMember
* Increase control over length of certain code constructs. [#697](https://github.com/fsprojects/fantomas/issues/697)
  - MaxRecordWidth
  - MaxArrayOrListWidth
  - MaxValueBindingWidth
  - MaxFunctionBindingWidth
  - MaxElmishWidth
* Better support for Elmish inspired code. [#922](https://github.com/fsprojects/fantomas/pull/922)

### Added
* SingleArgumentWebMode. [#927](https://github.com/fsprojects/fantomas/issues/927)
* AlignFunctionSignatureToIndentation. [#946](https://github.com/fsprojects/fantomas/issues/946)
* AlternativeLongMemberDefinitions. [#913](https://github.com/fsprojects/fantomas/issues/913)
* MultilineBlockBracketsOnSameColumn. [#453](https://github.com/fsprojects/fantomas/issues/453)
* NewlineBetweenTypeDefinitionAndMembers. [#752](https://github.com/fsprojects/fantomas/issues/752)
* KeepIfThenInSameLine. [#825](https://github.com/fsprojects/fantomas/issues/825)

### Fixed
* Comments in match statements are sometimes deleted. [#1010](https://github.com/fsprojects/fantomas/issues/1010)
* Comments on members with get/set are deleted. [#1009](https://github.com/fsprojects/fantomas/issues/1009)
* Hexadecimal numbers in enums are output as decimal numbers. [#1006](https://github.com/fsprojects/fantomas/issues/1006)
* List indentation issue. [#999](https://github.com/fsprojects/fantomas/issues/999)
* Hexadecimal numbers in match arms are output as decimal numbers. [#995](https://github.com/fsprojects/fantomas/issues/995)
* Removes the first comment above the member of the list. [#990](https://github.com/fsprojects/fantomas/issues/990)
* Brackets increased every time Fantomas is run. [#989](https://github.com/fsprojects/fantomas/issues/989)
* Bug: Hash directive in computation expression. [#977](https://github.com/fsprojects/fantomas/issues/977)
* Bug with hash directives inside a match statement. [#976](https://github.com/fsprojects/fantomas/issues/976)
* Bug with defines in record member assignment. [#968](https://github.com/fsprojects/fantomas/issues/968)
* Backquotes are stripped down from ``checked``. [#937](https://github.com/fsprojects/fantomas/issues/937)
* Fantomas wraps extra set of parenthesis around parenthesis. [#921](https://github.com/fsprojects/fantomas/issues/921)
* Remove indent setting from CLI tool. [#888](https://github.com/fsprojects/fantomas/issues/888)
* Fantomas generating invalid F# when trying to format a list created with yields. [#882](https://github.com/fsprojects/fantomas/issues/882)
* Invalid unit test ``method call on multiple lines``. [#862](https://github.com/fsprojects/fantomas/issues/862)
* Invalid unit test ``should break on . operator and keep indentation``. [#860](https://github.com/fsprojects/fantomas/issues/860)
* Multi-line arguments to chained method calls produce invalid code. [#702](https://github.com/fsprojects/fantomas/issues/702)
* Line comment displaced from commented #-directive. [#638](https://github.com/fsprojects/fantomas/issues/638)
* #if'd argument types are lost. [#633](https://github.com/fsprojects/fantomas/issues/633)
* #if'd attributes moved to wrong column. [#631](https://github.com/fsprojects/fantomas/issues/631)
* Assembly attributes create over-long lines. [#629](https://github.com/fsprojects/fantomas/issues/629)
* List expression can get combined to a single line with different semantics. [#931](https://github.com/fsprojects/fantomas/issues/931)
* Additional new line inserted around attributes. [#949](https://github.com/fsprojects/fantomas/issues/949)
* `with get` removal in FSI invalid. [#945](https://github.com/fsprojects/fantomas/issues/945)
* FSI file has "abstract" stripped. [#944](https://github.com/fsprojects/fantomas/issues/944)
* Insertion of space before function application can break dot-chaining. [#943](https://github.com/fsprojects/fantomas/issues/943)
* Concatenation of lines can break operator precedence. [#942](https://github.com/fsprojects/fantomas/issues/942)
* Extra spaces inserted in record definition. [#941](https://github.com/fsprojects/fantomas/issues/941)
* Comments at the end of async blocks are deleted automatically. [#936](https://github.com/fsprojects/fantomas/issues/936)
* Newline between comments should lead to individual comments. [#920](https://github.com/fsprojects/fantomas/issues/920)
* VS Code | Extra white space added to record definition. [#910](https://github.com/fsprojects/fantomas/issues/910)
* When cutting off function invocations, should place each param in its own line (or align them to the 1st param). [#907](https://github.com/fsprojects/fantomas/issues/907)
* Try online link points to old location. [#890](https://github.com/fsprojects/fantomas/issues/890)
* Leading `|` in single-case union type with access modifier. [#889](https://github.com/fsprojects/fantomas/issues/889)
* Type constraint on a type definition causes a loss of the type definition. [#887](https://github.com/fsprojects/fantomas/issues/887)
* Fantomas removes the 'and' if there are multiple member constraints on a function declaration. [#886](https://github.com/fsprojects/fantomas/issues/886)
* Comments inside a type definition can cause issues. [#885](https://github.com/fsprojects/fantomas/issues/885)
* Long function signature should align with equal sign. [#883](https://github.com/fsprojects/fantomas/issues/883)
* Newline not preserved between let and let bang. [#879](https://github.com/fsprojects/fantomas/issues/879)
* Stackoverflow problem with let bang in match. [#876](https://github.com/fsprojects/fantomas/issues/876)
* Incorrect formatting for chained class members using Websharper. [#871](https://github.com/fsprojects/fantomas/issues/871)
* Pipe before and inside lambda leads to wrong indent of following lambda. [#870](https://github.com/fsprojects/fantomas/issues/870)
* Formatting Program.fs with `--check` fails. [#869](https://github.com/fsprojects/fantomas/issues/869)
* Possible wrong indentation for functions with parameters over multiple lines. [#868](https://github.com/fsprojects/fantomas/issues/868)
* Invalid unit test ``different attributes according to defines``. [#864](https://github.com/fsprojects/fantomas/issues/864)
* Invalid unit test ``record instance with inherit keyword``. [#861](https://github.com/fsprojects/fantomas/issues/861)
* Invalid unit test ``should add space before type provider params``. [#859](https://github.com/fsprojects/fantomas/issues/859)
* Incorrect end of line added after "(" which makes the code not to compile. [#856](https://github.com/fsprojects/fantomas/issues/856)
* Incorrect end of line added after "(". [#855](https://github.com/fsprojects/fantomas/issues/855)
* SpaceBeforeUppercaseInvocation applied in the middle of a invocation chain. [#853](https://github.com/fsprojects/fantomas/issues/853)
* MultilineBlockBracketsOnSameColumn not working properly when calling base constructors. [#852](https://github.com/fsprojects/fantomas/issues/852)
* PageWidth not respected for member with one long parameter. [#850](https://github.com/fsprojects/fantomas/issues/850)
* Wrong indentation in member definition. [#844](https://github.com/fsprojects/fantomas/issues/844)
* Class type with long variable names results in invalid formatted F# code. [#841](https://github.com/fsprojects/fantomas/issues/841)
* Multiline let bang should have newline before. [#838](https://github.com/fsprojects/fantomas/issues/838)
* complex computation expression identifier looks off. [#835](https://github.com/fsprojects/fantomas/issues/835)
* keyword before type declaration leads to invalid F# code. [#830](https://github.com/fsprojects/fantomas/issues/830)
* Inconsistent if-then-else cut. [#825](https://github.com/fsprojects/fantomas/issues/825)
* MultilineBlockBracketsOnSameColumn=true not working on records with short names. [#823](https://github.com/fsprojects/fantomas/issues/823)
* --config fantomas-config.json gives error. [#821](https://github.com/fsprojects/fantomas/issues/821)
* multiline let bang should have a newline. [#819](https://github.com/fsprojects/fantomas/issues/819)
* Updated value not indented correctly. [#817](https://github.com/fsprojects/fantomas/issues/817)
* Comment removed in multi-case pattern matching. [#813](https://github.com/fsprojects/fantomas/issues/813)
* Wrong handling multi lines comment at the end of file after function application. [#810](https://github.com/fsprojects/fantomas/issues/810)
* Opening brace for test missing. [#806](https://github.com/fsprojects/fantomas/issues/806)
* Return attribute deleted on reformatting. [#800](https://github.com/fsprojects/fantomas/issues/800)
* Fantomas crash with evaluation of array member. [#798](https://github.com/fsprojects/fantomas/issues/798)
* Type restrictions in FSI files. [#797](https://github.com/fsprojects/fantomas/issues/797)
* AssemblyInfo.fs attributes get squashed together. [#796](https://github.com/fsprojects/fantomas/issues/796)
* Byte-order mark is stripped. [#795](https://github.com/fsprojects/fantomas/issues/795)
* Fantomas replaces "abstract" in fsi, leading to compile errors. [#794](https://github.com/fsprojects/fantomas/issues/794)
* Broken links in Readme.md. [#791](https://github.com/fsprojects/fantomas/issues/791)
* Multiline first member should not introduce initial newline. [#789](https://github.com/fsprojects/fantomas/issues/789)
* Newline added before let binding with attribute in class. [#786](https://github.com/fsprojects/fantomas/issues/786)
* Some floating-point numbers are changed. [#785](https://github.com/fsprojects/fantomas/issues/785)
* Adding newline before first comment in module. [#784](https://github.com/fsprojects/fantomas/issues/784)
* Parameter after multiline string parameter. [#783](https://github.com/fsprojects/fantomas/issues/783)
* Modulo operator misplaced. [#780](https://github.com/fsprojects/fantomas/issues/780)
* double-backtick identifier is formatted wrong when starts with non-alphanum character. [#776](https://github.com/fsprojects/fantomas/issues/776)
* Line comment after record not printed. [#774](https://github.com/fsprojects/fantomas/issues/774)
* Additional blank lines inserted after formatting. [#772](https://github.com/fsprojects/fantomas/issues/772)
* Error while formatting Fantomas unit test with compiler define. [#761](https://github.com/fsprojects/fantomas/issues/761)
* AbstractSlot with line comment is consider multi line. [#757](https://github.com/fsprojects/fantomas/issues/757)
* Missing space after multiline string. [#754](https://github.com/fsprojects/fantomas/issues/754)
* Cannot determine upper or lowercase. [#753](https://github.com/fsprojects/fantomas/issues/753)
* Feature: Add blank line between type definition and members. [#752](https://github.com/fsprojects/fantomas/issues/752)
* Default member implementation changed to member during formatting. [#742](https://github.com/fsprojects/fantomas/issues/742)
* Long function definition should put equals and body on a newline. [#740](https://github.com/fsprojects/fantomas/issues/740)
* Add extra space between prefix operator and string. [#736](https://github.com/fsprojects/fantomas/issues/736)
* MaxIfThenElseShortWidth is not respected. [#734](https://github.com/fsprojects/fantomas/issues/734)
* Shouldn't remove getters. [#733](https://github.com/fsprojects/fantomas/issues/733)
* Comment after `then` keyword gets removed. [#730](https://github.com/fsprojects/fantomas/issues/730)
* Determine if DotGet expression is upper- or lowercase. [#729](https://github.com/fsprojects/fantomas/issues/729)
* Check for Trivia content before the equals sign in let bindings. [#728](https://github.com/fsprojects/fantomas/issues/728)
* When advising user to file a bug, should mention the file it was trying to format. [#726](https://github.com/fsprojects/fantomas/issues/726)
* space removed from parameters passed to inherited class. [#720](https://github.com/fsprojects/fantomas/issues/720)
* Place parameters on a new line for very long member definitions. [#719](https://github.com/fsprojects/fantomas/issues/719)
* Exception: Unexpected scenario when formatting else if / elif. [#713](https://github.com/fsprojects/fantomas/issues/713)
* Fantomas keeps adding newlines every time you format. [#709](https://github.com/fsprojects/fantomas/issues/709)
* Duplicate spaces and lost of linecomment. [#687](https://github.com/fsprojects/fantomas/issues/687)
* Formatting of array literals of BigInteger. [#682](https://github.com/fsprojects/fantomas/issues/682)
* Hash directive not between namespace and module. [#681](https://github.com/fsprojects/fantomas/issues/681)
* Comment above static member is wrongly placed. [#680](https://github.com/fsprojects/fantomas/issues/680)
* Do not remove property setters. [#664](https://github.com/fsprojects/fantomas/issues/664)
* StringConstant printed twice. [#646](https://github.com/fsprojects/fantomas/issues/646)
* Newline after "bang" keywords in computation expressions. [#615](https://github.com/fsprojects/fantomas/issues/615)
* Incorrect indentation when folding a record update expression. [#536](https://github.com/fsprojects/fantomas/issues/536)
* Preserve comments after record. [#516](https://github.com/fsprojects/fantomas/issues/516)
* Long function signature broken into two lines. [#492](https://github.com/fsprojects/fantomas/issues/492)
* "Better" support for nesting complex expressions in async { } blocks. [#386](https://github.com/fsprojects/fantomas/issues/386)

## [3.3.0] - 2020-02-28

### Changed
* Support for `and!`. [#690](https://github.com/fsprojects/fantomas/issues/690)
* Support for new slice syntax. [#691](https://github.com/fsprojects/fantomas/issues/691)
* Support for check style flag [#642](https://github.com/fsprojects/fantomas/issues/642)
* Update FCS to 34.1 [#699](https://github.com/fsprojects/fantomas/pull/699)
* Allow to configure spaces before and after semicolon. [#653](https://github.com/fsprojects/fantomas/issues/653)
* Update README with link to YouTube videos series. [#672](https://github.com/fsprojects/fantomas/pull/672)

### Fixed
* Problem with --config and directory names containing ".". [#694](https://github.com/fsprojects/fantomas/issues/694)
* Space is removed after Foo.Create. [#676](https://github.com/fsprojects/fantomas/issues/676)
* Error in formatting nested else if construction. [#675](https://github.com/fsprojects/fantomas/issues/675)
* Unbalanced and misplaced #if directives after formatting. [#635](https://github.com/fsprojects/fantomas/issues/635)
* Stack overflow when using fantomas 3.2.0-beta-002. [#630](https://github.com/fsprojects/fantomas/issues/630)
* --help and --version return exit code 1. [#612](https://github.com/fsprojects/fantomas/issues/612)
* Line comment disappears after format. [#598](https://github.com/fsprojects/fantomas/issues/598)
* Stack overflow for global tool on OSX. [#591](https://github.com/fsprojects/fantomas/issues/591)
* Page width is not respected when formatting a function signature. [#495](https://github.com/fsprojects/fantomas/issues/495)

## [3.2.0] - 2020-02-03

### Changed
* Added support for settings configuration file. [#354](https://github.com/fsprojects/fantomas/issues/354)
* Use Argu for commandline argument parsing. [#607](https://github.com/fsprojects/fantomas/pull/607)

### Fixed
* Unicode null escapes are *still* unescaped. [#632](https://github.com/fsprojects/fantomas/issues/632)
* Back ticks are removed from enum. [#626](https://github.com/fsprojects/fantomas/issues/626)
* Pipe is removed when DU type name matches record type name. [#641](https://github.com/fsprojects/fantomas/issues/641)
* fantomas --version should return version. [#625](https://github.com/fsprojects/fantomas/issues/625)
* Extra newline between attribute and function. [#611](https://github.com/fsprojects/fantomas/issues/611)
* Invalid code produced when formatting type alias for struct tuple. [#605](https://github.com/fsprojects/fantomas/issues/605)
* Extra newlines repeatedly being added inside an object expression. [#601](https://github.com/fsprojects/fantomas/issues/601)
* Empty line added on each format. [#597](https://github.com/fsprojects/fantomas/issues/597)
* Error when formatting DU with single choice and attribute. [#596](https://github.com/fsprojects/fantomas/issues/596)
* Unwanted new line after elif expression. [#588](https://github.com/fsprojects/fantomas/issues/588)
* Unwanted new line added. [#586](https://github.com/fsprojects/fantomas/issues/586)
* Empty lines in multi-line string get moved. [#577](https://github.com/fsprojects/fantomas/issues/577)
* Error when combining #if directive with async block and let. [#576](https://github.com/fsprojects/fantomas/issues/576)
* DllImport not detected when using additional attribute. [#574](https://github.com/fsprojects/fantomas/issues/574)
* Comment in async block gets moved. [#573](https://github.com/fsprojects/fantomas/issues/573)
* Enum comments removed. [#572](https://github.com/fsprojects/fantomas/issues/572)
* Fantomas keeps adding new lines between two interface member implementations. [#569](https://github.com/fsprojects/fantomas/issues/569)
* Unindented DU case causes compile error. [#567](https://github.com/fsprojects/fantomas/issues/567)
* Erroneous whitespace in chained accessors. [#566](https://github.com/fsprojects/fantomas/issues/566)
* Comments inside type signatures break formatting. [#565](https://github.com/fsprojects/fantomas/issues/565)
* Hash symbol in signatures requires parens to remain. [#564](https://github.com/fsprojects/fantomas/issues/564)
* Stack overflow in Strict mode. [#562](https://github.com/fsprojects/fantomas/issues/562)
* Accessibility modifiers in DUs. [#561](https://github.com/fsprojects/fantomas/issues/561)
* Line comment place after lambda instead of infix function. [#559](https://github.com/fsprojects/fantomas/issues/559)
* Sequence expression inside computation expression outputs uncompilable code. [#553](https://github.com/fsprojects/fantomas/issues/553)
* Comment after [ is not preserved. [#551](https://github.com/fsprojects/fantomas/issues/551)
* Record update indentation incorrect around comments. [#537](https://github.com/fsprojects/fantomas/issues/537)
* Formatting document continuously adds new lines each time it's called. [#535](https://github.com/fsprojects/fantomas/issues/535)
* Comments like `(fun arg -> // comment` are lost. [#534](https://github.com/fsprojects/fantomas/issues/534)
* KeepNewlineAfter not respected in let binding. [#524](https://github.com/fsprojects/fantomas/issues/524)
* Improve formatting of lambda between parenthesis. [#523](https://github.com/fsprojects/fantomas/issues/523)
* Crash when using --keepNewlineAfter. [#513](https://github.com/fsprojects/fantomas/issues/513)
* Over-aggresive folding breaks nested lambda expressions. [#486](https://github.com/fsprojects/fantomas/issues/486)
* Add FormatASTRangeAsync to API. [#454](https://github.com/fsprojects/fantomas/issues/454)
* Intrinsic type extension member signatures are erased. [#413](https://github.com/fsprojects/fantomas/issues/413)
* Inconsistencies in if formatting. [#135](https://github.com/fsprojects/fantomas/issues/135)

## [3.1.0] - 2019-11-27

### Fixed
* invalid code generated after multiline string when other expressions exist on same line. [#545](https://github.com/fsprojects/fantomas/issues/545)
* Trivia before elif generates invalid code due to missing indentation. [#527](https://github.com/fsprojects/fantomas/issues/527)
* Don't add additional newline between two and blocks. [#520](https://github.com/fsprojects/fantomas/issues/520)
* Print line comment after `{` [#517](https://github.com/fsprojects/fantomas/issues/517)
* Formatting document removes '#if DEBUG' and '#endif'. [#512](https://github.com/fsprojects/fantomas/issues/512)
* Some unicode control characters are incorrectly formatted. [#506](https://github.com/fsprojects/fantomas/issues/506)
* New empty line inserted preceding module attribute. [#505](https://github.com/fsprojects/fantomas/issues/505)
* Weird indentation/breaks with lambda in pipeline. [#503](https://github.com/fsprojects/fantomas/issues/503)
* Sufficiently indent match case bodies for other indentation lengths than 4. [#502](https://github.com/fsprojects/fantomas/issues/502)
* `--noSpaceBeforeColon` doesn't work. [#499](https://github.com/fsprojects/fantomas/issues/499)
* Invalid code produced when wrapping method call to new line. [#498](https://github.com/fsprojects/fantomas/issues/498)
* Indexer usage fails to parse. [#497](https://github.com/fsprojects/fantomas/issues/497)

### Changed
* Use FCS 33.0.0. [pull/568](https://github.com/fsprojects/fantomas/pull/568)
* Use dotnet tools [pull/558](https://github.com/fsprojects/fantomas/pull/558)
* Add `--maxIfThenElseShortWidth` option, see [documentation](https://github.com/fsprojects/fantomas/blob/master/docs/Documentation.md)

## [3.0.0] - 2019-10-11

### Changed
* Use FCS 32.0.0. [490b121af427ec4f6eba94f6d6d08cf3f91e04c8](https://github.com/fsprojects/fantomas/pull/434/commits/490b121af427ec4f6eba94f6d6d08cf3f91e04c8)
* Deprecate PreserveEndOfLine feature. [#390](https://github.com/fsprojects/fantomas/issues/390)
* Upgrade to .NET Core 3.0 and deprecate dotnet-fantomas. [b13aa00a57541be5f6182dc65ee27dc81174ab15](https://github.com/fsprojects/fantomas/pull/434/commits/b13aa00a57541be5f6182dc65ee27dc81174ab15)
* F# 4.7 support. [9ab8f007446d2e8311a204a9c8a73d758a189939](https://github.com/fsprojects/fantomas/pull/434/commits/9ab8f007446d2e8311a204a9c8a73d758a189939)
* KeepNewlineAfter setting. [#449](https://github.com/fsprojects/fantomas/issues/449)
* Refactored API [#454](https://github.com/fsprojects/fantomas/issues/454)

### Fixed
* Adding parentheses around expressions can lead to incorrect indentation. [#385](https://github.com/fsprojects/fantomas/issues/385)
* Indentation removed when combining comments and compiler directives. [#382](https://github.com/fsprojects/fantomas/issues/382)
* Fantomas removes module and namespace if it is only 1 word (without dots). [#380](https://github.com/fsprojects/fantomas/issues/380)
* Indentation incorrect for code with chained fluent interface method calls. [#379](https://github.com/fsprojects/fantomas/issues/379)
* Incorrect indentation when space around delimiter is false. [#362](https://github.com/fsprojects/fantomas/issues/362)
* Meaningful spaces can be collapsed in record-with expressions. [#353](https://github.com/fsprojects/fantomas/issues/353)
* CLI arguments not accepted. [#334](https://github.com/fsprojects/fantomas/issues/334)
* Calls to constructor from inherited class leads to wrong indentation (breaks build). [#326](https://github.com/fsprojects/fantomas/issues/326)
* Indent level context lost in record initialization -- causes compilation failure. [#313](https://github.com/fsprojects/fantomas/issues/313)
* Semi-colons may or may not be insterted in list literals. [#312](https://github.com/fsprojects/fantomas/issues/312)
* Handling of blank lines is idiosyncratic. [#311](https://github.com/fsprojects/fantomas/issues/311)
* Over-length line not folded (inside #if block). [#309](https://github.com/fsprojects/fantomas/issues/309)
* With --preserverEOL, multi-line lambdas are not correctly formatted. [#307](https://github.com/fsprojects/fantomas/issues/307)
* Reformatting #if blocks controlling attributes changes the meaning of the code. [#304](https://github.com/fsprojects/fantomas/issues/304)
* Some spacing is still lost in and around #if blocks. [#303](https://github.com/fsprojects/fantomas/issues/303)
* Weird formattiing behavior. [#287](https://github.com/fsprojects/fantomas/issues/287)
* #if blocks result in code being moved around incorrectly. [#282](https://github.com/fsprojects/fantomas/issues/282)
* Inline replacement. [#278](https://github.com/fsprojects/fantomas/issues/278)
* No new line after long name when copying record with "with". [#155](https://github.com/fsprojects/fantomas/issues/155)
* Formatting of multi-line list literals. [#133](https://github.com/fsprojects/fantomas/issues/133)
* Problems with very long lines and/or files. [#119](https://github.com/fsprojects/fantomas/issues/119)
* Adjust default configuration to be more F# idiomatic. [#61](https://github.com/fsprojects/fantomas/issues/61)
* Excessive line breaking. [#43](https://github.com/fsprojects/fantomas/issues/43)
* [Trivia] Line comment after `then` breaks code. [#451](https://github.com/fsprojects/fantomas/issues/451)
* Bug report from fantomas-ui. [#450](https://github.com/fsprojects/fantomas/issues/450)
* Publish 3.0.0 to NuGet.org with a preview flag. [#448](https://github.com/fsprojects/fantomas/issues/448)
* Include directive with `__SOURCE_DIRECTORY__` is removed and replace. [#447](https://github.com/fsprojects/fantomas/issues/447)
* Formatting if expressions not according to style-guide. [#446](https://github.com/fsprojects/fantomas/issues/446)
* PreserveEndOfLine+SpaceAroundDelimiter add an unnecessary space before closing brace. [#443](https://github.com/fsprojects/fantomas/issues/443)
* Record option with attribute gets an additional space with PreserveEndOfLine. [#442](https://github.com/fsprojects/fantomas/issues/442)
* Quotation escapes removed - Bug report from fantomas-ui. [#440](https://github.com/fsprojects/fantomas/issues/440)
* Fantomas fails in Fake script. [#439](https://github.com/fsprojects/fantomas/issues/439)
* Configuration options for "Fabulous compatibility"?. [#437](https://github.com/fsprojects/fantomas/issues/437)
* Using fantomas with dotnet-format. [#430](https://github.com/fsprojects/fantomas/issues/430)
* Change space before colon default to false. [#429](https://github.com/fsprojects/fantomas/issues/429)
* global.json specifies outdated dotnet sdk. [#426](https://github.com/fsprojects/fantomas/issues/426)
* Errors after formatting secondary constructors. [#423](https://github.com/fsprojects/fantomas/issues/423)
* Wrong attribute and xml doc placement on reformat with PreserveEOL. [#422](https://github.com/fsprojects/fantomas/issues/422)
* When running the tool for a fairly large script file (1000 lines) nothing happens. [#416](https://github.com/fsprojects/fantomas/issues/416)
* Is Fantomas still supported for VS? Couldn't find it?. [#415](https://github.com/fsprojects/fantomas/issues/415)
* the required library libhostfxr.so could not be found. [#412](https://github.com/fsprojects/fantomas/issues/412)
* Latest FCS breaks fantomas. [#410](https://github.com/fsprojects/fantomas/issues/410)
* Compiled operators names are replaced with source names. [#409](https://github.com/fsprojects/fantomas/issues/409)
* Wrong anon module formatting when filename starts with a digit. [#408](https://github.com/fsprojects/fantomas/issues/408)
* Raw method names with `/` are formatted improperly. [#406](https://github.com/fsprojects/fantomas/issues/406)
* Attributes followed by unit literals aren't formatted properly. [#405](https://github.com/fsprojects/fantomas/issues/405)
* Wrongly removed "with" for member on record, PreserveEndOfLine=true. [#388](https://github.com/fsprojects/fantomas/issues/388)
* IndentSpaceNum is ignored if PreserveEndOfLine is enable. [#387](https://github.com/fsprojects/fantomas/issues/387)
* An option to preserve empty lines between logical blocks. [#496](https://github.com/fsprojects/fantomas/issues/496)
* Bug report from fantomas-ui. [#491](https://github.com/fsprojects/fantomas/issues/491)
* `finally` is duplicated, moved. [#487](https://github.com/fsprojects/fantomas/issues/487)
* Multiple #if cases causes failure (3.0.0 beta3 and beta4). [#484](https://github.com/fsprojects/fantomas/issues/484)
* Significant spaces lost (v3.0.0-beta4). [#483](https://github.com/fsprojects/fantomas/issues/483)
* #if/#endif lost with v3.0.0-beta-004. [#482](https://github.com/fsprojects/fantomas/issues/482)
* Exception when code for no defines is empty. [#480](https://github.com/fsprojects/fantomas/issues/480)
* Stackoverflow exception in AstTransformer. [#479](https://github.com/fsprojects/fantomas/issues/479)
* [Trivia] Significant spacing added after let binding in function. [#478](https://github.com/fsprojects/fantomas/issues/478)
* Incorrect replacement of `override` with `member`. [#477](https://github.com/fsprojects/fantomas/issues/477)
* [Trivia] Fantomas removes the parentheses around Fable's string field access syntax. [#476](https://github.com/fsprojects/fantomas/issues/476)
* [Trivia] Additional line added after very specific case. [#475](https://github.com/fsprojects/fantomas/issues/475)
* Multiple extension members cause additional lines to be printed. [#473](https://github.com/fsprojects/fantomas/issues/473)
* Long text lines cause out of range exception in 3.0.0-beta. [#472](https://github.com/fsprojects/fantomas/issues/472)
* Class member attributes cause additional lines. [#471](https://github.com/fsprojects/fantomas/issues/471)
* 'with' incorrectly removed. [#469](https://github.com/fsprojects/fantomas/issues/469)
* Online UI tool doesn't understand F# 4.6's {| |}. [#467](https://github.com/fsprojects/fantomas/issues/467)
* Exception handling 'with' clause using drop-through is malformed. [#465](https://github.com/fsprojects/fantomas/issues/465)
* Unicode null escapes are unescaped (v3.0.0. beta1, beta2). [#464](https://github.com/fsprojects/fantomas/issues/464)
* Multiline record not on new line after DU constructor. [#462](https://github.com/fsprojects/fantomas/issues/462)
* Feature request: Prefix generic type parameters. [#460](https://github.com/fsprojects/fantomas/issues/460)
* Fantomas hangs indefinitely when run. [#459](https://github.com/fsprojects/fantomas/issues/459)
* record mutation: first field after `with` should be placed in a new line. [#457](https://github.com/fsprojects/fantomas/issues/457)
* for i in 1..-1..0 do: should add space before `-`. [#456](https://github.com/fsprojects/fantomas/issues/456)
* Incorrect handling of attributes in static method. [#452](https://github.com/fsprojects/fantomas/issues/452)

## [3.0.0-beta-006] - 2019-10-02

### Changed
* FCS 32
* Partial F# 4.7 support

## [3.0.0-beta-005] - 2019-09-27

### Changed
* Move to .NETCore 3 global tool, deprecated net461
* Deprecated dotnet-fantomas tool
* Bug fixes

## [3.0.0-beta-004] - 2019-09-18

### Changed
* Restructured library API
* Improved performance

## [3.0.0-beta-003] - 2019-09-13

### Changed
* More trivia fixes
* FCS 31

## [3.0.0-beta-002] - 2019-07-20

### Changed
* More fixes with hash directives

## [3.0.0-beta-001] - 2019-07-12

### Changed
* Use FCS 28.0.0 and net461. [#436](https://github.com/fsprojects/fantomas/pull/436)
* Deprecated `PreserveEndOfLine` setting in favor of Trivia. [#434](https://github.com/fsprojects/fantomas/pull/434)
* Added support for formatting multiple code path in defines.

## [2.9.2] - 2019-02-02

### Fixed
* PreserveEndOfLine introduces additional newlines. [#360](https://github.com/fsprojects/fantomas/issues/360)
* Extra newline is introduced when file ends with multiline comment. [#363](https://github.com/fsprojects/fantomas/issues/363)
* Fantomas shouldn't remove parens when using the dynamic operator (?). [#369](https://github.com/fsprojects/fantomas/issues/369)
* Extra semicolons in list with PreserveEndOfLine. [#371](https://github.com/fsprojects/fantomas/issues/371)
* Multiple attributes indented wrongly with PreserveEndOfLine. [#370](https://github.com/fsprojects/fantomas/issues/370)
* pattern matched unions are formatted badly. [#283](https://github.com/fsprojects/fantomas/issues/283)
* wrong indentation when accessing member of constructed record. [#383](https://github.com/fsprojects/fantomas/issues/383)
* latest fantomas breaks Falanx indentation. [#384](https://github.com/fsprojects/fantomas/issues/384)
* Recurse option with globally installed dotnet-tool traverses 'obj' directories. [#341](https://github.com/fsprojects/fantomas/issues/341)
* creates invalid F# for string handling operations. [#365](https://github.com/fsprojects/fantomas/issues/365)

### Changed
* Use qualified name for inputPath and outputPath. [#376](https://github.com/fsprojects/fantomas/pull/376)
* Added Nightly nuget feed. [#375](https://github.com/fsprojects/fantomas/pull/375)
* Moved solution file to root folder. [#377](https://github.com/fsprojects/fantomas/pull/377)
* Add support for SynExpr.Set(_,_,_). [#368](https://github.com/fsprojects/fantomas/issues/368)
* Use FAKE 5. [#261](https://github.com/fsprojects/fantomas/issues/261)
* Added FAKE 5 sample. [#402](https://github.com/fsprojects/fantomas/issues/402)

## [2.9.1] - 2018-11-20

### Changed
* Added instructions for vscode and online website. [#333](https://github.com/fsprojects/fantomas/pull/333)
* Removed trailing spaces for each line, after formatting. [#328](https://github.com/fsprojects/fantomas/issues/328)
* Allow easy build/format/build cycle of external projects. [#337](https://github.com/fsprojects/fantomas/pull/337)
* Update to .NET Core 2.1 [#350](https://github.com/fsprojects/fantomas/issues/350)
* Removed unused open statements. [#352](https://github.com/fsprojects/fantomas/pull/352)
* Added regression test for Implicit module is added to resulting code. [#355](https://github.com/fsprojects/fantomas/pull/355)

### Fixed
* `in` is removed from binding when PreserveEndOfLine is true. [#340](https://github.com/fsprojects/fantomas/issues/340)
* unnecessary conversion from 'YieldOrReturn' to 'YieldOrReturnFrom', by update of FCS. [#339](https://github.com/fsprojects/fantomas/issues/339)
* Lazy<'T> is incorrectly rewritten. [#335](https://github.com/fsprojects/fantomas/issues/335)
* Fluent API with comments breaks code. [#331](https://github.com/fsprojects/fantomas/issues/331)
* wrong comment placement. [#289](https://github.com/fsprojects/fantomas/issues/289)

## [2.9.0] - 2018-10-17

### Changed
* Improved README. [#243](https://github.com/fsprojects/fantomas/issues/243)
* Bad split of chained method call expression. [#246](https://github.com/fsprojects/fantomas/issues/246)
* rec modifier removed for namespaces and modules. [#292](https://github.com/fsprojects/fantomas/issues/292)
* Over-enthusiastic removal of parentheses [#249](https://github.com/fsprojects/fantomas/issues/249)
* Broken reformat of "if" inside call (fantomas-tool 2.8.0) [#288](https://github.com/fsprojects/fantomas/issues/288)
* Support struct tuple. [#224](https://github.com/fsprojects/fantomas/issues/224)
* Support match! [#262](https://github.com/fsprojects/fantomas/issues/262)
* Upgrade to .NET 4.5.2 [#325](https://github.com/fsprojects/fantomas/pull/325)

### Fixed
* for AST formatting regression. [#321](https://github.com/fsprojects/fantomas/issues/321)

## [2.8.1] - 2018-09-12

### Changed
* Force parameter is *true* by default. [#267](https://github.com/fsprojects/fantomas/issues/267)
* Formatting compiler directives with inactive code is incorrect. [#270](https://github.com/fsprojects/fantomas/issues/270)
* rec keyword is removed from recursive modules [#274](https://github.com/fsprojects/fantomas/issues/274)
* Access modifiers in method signatures in signature files are not formatted correctly. [#284](https://github.com/fsprojects/fantomas/issues/284)
* `#if FOO || BAR => #if FOO` [#280](https://github.com/fsprojects/fantomas/issues/280)
* `override` becomes `member` in interface implementations. [#263](https://github.com/fsprojects/fantomas/issues/263)
* Operator >>.~ incorrectly formatted. [#291](https://github.com/fsprojects/fantomas/issues/291)
* Bad choice of line break location in boolean equality expression. [#248](https://github.com/fsprojects/fantomas/issues/248)
* Pipe operator inside quotation expression leads to wrong indentation. [#256](https://github.com/fsprojects/fantomas/issues/256)
* broken indent by pipe formatting. [#269](https://github.com/fsprojects/fantomas/issues/269)

### Fixed
* "Fantômas" mistranslation in README. [#273](https://github.com/fsprojects/fantomas/pull/273)
* for preserve EOL feature. [#275](https://github.com/fsprojects/fantomas/pull/275)

## [2.8.0] - 2018-07-07

### Changed
* Wrong indentation of `else` after comment [#241](https://github.com/dungpa/fantomas/issues/241)
* Change Content to None [#238](https://github.com/dungpa/fantomas/issues/238)
* Formatting of code with a pipe and a lambda expression [#211](https://github.com/dungpa/fantomas/issues/211)
* Added support for a global dotnet cli tool [#252](https://github.com/dungpa/fantomas/issues/252)
* Added option to preserve blank lines [#143](https://github.com/dungpa/fantomas/issues/143)

### Fixed
* for chopped of members [#239](https://github.com/dungpa/fantomas/issues/239)

## [2.7.1] - 2018-05-02

### Changed
* Hotfix for runtime problem when using dotnet cli tool

## [2.7.0] - 2018-05-02

### Changed
* Upgrade to .NET Core 2.0
* Published as `clitool`
* Upgrade to FCS 22.0.3
* Single case DUs on same line [#234](https://github.com/dungpa/fantomas/pull/234)
* Removed whitespaces around type provider arguments [#235](https://github.com/dungpa/fantomas/pull/235)

## [2.6.1] - 2017-04-22

### Changed
* Upgrade to FCS 11.0.4

## [2.5.0] - 2017-02-19

### Changed
* Upgrade to FCS 10.0.0

## [2.4.0] - 2016-10-24

### Changed
* Upgrade to FCS 8.0.0

## [2.3.0] - 2016-07-09

### Changed
* Upgrade to FCS 5.0.0

## [2.2.0] - 2016-04-24

### Changed
* Handle record types with private fields [#197](https://github.com/dungpa/fantomas/pull/197)
* Create a separate CLI NuGet package [#196](https://github.com/dungpa/fantomas/pull/196)
* Do not print out module names if not necessary [#196](https://github.com/dungpa/fantomas/pull/196)

## [2.1.0] - 2016-04-01

### Changed
* Upgrade to FCS 2.0.0.8

## [2.0.2] - 2015-11-15

### Changed
* Add a new public API using static members. Deprecate old functions.

### Fixed
* https://github.com/fsprojects/VisualFSharpPowerTools/issues/1151
* https://github.com/fsprojects/VisualFSharpPowerTools/issues/1143

## [1.11.0] - 2015-09-12

### Fixed
* https://github.com/fsprojects/VisualFSharpPowerTools/issues/366 [#177](https://github.com/dungpa/fantomas/pull/177)

### Changed
* Migrate to FCS 1.4.0.5

## [1.10.0] - 2015-08-29

### Changed
Improve formatting of bind operator [#175](https://github.com/dungpa/fantomas/pull/175)

## [1.9.0] - 2015-08-10

### Fixed
* https://github.com/fsprojects/VisualFSharpPowerTools/issues/1050 ([#172](https://github.com/dungpa/fantomas/pull/172))

## [1.8.0-beta] - 2015-07-19

### Changed
* Migrate to F# 4.0 ([#170](https://github.com/dungpa/fantomas/pull/170))

## [1.7.0] - 2015-06-10

### Changed
* Print attributes on member arguments ([#168](https://github.com/dungpa/fantomas/pull/168))
* Do not misrecognize "then" blocks in explicit constructors ([#168](https://github.com/dungpa/fantomas/pull/168))
* Suppress whitespaces inside dot access ([#168](https://github.com/dungpa/fantomas/pull/168))
* Insert brackets around tuples in type test patterns ([#168](https://github.com/dungpa/fantomas/pull/168))

### Fixed
* desugar patterns' bug exposed by FsCheck ([#167](https://github.com/dungpa/fantomas/pull/167))

## [1.6.0] - 2014-10-25

### Changed
* Add FAKE task helper

## [1.5.0] - 2014-09-18

### Changed
* Bugfix release

## [1.4.0] - 2014-07-01

### Changed
* Bugfix release

## [1.3.0] - 2014-05-17

### Changed
* Bugfix release

## [1.2.0] - 2014-04-21

### Changed
* Bugfix release

## [1.1.0] - 2014-03-29

### Changed
* Bugfix release

## [1.0.5] - 2014-01-07

### Changed
* Fully support F# 3.1
* Compatible with F# on Mono
* Handle external functions
* Improve support of multiline strings
* Implement various bug fixes

## [1.0.4] - 2013-11-16

### Changed
* Implement various bug fixes

## [1.0.3] - 2013-10-06

### Changed
* Implement various bug fixes
* Synchronize version numbers with NuGet packages

## [0.7.1] - 2013-03-24

### Changed
* Support Visual Studio 2013 (not support F# 3.1 constructs yet)

## [0.7.0] - 2013-03-24

### Changed
* Implement formatting cursor positions
* Implement reordering of open statements
* Enhance indentation of anonymous functions
* Add line breaks for nested let bindings
* Implement various bug fixes

## [0.5.0] - 2013-03-22

### Changed
* Improve formatting of signatures
* Improve UI interaction
* Enhance spacing of function applications and arguments
* Implement various bug fixes

## [0.4.1] - 2013-03-22

### Changed
* Initial release