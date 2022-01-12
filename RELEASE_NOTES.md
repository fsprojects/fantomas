#### 4.5.12 - 01/2022

* Fix Strings containing spaces at end of line change meaning. [#1941](https://github.com/fsprojects/fantomas/issues/1941)
* Fix Explicit class/end/with loses members. [#1940](https://github.com/fsprojects/fantomas/issues/1940)
* Fix Idempotency problem when static member with get. [#1913](https://github.com/fsprojects/fantomas/issues/1913)

#### 4.5.11 - 12/2021

* Fix KeepIndentInBranch not being respected. [#2003](https://github.com/fsprojects/fantomas/issues/2003)

#### 4.5.10 - 12/2021

* Fix Fantomas writes even when not necessary. [#1984](https://github.com/fsprojects/fantomas/issues/1984)

#### 4.5.9 - 11/2021

* Fix Operator application to some literals doesn't preserve spacing. [#1979](https://github.com/fsprojects/fantomas/issues/1979)

#### 4.5.8 - 11/2021

* Fix Fantomas is unable to format valid F# (.net 6.0) program. [#1969](https://github.com/fsprojects/fantomas/issues/1969)
* Fix Attributes on static members of recursive types formats incorrectly. [#1962](https://github.com/fsprojects/fantomas/issues/1962)
* Fix val mutable in signature loses 'mutable'. [#1954](https://github.com/fsprojects/fantomas/issues/1954)
* Fix Literals in signatures lose values. [#1953](https://github.com/fsprojects/fantomas/issues/1953)
* Fix Attribute on member of mutually dependent types fails to validate. [#1918](https://github.com/fsprojects/fantomas/issues/1918)
* Fix Wrong code is generated for member attribute in recursive type. [#1898](https://github.com/fsprojects/fantomas/issues/1898)
* Fix Attribute on type function incorrectly placed for 'and' types. [#1874](https://github.com/fsprojects/fantomas/issues/1874)

#### 4.5.7 - 11/2021

* Fix Formatting power operator in code quotation pattern match fails. [#1945](https://github.com/fsprojects/fantomas/issues/1945)

#### 4.5.6 - 11/2021

* Fix Offside code created when base constructor wraps across lines. [#1942](https://github.com/fsprojects/fantomas/issues/1942)
* Fix Extra space in val and member bindings in signature files. [#1934](https://github.com/fsprojects/fantomas/issues/1934)

#### 4.5.5 - 10/2021

* Improve: Provide more information when string merge failed. [#1904](https://github.com/fsprojects/fantomas/issues/1904)
* Fix Comment gets duplicated. [#1912](https://github.com/fsprojects/fantomas/issues/1912)
* Fix Vanity alignment used when splitting line in match block. [#1901](https://github.com/fsprojects/fantomas/issues/1901)
* Fix Unexpected loss of newline in closing bracket. [#1835](https://github.com/fsprojects/fantomas/issues/1835)
* Fix Relative patterns in .fantomasignore don't match any files. [#1713](https://github.com/fsprojects/fantomas/issues/1713)

#### 4.5.4 - 10/2021

* Fix Documentation contains reference to infix multiline formatter even though it's removed. [#1884](https://github.com/fsprojects/fantomas/issues/1884)
* Fix Newline was introduced in Sequential with LetBang. [#1882](https://github.com/fsprojects/fantomas/issues/1882)
* Fix Vanity alignment used when calling base constructor. [#1442](https://github.com/fsprojects/fantomas/issues/1442)

#### 4.5.3 - 09/2021

* Fix indented #if directive inside another non-indented #if directive drops significant whitespace. [#1866](https://github.com/fsprojects/fantomas/issues/1866)

#### 4.5.2 - 08/2021

* Fix Dropped comment with function with type parameter. [#1861](https://github.com/fsprojects/fantomas/issues/1861)
* Fix Comment dropped in a multi-option match. [#1855](https://github.com/fsprojects/fantomas/issues/1855)
* Fix StackoverflowException when formatted long triple-quoted strings. [#1837](https://github.com/fsprojects/fantomas/issues/1837)
* Fix Comment removed in match. [#1677](https://github.com/fsprojects/fantomas/issues/1677)

#### 4.5.1 - 07/2021

* Fix StackOverflow exceptions when collecting ColMultilineItem list. [#1839](https://github.com/fsprojects/fantomas/issues/1839)
* Honor .fantomasignore file when processing a folder. [#1834](https://github.com/fsprojects/fantomas/pull/1834)
* Fix Overly indented members on a record type with accessibility modifier. [#1824](https://github.com/fsprojects/fantomas/issues/1824)
* Fix MultiLineLambdaClosingNewline not respected with function keyword. [#1823](https://github.com/fsprojects/fantomas/issues/1823)
* Fix Comment is lost at the end of a match. [#1822](https://github.com/fsprojects/fantomas/issues/1822)

#### 4.5.0 - 07/2021

* Feature Always place bar in front of discriminated union. [#1750](https://github.com/fsprojects/fantomas/issues/1750)
* Feature Support multiple files/dirs as command-line arguments. [#1696](https://github.com/fsprojects/fantomas/issues/1696)
* Feature Disable empty line between mutually recursive type. [#1658](https://github.com/fsprojects/fantomas/issues/1658)
* Feature BlankLinesAroundNestedMultilineExpressions. [#1587](https://github.com/fsprojects/fantomas/pull/1587)
* Initial support of KeepIndentInBranch. [#1361](https://github.com/fsprojects/fantomas/issues/1361)
* Improve: Collect empty define block as single trivia. [#1528](https://github.com/fsprojects/fantomas/pull/1528)
* Improve: Refactor ASTTransformer. [#1497](https://github.com/fsprojects/fantomas/pull/1497)
* Improve: replace genTypeByLookup with Trivia. [#594](https://github.com/fsprojects/fantomas/issues/594)
* Update to FCS 39 [#1479](https://github.com/fsprojects/fantomas/pull/1479)
* Fix Space between identifier and then is lost. [#1816](https://github.com/fsprojects/fantomas/issues/1816)
* Fix KeepIndentInBranch causing offside error in if condition. [#1812](https://github.com/fsprojects/fantomas/issues/1812)
* Fix Indentation warnings for multiline match expression. [#1774](https://github.com/fsprojects/fantomas/issues/1774)
* Fix Extra indentation on match-case block. [#1234](https://github.com/fsprojects/fantomas/issues/1234)
* Fix Fantomas introduces meaningless match block. [#1806](https://github.com/fsprojects/fantomas/issues/1806)
* Fix Lazy causes indentation to produce invalid F#. [#1805](https://github.com/fsprojects/fantomas/issues/1805)
* Fix Nested Fluent API produces wrong code: misses indentation. [#1804](https://github.com/fsprojects/fantomas/issues/1804)
* Fix Tuple should be consider short branch in KeepIndentInBranch setting. [#1800](https://github.com/fsprojects/fantomas/issues/1800)
* Fix Fantomas adds "of" to a union case when it is seemingly _too long_. [#1796](https://github.com/fsprojects/fantomas/issues/1796)
* Fix Function application in if expression should remain on single line. [#1795](https://github.com/fsprojects/fantomas/issues/1795)
* Fix Conditional code is not printed. [#1794](https://github.com/fsprojects/fantomas/issues/1794)
* Fix Records in list in a pattern match always have a semicolon. [#1793](https://github.com/fsprojects/fantomas/issues/1793)
* Fix parenthesis expression in then should further indent inner expression. [#1777](https://github.com/fsprojects/fantomas/issues/1777)
* Fix Wild cards in lambda. [#1789](https://github.com/fsprojects/fantomas/issues/1789)
* Fix Soundness regression in 4.5.0-beta-001. Nested lambdas are stripped. [#1782](https://github.com/fsprojects/fantomas/issues/1782)
* Fix KeepIndentInBranch not respected when returning short function application. [#1779](https://github.com/fsprojects/fantomas/issues/1779)
* Fix Hash directive not printed above recursive binding. [#1776](https://github.com/fsprojects/fantomas/issues/1776)
* Fix Fantomas adds space before colon when SpaceBeforeColon is false. [#1773](https://github.com/fsprojects/fantomas/issues/1773)
* Fix Long return expression not formatted multiline. [#1771](https://github.com/fsprojects/fantomas/issues/1771)
* Fix Add support for LibraryOnlyStaticOptimization. [#1769](https://github.com/fsprojects/fantomas/issues/1769)
* Fix fsharp_keep_indent_in_branch not respected for multiline infix expression. [#1768](https://github.com/fsprojects/fantomas/issues/1768)
* Fix Inline lambda in argument list causes problems for argument chopping and lambda alignment. [#1028](https://github.com/fsprojects/fantomas/issues/1028)
* Fix Difference in behavior between signature and implementation files for single case DU. [#973](https://github.com/fsprojects/fantomas/issues/973)
* Fix Concatenation of multi-line """ strings mis-indented. [#639](https://github.com/fsprojects/fantomas/issues/639)
* Fix Comment after arrow lost with KeepIndentInBranch. [#1759](https://github.com/fsprojects/fantomas/issues/1759)
* Fix Multiline if/then/else in infix expression. [#1757](https://github.com/fsprojects/fantomas/issues/1757)
* Fix Newline and comment trivia are mixed up. [#1468](https://github.com/fsprojects/fantomas/issues/1468)
* Fix Inline lambda in argument list causes problems for argument chopping and lambda alignment. [#1028](https://github.com/fsprojects/fantomas/issues/1028)
* Fix Difference in behavior between signature and implementation files for single case DU. [#973](https://github.com/fsprojects/fantomas/issues/973)
* Fix Concatenation of multi-line """ strings mis-indented. [#639](https://github.com/fsprojects/fantomas/issues/639)
* Fix Trivia before SynPat.Paren. [#1753](https://github.com/fsprojects/fantomas/issues/1753)
* Fix Idempotency problem when trailing comment is present. [#1538](https://github.com/fsprojects/fantomas/issues/1538)
* Fix Offside error after formatting with MultiLineLambdaClosingNewline. [#1741](https://github.com/fsprojects/fantomas/issues/1741)
* Fix Pipe character missing in single-case try/with. [#1571](https://github.com/fsprojects/fantomas/issues/1571)
* Fix Multiline if condition in KeepIndentInBranch leads to warnings. [#1729](https://github.com/fsprojects/fantomas/issues/1729)
* Fix KeepIndentInBranch not respected for values. [#1728](https://github.com/fsprojects/fantomas/issues/1728)
* Fix KeepIndentInBranch not respected. [#1717](https://github.com/fsprojects/fantomas/issues/1717)
* Fix Bad interaction between KeepIndentInBranch and MultiLambdaClosingNewLine. [#1715](https://github.com/fsprojects/fantomas/issues/1715)
* Fix KeepIndentInBranch not being respected. [#1714](https://github.com/fsprojects/fantomas/issues/1714)
* Fix Breaking method chain in the middle of an if statement causes offside error. [#1712](https://github.com/fsprojects/fantomas/issues/1712)
* Fix Offside error when a comment appears before a match statement in pipeline. [#1711](https://github.com/fsprojects/fantomas/issues/1711)
* Fix Line comment after short pattern match clause causes additional parenthesis. [#1721](https://github.com/fsprojects/fantomas/issues/1721)
* Fix Appending two lists with @ fails. [#1719](https://github.com/fsprojects/fantomas/issues/1719)
* Fix Leading block comment makes type multiline. [#1718](https://github.com/fsprojects/fantomas/issues/1718)
* Fix List concat chain using operators fails to format. [#1188](https://github.com/fsprojects/fantomas/issues/1188)
* Fix Unexpected newline after inner let binding. [#1709](https://github.com/fsprojects/fantomas/issues/1709)
* Fix Currying a pair gets spread over multiple lines. [#1700](https://github.com/fsprojects/fantomas/issues/1700)
* Fix Additional parentheses are introduced when match is piped. [#1698](https://github.com/fsprojects/fantomas/issues/1698)
* Fix \t in string replaced by ASCII 9. [#1695](https://github.com/fsprojects/fantomas/issues/1695)
* Fix Idempotency problem with nested else if. [#1648](https://github.com/fsprojects/fantomas/issues/1648)
* Fix Idempotency problem with ifdef in a function. [#1646](https://github.com/fsprojects/fantomas/issues/1646)
* Fix string interpolation part crossing max line length introduces new line and lots of whitespace. [#1511](https://github.com/fsprojects/fantomas/issues/1511)
* Fix Splitting generic type parameters over multiple lines sometimes puts the break in an invalid place. [#1687](https://github.com/fsprojects/fantomas/issues/1687)
* Fix Idempotency problem related to comments in with. [#1686](https://github.com/fsprojects/fantomas/issues/1686)
* Fix Upcast requires a line break but is not given one. [#1685](https://github.com/fsprojects/fantomas/issues/1685)
* Fix Idempotency problem when trying to format a type with method chaining in it. [#1681](https://github.com/fsprojects/fantomas/issues/1681)
* Fix Line comments after null value used as argument in function call are removed. [#1676](https://github.com/fsprojects/fantomas/issues/1676)
* Fix Idempotency problem when trying to format a type with method chaining in it. [#1681](https://github.com/fsprojects/fantomas/issues/1681)
* Fix Comment after constant got moved to the next line. [#1671](https://github.com/fsprojects/fantomas/issues/1671)
* Fix Idempotency problem when ending with a comment. [#1649](https://github.com/fsprojects/fantomas/issues/1649)
* Fix end_of_line not respecting when file has ifdef. [#1673](https://github.com/fsprojects/fantomas/issues/1673)
* Fix Attributes in a recursive type get misplaced. [#1668](https://github.com/fsprojects/fantomas/issues/1668)
* Fix alternative_long_member_definitions docs. [#1666](https://github.com/fsprojects/fantomas/issues/1666)
* Fix Stack overflow on macOS for long pipelines. [#1453](https://github.com/fsprojects/fantomas/issues/1453)
* Fix Comment inside empty Elmish children is lost. [#1179](https://github.com/fsprojects/fantomas/issues/1179)
* Fix Long .Setup line in Moq code results in broken indentation. [#1662](https://github.com/fsprojects/fantomas/issues/1662)
* Fix Don't introduce parenthesis around SynPat.IsInst. [#1660](https://github.com/fsprojects/fantomas/issues/1660)
* Fix Offside error splitting long line. [#1651](https://github.com/fsprojects/fantomas/issues/1651)
* Fix Offside errors after formatting. [#1650](https://github.com/fsprojects/fantomas/issues/1650)
* Fix Idempotency problem with with block. [#1647](https://github.com/fsprojects/fantomas/issues/1647)
* Fix Formatting code makes Interpolated verbatim strings to non-verbatim strings, which breaks the code. [#1645](https://github.com/fsprojects/fantomas/issues/1645)
* Fix Comment stripped in a record with semi-colons. [#1643](https://github.com/fsprojects/fantomas/issues/1643)
* Fix Another shape which isn't respecting KeepIndentInBranch. [#1638](https://github.com/fsprojects/fantomas/issues/1638)
* Fix Incorrect code when function type parameter would break over line. [#1637](https://github.com/fsprojects/fantomas/issues/1637)
* Fix Multiline type parameter arguments inside indentation. [#1611](https://github.com/fsprojects/fantomas/issues/1611)
* Fix Fantomas add extra parenthesis in desugared lambda. [#1631](https://github.com/fsprojects/fantomas/issues/1631)
* Fix Mutually recursive functions break with function invocation above definition. [#1628](https://github.com/fsprojects/fantomas/issues/1628)
* Fix Removal of bar in one-case DU. [#1563](https://github.com/fsprojects/fantomas/issues/1563)
* Fix Multiline type signature is not unindent. [#1624](https://github.com/fsprojects/fantomas/issues/1624)
* Fix Failure to unindent with KeepIndentInBranch. [#1621](https://github.com/fsprojects/fantomas/issues/1621)
* Fix Some of newlines in string interpolation is deleted. [#1613](https://github.com/fsprojects/fantomas/issues/1613)
* Fix Idempotency problem when piping just before the keyword in. [#1610](https://github.com/fsprojects/fantomas/issues/1610)
* Fix Idempotency problem with lazy. [#1609](https://github.com/fsprojects/fantomas/issues/1609)
* Fix Idempotency issue with let … in. [#1608](https://github.com/fsprojects/fantomas/issues/1608)
* Fix Idempotency problem when splitting && over multiple lines. [#1607](https://github.com/fsprojects/fantomas/issues/1607)
* Fix Idempotency problem when splitting if-clause over multiple lines. [#1606](https://github.com/fsprojects/fantomas/issues/1606)
* Fix Idempotency problem with recursive types in FSI. [#1605](https://github.com/fsprojects/fantomas/issues/1605)
* Fix Idempotency problem with comments at the end of code. [#1604](https://github.com/fsprojects/fantomas/issues/1604)
* Fix Block comment in Elmish expression removed. [#1601](https://github.com/fsprojects/fantomas/issues/1601)
* Fix System.Exception: was not expecting token DOLLAR. [#1598](https://github.com/fsprojects/fantomas/issues/1598)
* Fix Define before opening bracket. [#1597](https://github.com/fsprojects/fantomas/issues/1597)
* Fix Swap internal and inline in signature file. [#1590](https://github.com/fsprojects/fantomas/issues/1590)
* Fix member laced with conditional. [#1589](https://github.com/fsprojects/fantomas/issues/1589)
* Fix if expression is not indented. [#1588](https://github.com/fsprojects/fantomas/issues/1588)
* Fix Fantomas throws an exception with custom operator (>??). [#1533](https://github.com/fsprojects/fantomas/issues/1533)
* Fix Unexpected identifier in lambda expression when using pre processor directives. [#1484](https://github.com/fsprojects/fantomas/issues/1484)
* Fix FormatException: Unexpected symbol '|' for DU case under pre-processor directive. [#1483](https://github.com/fsprojects/fantomas/issues/1483)
* Fix End-of-line comments lost when formatting multiline type function signature. [#1287](https://github.com/fsprojects/fantomas/issues/1287)
* Fix Comments are sometimes removed unexpectedly during formatting. [#1276](https://github.com/fsprojects/fantomas/issues/1276)
* Fix max_line_length not respected in mutliline infix expression in if. [#1584](https://github.com/fsprojects/fantomas/issues/1584)
* Fix Const() stripped from string literals break. [#1574](https://github.com/fsprojects/fantomas/issues/1574)
* Fix Conversion of & to byref is invalid in extern function declaration. [#1567](https://github.com/fsprojects/fantomas/issues/1567)
* Fix Quote character in a comment results in removing code inside preprocessor directive. [#1504](https://github.com/fsprojects/fantomas/issues/1504)
* Fix Documentation comment for primary class constructor is removed. [#1286](https://github.com/fsprojects/fantomas/issues/1286)
* Fix A comment before an anonymous function gets swallowed up. [#1190](https://github.com/fsprojects/fantomas/issues/1190)
* Fix KeepIndentInBranch not being respected?. [#1569](https://github.com/fsprojects/fantomas/issues/1569)
* Fix Split of very long function call in if body. [#1564](https://github.com/fsprojects/fantomas/issues/1564)
* Fix fsi extension loses docstring in mutually recursive type. [#1562](https://github.com/fsprojects/fantomas/issues/1562)
* Fix .fsi extension: attribute followed by docstring loses the docstring. [#1561](https://github.com/fsprojects/fantomas/issues/1561)
* Fix .fsi extension causes first DU case's docstring to be lost. [#1560](https://github.com/fsprojects/fantomas/issues/1560)
* Fix KeepIfThenInSameLine breaks function indentation. [#1559](https://github.com/fsprojects/fantomas/issues/1559)
* Fix All SynExpr should start on next line and indent. [#1556](https://github.com/fsprojects/fantomas/issues/1556)
* Fix Missing indentation when using pattern matching via anonymous functions in a pipeline. [#614](https://github.com/fsprojects/fantomas/issues/614)
* Fix Required type arguments are removed with DotGet lambda. [#1550](https://github.com/fsprojects/fantomas/issues/1550)
* Fix Fantomas removes the format string from string interpolation. [#1549](https://github.com/fsprojects/fantomas/issues/1549)
* Fix Missing in keyword. [#1548](https://github.com/fsprojects/fantomas/issues/1548)
* Fix Pattern inside when clause. [#1545](https://github.com/fsprojects/fantomas/issues/1545)
* Fix Hash defines in let binding. [#1543](https://github.com/fsprojects/fantomas/issues/1543)
* Fix Comments are stripped at the end of a vertical list. [#1541](https://github.com/fsprojects/fantomas/issues/1541)
* Fix Idempotency problem when match is follow by pipe. [#1532](https://github.com/fsprojects/fantomas/issues/1532)
* Fix Idempotency problem when exceptions in signature file. [#1531](https://github.com/fsprojects/fantomas/issues/1531)
* Fix Exception - detect of multiple defines when define surrounds a DU member. [#1503](https://github.com/fsprojects/fantomas/issues/1503)
* Fix DotGet infix expression. [#1529](https://github.com/fsprojects/fantomas/issues/1529)
* Fix SynPat.Or should have the same indent. [#1522](https://github.com/fsprojects/fantomas/issues/1522)
* Fix DotGet with parenthesis. [#1521](https://github.com/fsprojects/fantomas/issues/1521)
* Fix Trivia regressions around SynConst. [#1518](https://github.com/fsprojects/fantomas/issues/1518)
* Fix Long signatures have additonal newline inserted and don't respect the indent from the config. [#1515](https://github.com/fsprojects/fantomas/issues/1515)
* Fix Bad formatting when using elmish style + empty arrays. [#1510](https://github.com/fsprojects/fantomas/issues/1510)
* Fix \xHH escapes in string literal are expanded. [#1508](https://github.com/fsprojects/fantomas/issues/1508)
* Fix Indentation of pattern match clause. [#1501](https://github.com/fsprojects/fantomas/issues/1501)
* Fix Class parameters expands unit for long lines. [#1494](https://github.com/fsprojects/fantomas/issues/1494)
* Fix Comments inside Elmish gets repeated. [#1347](https://github.com/fsprojects/fantomas/issues/1347)
* Fix Extra space throws exception. [#1476](https://github.com/fsprojects/fantomas/issues/1476)
* Fix comment deleted on reformat. [#1343](https://github.com/fsprojects/fantomas/issues/1343)

#### 4.4.0 - 02/2021

* Revisit SynExpr.IfThenElse. [#1258](https://github.com/fsprojects/fantomas/issues/1258)
* Target netcoreapp3.1 for fantomas-tool.
* Stricter trivia selection. [#1304](https://github.com/fsprojects/fantomas/pull/1304)
* Fix Idempotency problem when function argument's type annotation requires brackets. [#1470](https://github.com/fsprojects/fantomas/issues/1470)
* Fix Inconsistency about when fantomas decides to split `()` (unit) to the next line. [#1469](https://github.com/fsprojects/fantomas/issues/1469)
* Fix Unexpected newline between hash directives. [#1464](https://github.com/fsprojects/fantomas/issues/1464)
* Fix Oscillating newlines in custom computation expression. [#1463](https://github.com/fsprojects/fantomas/issues/1463)
* Fix Violation of "avoid name-sensitive alignments" clause. [#1422](https://github.com/fsprojects/fantomas/issues/1422)
* Fix Incorrectly combines tokens when formatting. [#1407](https://github.com/fsprojects/fantomas/issues/1407)
* Fix string interpolation with multi-line string causes literal part to change. [#1451](https://github.com/fsprojects/fantomas/issues/1451)
* Fix `when` clause in try-with block gets split and causes compiler warnings about indentation. [#1406](https://github.com/fsprojects/fantomas/issues/1406)
* Fix Long line breaks match. [#1403](https://github.com/fsprojects/fantomas/issues/1403)
* Fix Long line causes offside error. [#1402](https://github.com/fsprojects/fantomas/issues/1402)
* Fix Nested matches format into something invalid. [#1400](https://github.com/fsprojects/fantomas/issues/1400)
* Fix Shortening big `if` clause still creates compiler warnings. [#1390](https://github.com/fsprojects/fantomas/issues/1390)
* Fix “FS0058: Possible incorrect indentation” on function composition after running Fantomas. [#1341](https://github.com/fsprojects/fantomas/issues/1341)
* Fix Typed App followed by chained lambda should not add space. [#1448](https://github.com/fsprojects/fantomas/issues/1448)
* Fix TypedApp should not have a space when chained. [#1447](https://github.com/fsprojects/fantomas/issues/1447)
* Fix Unexpected newline after short match expression. [#1445](https://github.com/fsprojects/fantomas/issues/1445)
* Fix Space after chain lambda function is not allowed. [#1440](https://github.com/fsprojects/fantomas/issues/1440)
* Fix Formatting error with MultilineBlockBracketsOnSameColumn. [#1396](https://github.com/fsprojects/fantomas/issues/1396)
* Fix fsharp_space_before_uppercase_invocation=true breaks method calls. [#1437](https://github.com/fsprojects/fantomas/issues/1437)
* Fix Crash regression on 4.4.0-beta-003. [#1438](https://github.com/fsprojects/fantomas/issues/1438)
* Fix MultiLineLambdaClosingNewline concats lambda arguments. [#1427](https://github.com/fsprojects/fantomas/issues/1427)
* Fix `member val` causes invalid code to be generated. [#1426](https://github.com/fsprojects/fantomas/issues/1426)
* Fix Surround return type annotations with white space [F# style guide]. [#1420](https://github.com/fsprojects/fantomas/issues/1420)
* Fix Lists concatene onto one line invalidly. [#1405](https://github.com/fsprojects/fantomas/issues/1405)
* Fix Accessibility modifier on record causes unindentation of following type. [#1404](https://github.com/fsprojects/fantomas/issues/1404)
* Fix Invalid addition of a space after constructor invocation. [#1401](https://github.com/fsprojects/fantomas/issues/1401)
* Fix "Inline" is incorrectly stripped out in FSI file. [#1399](https://github.com/fsprojects/fantomas/issues/1399)
* Fix Multiple type checks in a `try/with` get collapsed. [#1395](https://github.com/fsprojects/fantomas/issues/1395)
* Fix Short line length and member constraint leads to invalid code. [#1394](https://github.com/fsprojects/fantomas/issues/1394)
* Fix Object expression newline gets added/removed. [#1388](https://github.com/fsprojects/fantomas/issues/1388)
* Fix Arrays of constructors with lots of arguments gets dedented too much. [#1382](https://github.com/fsprojects/fantomas/issues/1382)
* Fix Format in pre-commit hook. [#1207](https://github.com/fsprojects/fantomas/issues/1207)
* Fix Shortening an 'if' condition causes compilation warnings about indentation. [#1374](https://github.com/fsprojects/fantomas/issues/1374)
* Fix Some escapes are unexpectedly modified in character literal patterns. [#1372](https://github.com/fsprojects/fantomas/issues/1372)
* Fix Fantomas formats with an error for very long DU case match. [#1364](https://github.com/fsprojects/fantomas/issues/1364)
* Fix Fantomas errors out on `new Foo ""`. [#1363](https://github.com/fsprojects/fantomas/issues/1363)
* Fix Aesthetics of long members in a type declaration. [#1362](https://github.com/fsprojects/fantomas/issues/1362)
* Fix Comment on first constructor argument gets removed. [#1350](https://github.com/fsprojects/fantomas/issues/1350)
* Fix “FS0058: Possible incorrect indentation” around if/then/else after running Fantomas. [#1349](https://github.com/fsprojects/fantomas/issues/1349)
* Fix Failing to format file should return an exit code different than 0. [#1340](https://github.com/fsprojects/fantomas/issues/1340)
* Fix Shorter MaxLineLength with long variable name yields invalid F# code according to fantomas. [#1241](https://github.com/fsprojects/fantomas/issues/1241)
* Fix MultilineBlockBracketsOnSameColumn should be honored inside match block. [#1238](https://github.com/fsprojects/fantomas/issues/1238)
* Fix Update constructor formatting to match MS Style guide. [#1359](https://github.com/fsprojects/fantomas/issues/1359)
* Fix Violation of name-sensitive alignments. [#1358](https://github.com/fsprojects/fantomas/issues/1358)
* Fix Pattern matching breaks code when expression is long and somewhat complex. [#1352](https://github.com/fsprojects/fantomas/issues/1352)
* Fix Inserts extra newline everytime formatter runs. [#1346](https://github.com/fsprojects/fantomas/issues/1346)
* Fix Functions looses space before parameter if func is defined inside method. [#1345](https://github.com/fsprojects/fantomas/issues/1345)
* Fix Required backslash removed in string interpolation. [#1344](https://github.com/fsprojects/fantomas/issues/1344)
* Fix Swallows comment before #nowarn directive. [#1220](https://github.com/fsprojects/fantomas/issues/1220)
* Fix Swallows comment inside `with` block (of a try-with). [#1219](https://github.com/fsprojects/fantomas/issues/1219)
* Fix Program.fs inside full path can lead to invalid AST. [#1337](https://github.com/fsprojects/fantomas/issues/1337)
* Fix Formatting of long parameter lists. [#657](https://github.com/fsprojects/fantomas/issues/657)
* Fix DotGet inside If expression not correct on second format. [#1329](https://github.com/fsprojects/fantomas/issues/1329)
* Fix Pipe is indented too far. [#1327](https://github.com/fsprojects/fantomas/issues/1327)
* Fix IfThenElse piped leads to invalid code. [#1324](https://github.com/fsprojects/fantomas/issues/1324)
* Fix Multiline when condition in pattern match needs to be further indented. [#1320](https://github.com/fsprojects/fantomas/issues/1320)
* Fix Add comma in front of expression in tuple with if/then/else. [#1319](https://github.com/fsprojects/fantomas/issues/1319)
* Fix New line before for loop not preserved. [#1317](https://github.com/fsprojects/fantomas/issues/1317)
* Fix Newline before set expression is lost. [#1314](https://github.com/fsprojects/fantomas/issues/1314)
* Fix Newline after let bang is missing. [#1313](https://github.com/fsprojects/fantomas/issues/1313)
* Fix Revisit place parameters on a new line for long definitions. [#1307](https://github.com/fsprojects/fantomas/issues/1307)
* Fix static member should only have a single indent. [#1300](https://github.com/fsprojects/fantomas/issues/1300)
* Fix Elmish-like syntax using yields with interspersed let statements breaks the code. [#1191](https://github.com/fsprojects/fantomas/issues/1191)
* Fix The 'member' keyword gets deleted in 'abstract member' declarations. [#1106](https://github.com/fsprojects/fantomas/issues/1106)
* Fix Don't indent too far. [#659](https://github.com/fsprojects/fantomas/issues/659)
* Fix static member should only have a single indent. [#1300](https://github.com/fsprojects/fantomas/issues/1300)
* Fix Named string argument to type provider requires a space prior to '@', which Fantomas removes. [#1209](https://github.com/fsprojects/fantomas/issues/1209)
* Fix Don't indent too far. [#659](https://github.com/fsprojects/fantomas/issues/659)
* Fix spaces removed from string. [#1290](https://github.com/fsprojects/fantomas/issues/1290)

#### 4.3.0 - 12/2020

* Feature Clarify constructors. [#1217](https://github.com/fsprojects/fantomas/issues/1217)
* Feature MultiLineLambdaClosingNewline. [#1221](https://github.com/fsprojects/fantomas/issues/1221)
* Feature Disable Elmish syntax. [#1198](https://github.com/fsprojects/fantomas/issues/1198)
* Support user-provided end-of-line characters. [#1231](https://github.com/fsprojects/fantomas/issues/1231)
* Add option to make expressions multiline based on number of subexpressions rather than character length. [#1143](https://github.com/fsprojects/fantomas/issues/1143)
* Update to FCS 38. [#1240](https://github.com/fsprojects/fantomas/pull/1240)
* Fix Comment after let binding breaks downstream output. [#1284](https://github.com/fsprojects/fantomas/issues/1284)
* Fix Comments are removed before and after empty array literals. [#1281](https://github.com/fsprojects/fantomas/issues/1281)
* Fix Use a safe filename when formatting from CodeFormatter. [#1279](https://github.com/fsprojects/fantomas/issues/1279)
* Fix Incorrect movement of a comma. [#966](https://github.com/fsprojects/fantomas/issues/966)
* Fix Bracket indentation is incorrect. [#1271](https://github.com/fsprojects/fantomas/issues/1271)
* Fix tuple with match formats to invalid code. [#1269](https://github.com/fsprojects/fantomas/issues/1269)
* Fix Incorrect movement of a comma. [#966](https://github.com/fsprojects/fantomas/issues/966)
* Fix Not adding a space even when all SpaceBefore* settings are enabled. [#964](https://github.com/fsprojects/fantomas/issues/964)
* Fix Multiline if condition can have incorrect indentation error. [#1267](https://github.com/fsprojects/fantomas/issues/1267)
* Fix multiline do bang gives a warning. [#1265](https://github.com/fsprojects/fantomas/issues/1265)
* Fix regressions. [#1264](https://github.com/fsprojects/fantomas/pull/1264)
* Fix multiline yield bang in list should be further indented. [#1254](https://github.com/fsprojects/fantomas/issues/1254)
* Fix Or pipe in destructured record should not be splitted. [#1252](https://github.com/fsprojects/fantomas/issues/1252)
* Fix Swap order of private and inline. [#1250](https://github.com/fsprojects/fantomas/issues/1250)
* Fix Comment is lost in enum. [#1247](https://github.com/fsprojects/fantomas/issues/1247)
* Fix Nested if/else/then in short mode. [#1243](https://github.com/fsprojects/fantomas/issues/1243)
* Fix Something doesn't add up in fix for 303. [#1093](https://github.com/fsprojects/fantomas/issues/1093)
* Fix Fantomas format is "unstable/oscillates" after upcast operator: adds/removes empty line. [#1227](https://github.com/fsprojects/fantomas/issues/1227)
* Fix Misplaces `=` in function signature so it's invalid F# code. [#1218](https://github.com/fsprojects/fantomas/issues/1218)
* Fix Additional newline is added between if/elif and for loop. [#1211](https://github.com/fsprojects/fantomas/issues/1211)
* Fix Let binding in hash directive disappears. [#1205](https://github.com/fsprojects/fantomas/issues/1205)
* Fix Downcast operator doesn't get a new line. [#1203](https://github.com/fsprojects/fantomas/issues/1203)
* Fix Implicit unit else results in extra lines with each reformat. [#1187](https://github.com/fsprojects/fantomas/issues/1187)
* Fix Incorrect formatting of function parameter application with lambdas. [#1201](https://github.com/fsprojects/fantomas/issues/1201)
* Fix Lambda functions in fluent API calls should indent further. [#970](https://github.com/fsprojects/fantomas/issues/970)
* Fix Hard to read code when using Thoth.Json. [#685](https://github.com/fsprojects/fantomas/issues/685)
* Fix --check should ignore the line endings. [#1196](https://github.com/fsprojects/fantomas/issues/1196)
* Fix Format entire return type on the line. [#1181](https://github.com/fsprojects/fantomas/issues/1181)
* Fix Only add one `in` keyword in LetOrUse. [#1176](https://github.com/fsprojects/fantomas/issues/1176)
* Fix Multiline SynPat.Record in pattern match is formatted as a mixture of single/multiline styles. [#1173](https://github.com/fsprojects/fantomas/issues/1173)
* Fix Inconsistent indentation of multiline records with internal keyword when fsharp_multiline_block_brackets_on_same_column is on/off. [#1171](https://github.com/fsprojects/fantomas/issues/1171)
* Fix Lambda argument splits in awkward way. [#1164](https://github.com/fsprojects/fantomas/issues/1164)
* Fix Multiline expression should be on next line. [#1158](https://github.com/fsprojects/fantomas/issues/1158)
* Fix Missing in keyword makes code invalid. [#1114](https://github.com/fsprojects/fantomas/issues/1114)
* Fix Invalid code after format. [#1032](https://github.com/fsprojects/fantomas/issues/1032)
* Fix Space before ^ SRTP type is removed in function call. [#984](https://github.com/fsprojects/fantomas/issues/984)
* Fix Shouldn't remove space after colon. [#908](https://github.com/fsprojects/fantomas/issues/908)
* Fix Crash when formatting with config file. [#824](https://github.com/fsprojects/fantomas/issues/824)
* Fix Formatting typeof generic static constraint fails to compile. [#803](https://github.com/fsprojects/fantomas/issues/803)
* Fix Indenting of record definition when internal. [#658](https://github.com/fsprojects/fantomas/issues/658)

#### 4.2.0 - 09/2020

* Feature MaxDotGetExpressionWidth. [#501](https://github.com/fsprojects/fantomas/issues/501)
* Fix Confusing symmetry between infix operators. [#988](https://github.com/fsprojects/fantomas/issues/988)
* Fix Comment before closing parenthesis is lost. [#1146](https://github.com/fsprojects/fantomas/issues/1146)

#### 4.1.1 - 09/2020

* Fix No newline between module and first declaration. [#1139](https://github.com/fsprojects/fantomas/issues/1139)
* Fix additional new lines added after each call to format. [#1137](https://github.com/fsprojects/fantomas/issues/1137)
* Fix Generics error when breaking line. [#1134](https://github.com/fsprojects/fantomas/issues/1134)
* Fix Comments on DUs parameterized by functions are dropped. [#1128](https://github.com/fsprojects/fantomas/issues/1128)
* Fix Preserve underscore in number. [#1120](https://github.com/fsprojects/fantomas/issues/1120)

#### 4.1.0 - 09/2020

* Ignore files by `.fantomasignore` file. [#420](https://github.com/fsprojects/fantomas/issues/420)
* Limit trivia by AST MainNode name. [#992](https://github.com/fsprojects/fantomas/pull/992)
* Lead by example. [#666](https://github.com/fsprojects/fantomas/issues/666)
* Verify all unit test whether the formatted code is valid. [#842](https://github.com/fsprojects/fantomas/issues/842)
* Fix Comments get dropped from the end of multi-line records. [#1124](https://github.com/fsprojects/fantomas/issues/1124)
* Fix Functions in nested modules which follow a type consisting of only one member of a DU are un-nested from that module. [#1123](https://github.com/fsprojects/fantomas/issues/1123)
* Fix After discriminated union in module wrongly indented. [#1122](https://github.com/fsprojects/fantomas/issues/1122)
* Fix Extra whitespace with a type which has an attribute. [#1116](https://github.com/fsprojects/fantomas/issues/1116)
* Fix Adds newline between comments and (all but the first) attribute. [#1108](https://github.com/fsprojects/fantomas/issues/1108)
* Fix Extra whitespace lines added in fsi files at the end of a nested module declaration. [#1105](https://github.com/fsprojects/fantomas/issues/1105)
* Fix Extra new line is added before attributes. [#1097](https://github.com/fsprojects/fantomas/issues/1097)
* Fix Comments after closing brace are lost. [#1096](https://github.com/fsprojects/fantomas/issues/1096)
* Fix Comment after Or operator lost. [#1095](https://github.com/fsprojects/fantomas/issues/1095)
* Fix Trivia before bar is being repeated. [#1083](https://github.com/fsprojects/fantomas/issues/1083)
* Fix Comment after arrow is being duplicated. [#1082](https://github.com/fsprojects/fantomas/issues/1082)
* Fix Allow line break after return to avoid excessive indenting/aligning. [#1062](https://github.com/fsprojects/fantomas/issues/1062)
* Fix Name of static members are removed/empty. [#1059](https://github.com/fsprojects/fantomas/issues/1059)
* Fix When using parenthesis in type definition, it will sometimes keep adding additional () for each time fantomas i run. [#1057](https://github.com/fsprojects/fantomas/issues/1057)
* Fix Multiline string in use expression. [#1055](https://github.com/fsprojects/fantomas/issues/1055)
* Fix Issue #246 has returned in v4.0.0. [#1051](https://github.com/fsprojects/fantomas/issues/1051)
* Fix Indentation after multiple hash directives is off. [#1026](https://github.com/fsprojects/fantomas/issues/1026)
* Fix if/then/else indented too far. [#1054](https://github.com/fsprojects/fantomas/issues/1054)
* Fix Single AST node should contain trivia. [#1031](https://github.com/fsprojects/fantomas/issues/1031)
* Fix Formatter adds extra newlines between type and any subsequent val in .fsi files. [#1029](https://github.com/fsprojects/fantomas/issues/1029)
* Fix Comments in if/then/else statements are sometimes deleted. [#1019](https://github.com/fsprojects/fantomas/issues/1019)
* Fix Moves type name around when writing constrained type defintions. [#1018](https://github.com/fsprojects/fantomas/issues/1018)
* Fix Line break before bracket on long method call followed by member access causes semantic change. [#994](https://github.com/fsprojects/fantomas/issues/994)
* Fix Long union case should be split over multiple lines. [#972](https://github.com/fsprojects/fantomas/issues/972)
* Fix FSI formatting does the wrong thing with comments on single-case DU. [#965](https://github.com/fsprojects/fantomas/issues/965)
* Fix Invalid unit test ``should break lines on multiline if conditions``. [#863](https://github.com/fsprojects/fantomas/issues/863)
* Fix Abstract member declarations don't follow page width. [#435](https://github.com/fsprojects/fantomas/issues/435)

#### 4.0.0 - 08/2020

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
* Feature SingleArgumentWebMode. [#927](https://github.com/fsprojects/fantomas/issues/927)
* Feature AlignFunctionSignatureToIndentation. [#946](https://github.com/fsprojects/fantomas/issues/946)
* Feature AlternativeLongMemberDefinitions. [#913](https://github.com/fsprojects/fantomas/issues/913)
* Feature MultilineBlockBracketsOnSameColumn. [#453](https://github.com/fsprojects/fantomas/issues/453)
* Feature NewlineBetweenTypeDefinitionAndMembers. [#752](https://github.com/fsprojects/fantomas/issues/752)
* Feature KeepIfThenInSameLine. [#825](https://github.com/fsprojects/fantomas/issues/825)
* Fix Comments in match statements are sometimes deleted. [#1010](https://github.com/fsprojects/fantomas/issues/1010)
* Fix Comments on members with get/set are deleted. [#1009](https://github.com/fsprojects/fantomas/issues/1009)
* Fix Hexadecimal numbers in enums are output as decimal numbers. [#1006](https://github.com/fsprojects/fantomas/issues/1006)
* Fix List indentation issue. [#999](https://github.com/fsprojects/fantomas/issues/999)
* Fix Hexadecimal numbers in match arms are output as decimal numbers. [#995](https://github.com/fsprojects/fantomas/issues/995)
* Fix Removes the first comment above the member of the list. [#990](https://github.com/fsprojects/fantomas/issues/990)
* Fix Brackets increased every time Fantomas is run. [#989](https://github.com/fsprojects/fantomas/issues/989)
* Fix Bug: Hash directive in computation expression. [#977](https://github.com/fsprojects/fantomas/issues/977)
* Fix Bug with hash directives inside a match statement. [#976](https://github.com/fsprojects/fantomas/issues/976)
* Fix Bug with defines in record member assignment. [#968](https://github.com/fsprojects/fantomas/issues/968)
* Fix Backquotes are stripped down from ``checked``. [#937](https://github.com/fsprojects/fantomas/issues/937)
* Fix Fantomas wraps extra set of parenthesis around parenthesis. [#921](https://github.com/fsprojects/fantomas/issues/921)
* Fix Remove indent setting from CLI tool. [#888](https://github.com/fsprojects/fantomas/issues/888)
* Fix Fantomas generating invalid F# when trying to format a list created with yields. [#882](https://github.com/fsprojects/fantomas/issues/882)
* Fix Invalid unit test ``method call on multiple lines``. [#862](https://github.com/fsprojects/fantomas/issues/862)
* Fix Invalid unit test ``should break on . operator and keep indentation``. [#860](https://github.com/fsprojects/fantomas/issues/860)
* Fix Multi-line arguments to chained method calls produce invalid code. [#702](https://github.com/fsprojects/fantomas/issues/702)
* Fix Line comment displaced from commented #-directive. [#638](https://github.com/fsprojects/fantomas/issues/638)
* Fix #if'd argument types are lost. [#633](https://github.com/fsprojects/fantomas/issues/633)
* Fix #if'd attributes moved to wrong column. [#631](https://github.com/fsprojects/fantomas/issues/631)
* Fix Assembly attributes create over-long lines. [#629](https://github.com/fsprojects/fantomas/issues/629)
* Fix List expression can get combined to a single line with different semantics. [#931](https://github.com/fsprojects/fantomas/issues/931)
* Fix Additional new line inserted around attributes. [#949](https://github.com/fsprojects/fantomas/issues/949)
* Fix `with get` removal in FSI invalid. [#945](https://github.com/fsprojects/fantomas/issues/945)
* Fix FSI file has "abstract" stripped. [#944](https://github.com/fsprojects/fantomas/issues/944)
* Fix Insertion of space before function application can break dot-chaining. [#943](https://github.com/fsprojects/fantomas/issues/943)
* Fix Concatenation of lines can break operator precedence. [#942](https://github.com/fsprojects/fantomas/issues/942)
* Fix Extra spaces inserted in record definition. [#941](https://github.com/fsprojects/fantomas/issues/941)
* Fix Comments at the end of async blocks are deleted automatically. [#936](https://github.com/fsprojects/fantomas/issues/936)
* Fix Newline between comments should lead to individual comments. [#920](https://github.com/fsprojects/fantomas/issues/920)
* Fix VS Code | Extra white space added to record definition. [#910](https://github.com/fsprojects/fantomas/issues/910)
* Fix When cutting off function invocations, should place each param in its own line (or align them to the 1st param). [#907](https://github.com/fsprojects/fantomas/issues/907)
* Fix Try online link points to old location. [#890](https://github.com/fsprojects/fantomas/issues/890)
* Fix Leading `|` in single-case union type with access modifier. [#889](https://github.com/fsprojects/fantomas/issues/889)
* Fix Type constraint on a type definition causes a loss of the type definition. [#887](https://github.com/fsprojects/fantomas/issues/887)
* Fix Fantomas removes the 'and' if there are multiple member constraints on a function declaration. [#886](https://github.com/fsprojects/fantomas/issues/886)
* Fix Comments inside a type definition can cause issues. [#885](https://github.com/fsprojects/fantomas/issues/885)
* Fix Long function signature should align with equal sign. [#883](https://github.com/fsprojects/fantomas/issues/883)
* Fix Newline not preserved between let and let bang. [#879](https://github.com/fsprojects/fantomas/issues/879)
* Fix Stackoverflow problem with let bang in match. [#876](https://github.com/fsprojects/fantomas/issues/876)
* Fix Incorrect formatting for chained class members using Websharper. [#871](https://github.com/fsprojects/fantomas/issues/871)
* Fix Pipe before and inside lambda leads to wrong indent of following lambda. [#870](https://github.com/fsprojects/fantomas/issues/870)
* Fix Formatting Program.fs with `--check` fails. [#869](https://github.com/fsprojects/fantomas/issues/869)
* Fix Possible wrong indentation for functions with parameters over multiple lines. [#868](https://github.com/fsprojects/fantomas/issues/868)
* Fix Invalid unit test ``different attributes according to defines``. [#864](https://github.com/fsprojects/fantomas/issues/864)
* Fix Invalid unit test ``record instance with inherit keyword``. [#861](https://github.com/fsprojects/fantomas/issues/861)
* Fix Invalid unit test ``should add space before type provider params``. [#859](https://github.com/fsprojects/fantomas/issues/859)
* Fix Incorrect end of line added after "(" which makes the code not to compile. [#856](https://github.com/fsprojects/fantomas/issues/856)
* Fix Incorrect end of line added after "(". [#855](https://github.com/fsprojects/fantomas/issues/855)
* Fix SpaceBeforeUppercaseInvocation applied in the middle of a invocation chain. [#853](https://github.com/fsprojects/fantomas/issues/853)
* Fix MultilineBlockBracketsOnSameColumn not working properly when calling base constructors. [#852](https://github.com/fsprojects/fantomas/issues/852)
* Fix PageWidth not respected for member with one long parameter. [#850](https://github.com/fsprojects/fantomas/issues/850)
* Fix Wrong indentation in member definition. [#844](https://github.com/fsprojects/fantomas/issues/844)
* Fix Class type with long variable names results in invalid formatted F# code. [#841](https://github.com/fsprojects/fantomas/issues/841)
* Fix Multiline let bang should have newline before. [#838](https://github.com/fsprojects/fantomas/issues/838)
* Fix complex computation expression identifier looks off. [#835](https://github.com/fsprojects/fantomas/issues/835)
* Fix keyword before type declaration leads to invalid F# code. [#830](https://github.com/fsprojects/fantomas/issues/830)
* Fix Inconsistent if-then-else cut. [#825](https://github.com/fsprojects/fantomas/issues/825)
* Fix MultilineBlockBracketsOnSameColumn=true not working on records with short names. [#823](https://github.com/fsprojects/fantomas/issues/823)
* Fix --config fantomas-config.json gives error. [#821](https://github.com/fsprojects/fantomas/issues/821)
* Fix multiline let bang should have a newline. [#819](https://github.com/fsprojects/fantomas/issues/819)
* Fix Updated value not indented correctly. [#817](https://github.com/fsprojects/fantomas/issues/817)
* Fix Comment removed in multi-case pattern matching. [#813](https://github.com/fsprojects/fantomas/issues/813)
* Fix Wrong handling multi lines comment at the end of file after function application. [#810](https://github.com/fsprojects/fantomas/issues/810)
* Fix Opening brace for test missing. [#806](https://github.com/fsprojects/fantomas/issues/806)
* Fix Return attribute deleted on reformatting. [#800](https://github.com/fsprojects/fantomas/issues/800)
* Fix Fantomas crash with evaluation of array member. [#798](https://github.com/fsprojects/fantomas/issues/798)
* Fix Type restrictions in FSI files. [#797](https://github.com/fsprojects/fantomas/issues/797)
* Fix AssemblyInfo.fs attributes get squashed together. [#796](https://github.com/fsprojects/fantomas/issues/796)
* Fix Byte-order mark is stripped. [#795](https://github.com/fsprojects/fantomas/issues/795)
* Fix Fantomas replaces "abstract" in fsi, leading to compile errors. [#794](https://github.com/fsprojects/fantomas/issues/794)
* Fix Broken links in Readme.md. [#791](https://github.com/fsprojects/fantomas/issues/791)
* Fix Multiline first member should not introduce initial newline. [#789](https://github.com/fsprojects/fantomas/issues/789)
* Fix Newline added before let binding with attribute in class. [#786](https://github.com/fsprojects/fantomas/issues/786)
* Fix Some floating-point numbers are changed. [#785](https://github.com/fsprojects/fantomas/issues/785)
* Fix Adding newline before first comment in module. [#784](https://github.com/fsprojects/fantomas/issues/784)
* Fix Parameter after multiline string parameter. [#783](https://github.com/fsprojects/fantomas/issues/783)
* Fix Modulo operator misplaced. [#780](https://github.com/fsprojects/fantomas/issues/780)
* Fix double-backtick identifier is formatted wrong when starts with non-alphanum character. [#776](https://github.com/fsprojects/fantomas/issues/776)
* Fix Line comment after record not printed. [#774](https://github.com/fsprojects/fantomas/issues/774)
* Fix Additional blank lines inserted after formatting. [#772](https://github.com/fsprojects/fantomas/issues/772)
* Fix Error while formatting Fantomas unit test with compiler define. [#761](https://github.com/fsprojects/fantomas/issues/761)
* Fix AbstractSlot with line comment is consider multi line. [#757](https://github.com/fsprojects/fantomas/issues/757)
* Fix Missing space after multiline string. [#754](https://github.com/fsprojects/fantomas/issues/754)
* Fix Cannot determine upper or lowercase. [#753](https://github.com/fsprojects/fantomas/issues/753)
* Fix Feature: Add blank line between type definition and members. [#752](https://github.com/fsprojects/fantomas/issues/752)
* Fix Default member implementation changed to member during formatting. [#742](https://github.com/fsprojects/fantomas/issues/742)
* Fix Long function definition should put equals and body on a newline. [#740](https://github.com/fsprojects/fantomas/issues/740)
* Fix Add extra space between prefix operator and string. [#736](https://github.com/fsprojects/fantomas/issues/736)
* Fix MaxIfThenElseShortWidth is not respected. [#734](https://github.com/fsprojects/fantomas/issues/734)
* Fix Shouldn't remove getters. [#733](https://github.com/fsprojects/fantomas/issues/733)
* Fix Comment after `then` keyword gets removed. [#730](https://github.com/fsprojects/fantomas/issues/730)
* Fix Determine if DotGet expression is upper- or lowercase. [#729](https://github.com/fsprojects/fantomas/issues/729)
* Fix Check for Trivia content before the equals sign in let bindings. [#728](https://github.com/fsprojects/fantomas/issues/728)
* Fix When advising user to file a bug, should mention the file it was trying to format. [#726](https://github.com/fsprojects/fantomas/issues/726)
* Fix space removed from parameters passed to inherited class. [#720](https://github.com/fsprojects/fantomas/issues/720)
* Fix Place parameters on a new line for very long member definitions. [#719](https://github.com/fsprojects/fantomas/issues/719)
* Fix Exception: Unexpected scenario when formatting else if / elif. [#713](https://github.com/fsprojects/fantomas/issues/713)
* Fix Fantomas keeps adding newlines every time you format. [#709](https://github.com/fsprojects/fantomas/issues/709)
* Fix Duplicate spaces and lost of linecomment. [#687](https://github.com/fsprojects/fantomas/issues/687)
* Fix Formatting of array literals of BigInteger. [#682](https://github.com/fsprojects/fantomas/issues/682)
* Fix Hash directive not between namespace and module. [#681](https://github.com/fsprojects/fantomas/issues/681)
* Fix Comment above static member is wrongly placed. [#680](https://github.com/fsprojects/fantomas/issues/680)
* Fix Do not remove property setters. [#664](https://github.com/fsprojects/fantomas/issues/664)
* Fix StringConstant printed twice. [#646](https://github.com/fsprojects/fantomas/issues/646)
* Fix Newline after "bang" keywords in computation expressions. [#615](https://github.com/fsprojects/fantomas/issues/615)
* Fix Incorrect indentation when folding a record update expression. [#536](https://github.com/fsprojects/fantomas/issues/536)
* Fix Preserve comments after record. [#516](https://github.com/fsprojects/fantomas/issues/516)
* Fix Long function signature broken into two lines. [#492](https://github.com/fsprojects/fantomas/issues/492)
* Fix "Better" support for nesting complex expressions in async { } blocks. [#386](https://github.com/fsprojects/fantomas/issues/386)

#### 3.3.0 - 02/2020

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

#### 3.2.0 - 02/2020

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

#### 3.1.0 - 11/2019
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

#### 3.0.0 - 10/2019
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
