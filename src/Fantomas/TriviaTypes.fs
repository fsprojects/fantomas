module Fantomas.TriviaTypes

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text

type FsTokenType =
    | AMP
    | AMP_AMP
    | AND_BANG
    | BAR
    | BAR_BAR
    | BAR_RBRACK
    | COLON_COLON
    | COLON_EQUALS
    | COLON_GREATER
    | COLON_QMARK
    | COLON_QMARK_GREATER
    | DELAYED
    | DO
    | DOLLAR
    | DOT_DOT
    | DOT_DOT_HAT
    | ELIF
    | ELSE
    | EQUALS
    | FINALLY
    | GREATER
    | IF
    | IN
    | INFIX_AMP_OP
    | INFIX_BAR_OP
    | INFIX_COMPARE_OP
    | INFIX_STAR_DIV_MOD_OP
    | INFIX_STAR_STAR_OP
    | INT32_DOT_DOT
    | LBRACE
    | LBRACK
    | LBRACK_BAR
    | LESS
    | LPAREN
    | LPAREN_STAR_RPAREN
    | MEMBER
    | MINUS
    | PERCENT_OP
    | PLUS_MINUS_OP
    | PREFIX_OP
    | QMARK
    | QMARK_QMARK
    | RARROW
    | RBRACE
    | RBRACK
    | RPAREN
    | THEN
    | TRY
    | WITH

type Token =
    { TokenInfo: FSharpTokenInfo
      LineNumber: int
      Content: string }

type Comment =
    | LineCommentAfterSourceCode of comment: string
    | LineCommentOnSingleLine of comment: string * range
    | BlockComment of string * newlineBefore: bool * newlineAfter: bool

(* LineComment Examples

let a = 7 // b

=> LineCommentAfterSourceCode("// b", true)

// meh
let a = 7

=> LineCommentOnSingleLine("// meh", false)
*)

type TriviaContent =
    | Keyword of Token
    | KeywordString of string
    | Number of string
    | StringContent of string
    | IdentOperatorAsWord of string
    | IdentBetweenTicks of string
    | Comment of Comment
    | Newline
    | Directive of directive: string
    | CharContent of string
    | EmbeddedIL of string

type Trivia =
    { Item: TriviaContent
      Range: Range }
    static member Create item range : Trivia = { Item = item; Range = range }

type TriviaIndex = TriviaIndex of int * int

type FsAstType =
    | Ident_
    | LongIdent_ // namespace or module identifier
    // Modules and namespaces cannot really be trusted
    // Their range can be influenced by non code constructs (like comments)
    //    | SynModuleOrNamespace_AnonModule
    //    | SynModuleOrNamespace_DeclaredNamespace
    //    | SynModuleOrNamespace_GlobalNamespace
    //    | SynModuleOrNamespace_NamedModule
    | SynModuleDecl_ModuleAbbrev
    | SynModuleDecl_NestedModule
    | SynModuleDecl_Let
    | SynModuleDecl_DoExpr
    | SynModuleDecl_Types
    | SynModuleDecl_Exception
    | SynModuleDecl_Open
    | SynModuleDecl_OpenType
    | SynModuleDecl_Attributes
    | SynModuleDecl_HashDirective
    | SynModuleDecl_NamespaceFragment
    | SynExpr_Paren
    | SynExpr_Quote
    // | SynExpr_Const use SynConst instead
    // | SynExpr_Typed use either the nested SynExpr or SynType
    | SynExpr_Tuple
    | SynExpr_StructTuple
    | SynExpr_Record
    | SynExpr_AnonRecd
    | SynExpr_New
    | SynExpr_ObjExpr
    | SynExpr_While
    | SynExpr_For
    | SynExpr_ForEach
    | SynExpr_ArrayOrListOfSeqExpr
    | SynExpr_ArrayOrList
    // | SynExpr_CompExpr use first nested SynExpr
    | SynExpr_Lambda
    | SynExpr_MatchLambda
    | SynExpr_MatchLambda_Function
    | SynExpr_Match
    | SynExpr_Do
    | SynExpr_Assert
    | SynExpr_App
    | SynExpr_TypeApp
    // | SynExpr_LetOrUse use first nested SynExpr
    | SynExpr_TryWith
    | SynExpr_TryFinally
    | SynExpr_Lazy
    // | SynExpr_Sequential use first nested SynExpr
    | SynExpr_SequentialOrImplicitYield
    | SynExpr_IfThenElse
    | SynExpr_Ident
    | SynExpr_LongIdent
    | SynExpr_LongIdentSet
    | SynExpr_DotGet
    | SynExpr_DotSet
    | SynExpr_Set
    | SynExpr_DotIndexedGet
    | SynExpr_DotIndexedSet
    | SynExpr_NamedIndexedPropertySet
    | SynExpr_DotNamedIndexedPropertySet
    | SynExpr_TypeTest
    | SynExpr_Upcast
    | SynExpr_Downcast
    | SynExpr_InferredUpcast
    | SynExpr_InferredDowncast
    | SynExpr_Null
    | SynExpr_AddressOf
    | SynExpr_TraitCall
    | SynExpr_JoinIn
    | SynExpr_ImplicitZero
    | SynExpr_YieldOrReturn
    | SynExpr_YieldOrReturnFrom
    | SynExpr_LetOrUseBang
    | SynExpr_MatchBang
    | SynExpr_DoBang
    | SynExpr_LibraryOnlyILAssembly
    | SynExpr_LibraryOnlyStaticOptimization
    | SynExpr_LibraryOnlyUnionCaseFieldGet
    | SynExpr_LibraryOnlyUnionCaseFieldSet
    | SynExpr_ArbitraryAfterError
    | SynExpr_FromParseError
    | SynExpr_DiscardAfterMissingQualificationAfterDot
    | SynExpr_Fixed
    | SynExpr_InterpolatedString
    | SynInterpolatedStringPart_String
    | SynInterpolatedStringPart_FillExpr
    | RecordField_
    | AnonRecordField_
    | AnonRecordTypeField_
    | SynMemberSig_Member
    | SynMemberSig_Interface
    | SynMemberSig_Inherit
    | SynMemberSig_ValField
    | SynMemberSig_NestedType
    | SynIndexerArg_One
    | SynIndexerArg_Two
    | SynMatchClause_Clause
    | ArgOptions_
    | InterfaceImpl_
    | TypeDefn_
    | TypeDefnSig_
    // | SynTypeDefnSigRepr_ObjectModel use first nested node
    | SynTypeDefnSigRepr_Exception
    | SynMemberDefn_Open
    | SynMemberDefn_OpenType
    | SynMemberDefn_Member
    | SynMemberDefn_ImplicitCtor
    | SynMemberDefn_ImplicitInherit
    | SynMemberDefn_LetBindings
    | SynMemberDefn_AbstractSlot
    | SynMemberDefn_Interface
    | SynMemberDefn_Inherit
    | SynMemberDefn_ValField
    | SynMemberDefn_NestedType
    | SynMemberDefn_AutoProperty
    | SynSimplePat_Id
    | SynSimplePat_Typed
    | SynSimplePat_Attrib
    | SynSimplePats_SimplePats
    | SynSimplePats_Typed
    | StandaloneExpression_
    | NormalBinding_
    | DoBinding_
    | SynBindingReturnInfo_
    | SynValTyparDecls_
    | TyparDecl_
    | Typar_
    | ValSpfn_
    // | SynPat_Const, use SynConst instead
    | SynPat_Wild
    | SynPat_Named
    | SynPat_Typed
    | SynPat_Attrib
    // | SynPat_Or, use the inner patterns instead
    | SynPat_Ands
    | SynPat_LongIdent
    | SynPat_Tuple
    | SynPat_Paren
    | SynPat_ArrayOrList
    | SynPat_Record
    | SynPat_Null
    | SynPat_OptionalVal
    | SynPat_IsInst
    | SynPat_QuoteExpr
    | SynPat_DeprecatedCharRange
    | SynPat_InstanceMember
    | SynPat_FromParseError
    | SynConst_Bool
    | SynConst_Unit
    | SynConst_SByte
    | SynConst_Byte
    | SynConst_Int16
    | SynConst_UInt16
    | SynConst_Int32
    | SynConst_UInt32
    | SynConst_Int64
    | SynConst_UInt64
    | SynConst_IntPtr
    | SynConst_UIntPtr
    | SynConst_Single
    | SynConst_Double
    | SynConst_Char
    | SynConst_Decimal
    | SynConst_UserNum
    | SynConst_String
    | SynConst_Bytes
    | SynConst_UInt16s
    | SynConst_Measure
    | Pats_
    | NamePatPairs_
    | ComponentInfo_
    // | SynTypeDefnRepr_ObjectModel use first nested node
    // | SynTypeDefnRepr_Simple use first nested node
    | SynTypeDefnRepr_Exception
    | SynTypeDefnKind_TyconUnspecified
    | SynTypeDefnKind_TyconClass
    | SynTypeDefnKind_TyconInterface
    | SynTypeDefnKind_TyconStruct
    | SynTypeDefnKind_TyconRecord
    | SynTypeDefnKind_TyconUnion
    | SynTypeDefnKind_TyconAbbrev
    | SynTypeDefnKind_TyconHiddenRepr
    | SynTypeDefnKind_TyconAugmentation
    | SynTypeDefnKind_TyconILAssemblyCode
    | SynTypeDefnKind_TyconDelegate
    | SynTypeDefnSimpleRepr_None
    | SynTypeDefnSimpleRepr_Union
    | SynTypeDefnSimpleRepr_Enum
    | SynTypeDefnSimpleRepr_Record
    | SynTypeDefnSimpleRepr_General
    | SynTypeDefnSimpleRepr_LibraryOnlyILAssembly
    | SynTypeDefnSimpleRepr_TypeAbbrev
    | SynTypeDefnSimpleRepr_Exception
    | SynExceptionDefn_
    | SynExceptionDefnRepr_
    | SynAttribute_
    | SynAttributeList_
    | UnionCase_
    | UnionCaseFields_
    | UnionCaseFullType_
    | EnumCase_
    | Field_
    | SynType_LongIdent
    | SynType_App
    | SynType_LongIdentApp
    | SynType_Tuple
    | SynType_Array
    | SynType_Fun
    | SynType_Var
    | SynType_Anon
    | SynType_WithGlobalConstraints
    | SynType_HashConstraint
    | SynType_MeasureDivide
    | SynType_MeasurePower
    | SynType_StaticConstant
    | SynType_StaticConstantExpr
    | SynType_StaticConstantNamed
    | SynType_AnonRecd
    | SynType_Paren
    | SynValData_
    | SynValInfo_
    | SynArgInfo_
    | ParsedHashDirective_
    // Modules and namespaces cannot really be trusted
    // Their range can be influenced by non code constructs (like comments)
//    | SynModuleOrNamespaceSig_AnonModule
//    | SynModuleOrNamespaceSig_DeclaredNamespace
//    | SynModuleOrNamespaceSig_GlobalNamespace
//    | SynModuleOrNamespaceSig_NamedModule
    | SynModuleSigDecl_ModuleAbbrev
    | SynModuleSigDecl_NestedModule
    | SynModuleSigDecl_Types
    | SynModuleSigDecl_Open
    | SynModuleSigDecl_OpenType
    | SynModuleSigDecl_HashDirective
    | SynModuleSigDecl_Exception
    | SynModuleSigDecl_NamespaceFragment
    | SynExceptionSig_
    | SynAccess_Private
    | SynAccess_Internal
    | SynAccess_Public
    | File_
    | SigFile_

type TriviaNodeType =
    | MainNode of ``type``: FsAstType
    | Token of ``type``: FsTokenType * Token

type TriviaNode =
    { Type: TriviaNodeType
      ContentBefore: TriviaContent list
      ContentItself: TriviaContent option
      ContentAfter: TriviaContent list
      Range: Range }

type TriviaNodeAssigner(nodeType: TriviaNodeType, range: Range, ?linesBetweenParent: int) =
    member this.Type = nodeType
    member this.Range = range
    member this.AttributeLinesBetweenParent = linesBetweenParent
    member val ContentBefore = ResizeArray<TriviaContent>() with get, set
    member val ContentItself = Option<TriviaContent>.None with get, set
    member val ContentAfter = ResizeArray<TriviaContent>() with get, set

type MkRange = int * int -> int * int -> Range
