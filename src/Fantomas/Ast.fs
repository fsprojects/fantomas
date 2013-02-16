module Fantomas.Ast

// This module is a modified version of http://fsharprefactor.codeplex.com/SourceControl/changeset/view/96754#1277905
// Most types are simplified according to our needs.

open System

type Literal = 
   | Char of char
   | String of string
   | Int16 of int16
   | Int of int
   | Int64 of int64
   | UInt16 of uint16
   | UInt of uint32
   | UInt64 of uint64
   | Byte of byte
   | SByte of sbyte
   | BigInt of bigint
   | IntPtr of IntPtr
   // Synonym of float32
   | Single of single
   // Synonym of float
   | Double of double
   | Bool of bool
   | Unit

type IsLetRec = bool

type Pat<'a> =
    | PVar of 'a
    | PApp of Pat<'a> * Pat<'a>
    | PLit of Literal    
    | PTuple of Pat<'a> list
    | PRecord of (string * Pat<'a>) list
    | PWild
    | PList of Pat<'a> list
    | PThis
    | PLongVar of Pat<'a> list
    | PIsInst of Type<'a>
    | PNull
    | POr of Pat<'a> * Pat<'a>
    | PAttribute of Pat<'a> * Attribute<'a> list
    | PAnds of Pat<'a> list
    | PNamed of Pat<'a> * Pat<'a>
    | PParen of Pat<'a>

and Attribute<'a> = Attribute of Exp<'a>

and Measure<'a> = 
    | Seq of Measure<'a> list
    | Named of Type<'a> 
    | Power of Measure<'a> * int
    | Product of Measure<'a> * Measure<'a>
    | Divide of Measure<'a> * Measure<'a>
    | One
    | Anon
    | MVar of string
    
and Exp<'a> = 
    | Var of 'a     
    | LongVarSet of Exp<'a> * Exp<'a>
    // Lambda abstraction
    | Lam of Pat<'a> list * Exp<'a>
    | App of Exp<'a> * Exp<'a>    
    // Local definition
    | Let of IsLetRec * (Pat<'a> * Exp<'a>) list * Exp<'a>    
    | LetBang of Pat<'a> * Exp<'a> * Exp<'a>
    | Lit of Literal 
    | Measure of Exp<'a> * Measure<'a>
    | Tuple of Exp<'a> list
    | List of Exp<'a> list
    | Match of Exp<'a> * Clause<'a> list
    | ForEach of Pat<'a> * Exp<'a> * Exp<'a>
    | For of Pat<'a> * Exp<'a> * Exp<'a> * Exp<'a>
    | TypeApp of Exp<'a> * Type<'a> list
    | Assert of Exp<'a>
    | While of Exp<'a> * Exp<'a>
    | AddressOf of Exp<'a>
    | Paren of Exp<'a>
    | YieldOrReturn of Exp<'a>
    | YieldOrReturnFrom of Exp<'a>
    | IfThenElse of Exp<'a> * Exp<'a> * Exp<'a> option
    | DotGet of Exp<'a> * Exp<'a>
    | DotSet of Exp<'a> * Exp<'a> * Exp<'a>
    | DotIndexedSet of Exp<'a> * Exp<'a> list * Exp<'a>
    | DotIndexedGet of Exp<'a> * Exp<'a> list
    | Record of ('a * Exp<'a>) list
    | New of Type<'a> * Exp<'a>
    | ObjExpr of ClassMember<'a> list
    | Do of Exp<'a>
    | DoBang of Exp<'a>
    | Downcast of Exp<'a> * Type<'a>
    | Upcast of Exp<'a> * Type<'a>
    | TryWith of Exp<'a> * Clause<'a> list
    | TryFinally of Exp<'a> * Exp<'a>
    | Typed of Exp<'a> * Type<'a>
    | Lazy of Exp<'a>
    | InferredDowncast of Exp<'a>
    | InferredUpcast of Exp<'a>
    | Quote of Exp<'a> * Exp<'a>
    | TypeTest of Exp<'a> * Type<'a>
    | TraitCall of string list * MemberSig<'a> * Exp<'a>
    | Null
    | ArbitraryAfterError
    | NotSupported

and Clause<'a> = Clause of Pat<'a> * Exp<'a>

and ExceptionDef<'a> = ExceptionDef of string * ClassMember<'a> list    

and TypeDef<'a> = 
    | DisUnion of string * 'a list
    | Enum of string * ('a * Literal) list
    | Record of string * 'a option list * ClassMember<'a> list
    | None of string
    | Class of string * ClassMember<'a> list
    | Abbrev of string * Type<'a>

and Type<'a> = 
    | TIdent of 'a
    | TLongIdent of Type<'a> list
    | TFun of Type<'a> * Type<'a>
    | TVar of Type<'a>
    | TApp of Type<'a> * Type<'a> list
    | TTuple of Type<'a> list
    | TArray of int * Type<'a>
    | TAnon
    | TMeasurePower of Type<'a> * int
    | TMeasureOne

and ClassMember<'a> = 
    | ImplicitCtor of Pat<'a> list
    | Member of bool * Pat<'a> * Exp<'a>
    | LetBindings of Exp<'a> list
    | AbstractSlot of string
    | Interface of Type<'a> * ClassMember<'a> list option
    | ValField of Type<'a> option * Type<'a>
    | Inherit of Type<'a> * Type<'a> option
    | ImplicitInherit of Type<'a> * Exp<'a> * Type<'a> option
    | NotSupported

and MemberSig<'a> = MemberSig of Type<'a>

type Module<'a> = 
    | Exp of Exp<'a> list
    | Types of TypeDef<'a> list
    | NestedModule of string list * Module<'a> list
    | Open of string list
    | Exception of ExceptionDef<'a>
    | HashDirective of string * string list
    | Attributes of Attribute<'a> list
    | ModuleAbbrev of string * string list
    | NotSupported