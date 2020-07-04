module Common

open Prelude

[<Struct>]
type FontSize =
    | Int of (size:uint)
    | Dec of (sizeInt:uint) * (sizeDecimal:byte)

type BulletStyle =
    | SmallDot
    | MediumDot
    | BigDot
    | Cross
    | Plus
    | Dash
    | O
    | Circle

type EnumerationStyle =
    | Number of WholeNumber
    | Letter of ASCIICharacter
    | Decimal of WholeNumber list

type Formatting =
    | Font of FontSize
    | Bold
    | Italic
    | Strikethrough
    | BulletListItem of BulletStyle
    | EnumeratedListItem of EnumerationStyle
    | URL of link:string
    | Codeblock of ProgrammingLanguage
    | Pair of Formatting*Formatting
    | Footnote of number:Nat * text : Text
    
and Text =
    | Raw of string
    | Formatted of Formatting*string
    | Sequence of Text list

type Heading = | Heading of level:WholeNumber * content:Text

type Paragraph = | Paragraph of Text

type Nat =
    | Zero
    | Succ of Nat
with
    static member Zero : Nat = Zero
    static member One : Nat = Succ Zero
    static member (+) (a:Nat,b:Nat) =
        match (a,b) with
        | (Zero, _) -> b
        | (_,Zero) -> a
        | (Succ n, _) -> Succ (n + b)
    static member (*) (a:Nat,b:Nat) =
        match (a,b) with
        | (Zero,_) -> Zero
        | (_,Zero) -> Zero
        | (Succ n, Succ m) -> a + (n * b)
    static member (>) (a,b) =
        match (a,b) with
            | (Zero,Zero) -> false
            | (Zero, ) -> false
            | (_, Zero) -> true
            | (Succ n, Succ m) -> n > m

type WholeNumber =
    | One
    | SuccWhole of WholeNumber
    member x.ToNat() =
        match x with
            | One -> Succ Zero
            | SuccWhole n -> Succ (n.ToNat())
    static member One : WholeNumber = One
    static member (+) (a:WholeNumber,b:WholeNumber) =
        match (a,b) with
        | (One,_) -> SuccWhole b
        | (_, One) -> SuccWhole a
        | (SuccWhole n, _) -> SuccWhole (n + b)


/// Hacky """"""dependently""""""-""""""typed"""""" vector.
type Vect<'T> =
    | ZeroVector of Nat * ('T list)
    | VectCons of Nat * 'T * (VectConstructor 'T)
with
    // TODO IMMEDIATELY refactor to InformativeResult
    member x.CheckValidity =
        match x with
        | ZeroVector n xs ->
            match (n,xs) with
            | (Zero,[]) -> true
            | _ -> false
        | VectCons n v ->
            match (n,v) with
                | (Zero, ZeroVector (_,_)) -> false
                | (Succ Zero, ZeroVector (_,_)) -> true

       

// [<Struct>]
// type UTF7Char =
//     | 7Bit of char:byte
//     // This should be a pointer but really we need to just not support UTF7
//     | Unsupported of escape : byte

[<Struct>]
type EncodedCharacter =
    | ASCII of char:byte
    | UTF8  
    | UTF18

/// Used for code blocks, etc
type ProgrammingLanguage =
    | Unspecified
    | FSharp
    | Scheme
    | Bash
    | PowerShell
    | CSharp
    | C
    | CPP
    | Haskell
    | Java
    | Scala
    | R
    | Python
    | Rust

[<Struct>]
type DocumentLocation =
    | RowCol of rownum:uint32 * colnum : uint16
    | RowColPage of row:uint16*col:uint16*page:uint32

[<Struct>]
type DocumentOperation<'TEncoding> =
    | View
    | InsertCharacter of character : 'TEncoding * location : DocumentLocation
    | DeleteCharacter of loc : DocumentLocaton
