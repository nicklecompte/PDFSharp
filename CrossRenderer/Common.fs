module Common

open Prelude

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

type WholeNumber =
    | One
    | SuccWhole of WholeNumber
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

type private Vect<'T>(n:Nat) =

    
    

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

type ProgrammingLanguage =
    | Unspecified
    | FSharp
    | Scheme

[<Struct>]
type DocumentLocation =
    | RowCol of rownum:uint32 * colnum : uint16
    | RowColPage of row:uint16*col:uint16*page:uint32
