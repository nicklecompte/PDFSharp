module AbstractDocument

open Common

[<Struct>]
type FontSize =
    | Int of (size:uint)
    | Dec of (sizeInt:uint) * (sizeDecimal:byte)

type ProgrammingLanguage =
    | Unspecified
    | FSharp
    | Scheme

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

type DocumentComponent =
    | Header of Heading
    | TextBlock of Paragraph
    | Lines of DocumentComponent list

type AbstractDocument<'T>
