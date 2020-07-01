module Markdown

open Common
open AbstractDocument
open System
open System.Text

////////////////////////////////////////////
////      TYPES                          ///
////////////////////////////////////////////

type MarkdownInfo = string // Markdown should be pretty generic about these things

type MarkdownWarning =
    | UnsupportedHeaderDepth
    | UnsupportedLanguageFormatting

type MarkdownError =
    | FigureOutLater
       
// TODO refactor to CPS
let markdownFormattedTextToString format text =
    let builder = new StringBuilder()
    let rec helper f b t =
        match f with
            | Font _ -> t
            | Bold ->
                b.Append("**").Append(t).Append("**").ToString()
            | Italic ->
                b.Append('*').Append(t).Append('*').ToString()
            | URL link ->
                b.Append('[').Append(t).Append(']')
                .Append('(').Append(link).Append(')').ToString()
            | Pair f1 f2 -> helper f1 b (helper f2 b t)
    helper format builder text
      
let markdownTextToString (t: Text) : string =
    match t with
        | Raw s -> s
        | Formatted(f,s) ->
            match f with
                

let markdownHeadingToString (Heading(n,t) : Heading) :InformativeResult<string,_,string,_,_,exn,string> =
    let builder = new StringBuilder()
    let inTxt = markdownTextToString t
    let warn =
        match n with
        | One ->
            builder.Append("# ") |> ignore
            false
        | SuccWhole One ->
            builder.Append("##" ) |> ignore
            false
        | SuccWhole (SuccWhole One) ->
            builder.Append("### ")  |> ignore
            false
        | _ ->
            builder.Append("### ")  |> ignore
            true
    builder.Append(inTxt) |> ignore
    let headerStr = builder.ToString()
    match warn with
        | true -> WarningOK ("Markdown only supports level three headers",headerStr)
        | false -> OK headerStr 
           

let rec renderAsMarkdownLines : DocumentComponent -> string list = fun document ->
    match document with
        | Header h ->
            match h with
                | Heading (n,t) -> failwith "not done"

let parseMarkdownLine (inputLine : string) : DocumentComponent

let parseMarkdown (input : string) : AbstractDocument =
    
                  
