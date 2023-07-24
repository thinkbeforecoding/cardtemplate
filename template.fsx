#I @"C:\dev\PdfPig\src\UglyToad.PdfPig\bin\Debug\net6.0\"
#I @"C:\dev\FSharp.Formatting\src\FSharp.Formatting.Markdown\bin\Debug\netstandard2.1\"
#r @"UglyToad.PdfPig.Core"
#r @"UglyToad.PdfPig"
#r @"UglyToad.PdfPig.Tokens"
#r "FSharp.Formatting.Common"
#r "FSharp.Formatting.Markdown"
open System.IO
open UglyToad.PdfPig
open UglyToad.PdfPig.Writer
open UglyToad.PdfPig.Tokens
open UglyToad.PdfPig.Content
open UglyToad.PdfPig.Graphics.Operations
open UglyToad.PdfPig.Graphics.Operations.TextShowing
open UglyToad.PdfPig.Graphics.Operations.TextPositioning
open UglyToad.PdfPig.Core
open UglyToad.PdfPig.Graphics.Operations.TextObjects
open UglyToad.PdfPig.Graphics.Operations.TextState
open UglyToad.PdfPig.PdfFonts
open UglyToad.PdfPig.Writer.Fonts
open UglyToad.PdfPig.Graphics.Operations.SpecialGraphicsState
open FSharp.Formatting.Markdown


let printOps (ops: IGraphicsStateOperation seq) =
    for op in ops do
        match op with
        | :? ShowTextsWithPositioning as op ->
            printfn "[ShowTextWithPositioning]"
            for item in op.Array do
                printfn "    [%s]%O" (item.GetType().Name) item
            
        | op -> printfn "[%s] %O" (op.GetType().Name) op


let (|ShowTextsWithPositioning|_|) (op: IGraphicsStateOperation) =
    match op with
    | :? ShowTextsWithPositioning as op ->
        let builder = System.Text.StringBuilder()
        for token in op.Array do
            match token with
            | :? StringToken as t -> builder.Append(t.Data) |> ignore
            | _ -> ()
    
        Some (op, builder.ToString())
    | _ -> None

let (|ShowText|_|) (op: IGraphicsStateOperation) =
    match op with
    | :? ShowText as op ->
        Some (op, op.Text)
    | _ -> None

let rect (token: IToken) =
    let num (t: IToken) =
        (t:?>NumericToken).Double
    match token with
    | :? ArrayToken as t ->
        PdfRectangle(num t[0], num t.Data[1], num t.Data[2], num t.Data[3])
    | _ -> failwith "Not an ArrayToken"


let removeText (ops: IGraphicsStateOperation list) =
    let rec loop (ops: IGraphicsStateOperation list) result =
        match ops with
        | (:? BeginText) :: tail -> skip tail result
        | op :: tail -> loop tail (op :: result)
        | [] -> List.rev result 
    and skip (ops: IGraphicsStateOperation list) result =
        match ops with
        | (:? EndText) :: tail -> loop tail result
        | _ :: tail -> skip tail result
        | [] -> List.rev result

    loop ops []

let updateText (f: IGraphicsStateOperation list -> IGraphicsStateOperation list) (ops: IGraphicsStateOperation list) =
    let rec loop (ops: IGraphicsStateOperation list) result =
        match ops with
        | (:? BeginText) as op :: tail -> take tail result [op]
        | op :: tail -> loop tail (op :: result)
        | [] -> List.rev result 
    and take (ops: IGraphicsStateOperation list) result section =
        match ops with
        | (:? EndText) as op :: tail ->
            let newOps = f (List.rev (op :: section))
            loop tail ((List.rev newOps) @ result)
        | op :: tail -> take tail result (op :: section) 
        | [] -> List.rev result

    loop ops []


let (|InnerText|_|) (ops: IGraphicsStateOperation list) =
    ops |> 
    List.tryPick (function
        | ShowTextsWithPositioning(_,txt) -> Some txt
        | ShowText(_,txt) -> Some txt
        | _ -> None)

type Font = 
    { Name: NameToken
      Font: IWritingFont }
type Color =
    { R: decimal
      G: decimal
      B: decimal }

module Color =
    let rgb r g b = {R=r; G=g; B=b}
    let black = rgb 0m 0m 0m
    let blue = rgb 0.251m 0.51m 0.612m
    let red = rgb 1m 0.19m 0.271m
    let yellow = rgb 0.961m 0.698m 0.086m
    let green = rgb 0.278m 0.612m 0.416m

type TextSpan = 
    { Text: string
      Font: Font
      Color: Color option }

let getText (textToFind: string) (ops: IGraphicsStateOperation list) : IGraphicsStateOperation list =
    let rec findStart (ops: IGraphicsStateOperation list)  =
        match ops with
        | (:? BeginText) as op :: tail -> take tail [op]
        | op :: tail -> findStart tail 
        | [] -> []
    and take (ops: IGraphicsStateOperation list) (result: IGraphicsStateOperation list) =
        match ops with
        | ShowTextsWithPositioning (op ,txt) :: tail ->
            if txt.Contains(textToFind) then
                take tail (op :: result)
            else
                findStart tail
        | (:? EndText) as op :: _ -> List.rev (op:: result)
        | op :: tail -> take tail (op :: result)
        | [] -> List.rev result

    findStart ops 


let changeText (page: PdfPageBuilder) (newTexts: TextSpan list list) (ops: IGraphicsStateOperation list) =
    let renderSpans newLine spans =
        match spans with
        | head :: rest ->
            [
                SetFontAndSize(head.Font.Name, 1m) :> IGraphicsStateOperation
                match head.Color with
                | Some color -> SetNonStrokeColorDeviceRgb(color.R, color.G, color.B)
                | None -> ()
                
                if newLine then
                    MoveToNextLineShowText(page.UseLetters(head.Text,head.Font.Font))
                else
                    ShowText(page.UseLetters(head.Text,head.Font.Font))
                for span in rest do
                    SetFontAndSize(span.Font.Name, 1m) :> IGraphicsStateOperation
                    match span.Color with
                    | Some color -> SetNonStrokeColorDeviceRgb(color.R, color.G, color.B)
                    | None -> ()
                    ShowText(page.UseLetters(span.Text,span.Font.Font))
            ] 
        | [] -> []
    ops 
    |> List.collect (function
        | ShowTextsWithPositioning(_,_)
        | ShowText _ ->
            match newTexts with
            | [line] ->
                renderSpans false line
            | line :: rest ->
                [
                    SetTextLeading(1m) :> IGraphicsStateOperation
                    yield! renderSpans false line
                    for line in rest do 
                        yield! renderSpans true line
                ]
            | [] -> []
        | op -> [op]
         )

let getTextMatrix (ops: IGraphicsStateOperation list) =
    ops
    |> List.pick (function 
        | :? SetTextMatrix as op -> Some op.Value
        | _ -> None)

let changeTextCenter (page: PdfPageBuilder) (newTexts: (decimal * TextSpan list) list) (ops: IGraphicsStateOperation list) =
    let pageWidth = decimal page.PageSize.Width
    let m = getTextMatrix ops
    let offset = (m[3] * decimal (newTexts.Length-1)) / 2m
    let renderSpans newLine line spans =
        match spans with
        | w,head :: rest ->
            let x = (pageWidth - w) / 2m
            [   Push.Value :> IGraphicsStateOperation
                SetFontAndSize(head.Font.Name, 1m) 
                match head.Color with
                | Some color -> SetNonStrokeColorDeviceRgb(color.R, color.G, color.B)
                | None -> ()
                SetTextMatrix( [| 9m; 0m; 0m; 9m; x; m[5] + offset - decimal line * 9m |])

                // if newLine then
                //     MoveToNextLineShowText(page.UseLetters(head.Text,head.Font.Font))
                // else
                ShowText(page.UseLetters(head.Text,head.Font.Font))
                for span in rest do
                    SetFontAndSize(span.Font.Name, 1m) 
                    match span.Color with
                    | Some color -> SetNonStrokeColorDeviceRgb(color.R, color.G, color.B)
                    | None -> ()
                    ShowText(page.UseLetters(span.Text,span.Font.Font))

                Pop.Value
            ] 
        | _,[] -> []
    ops 
    |> List.collect (function
        | ShowTextsWithPositioning(_,_)
        | ShowText _ ->
            match newTexts with
            | [line] ->
                renderSpans false 0 line
            | line :: rest ->
                [
                    SetTextLeading(1m) :> IGraphicsStateOperation
                    yield! renderSpans false 0 line
                    for i,line in rest |> List.indexed do 
                        yield! renderSpans true (i+1) line
                ]
            | [] -> []
        | op -> [op]
         )


let changeFontName (font: NameToken) (ops: IGraphicsStateOperation list) =
    ops 
    |> List.map (function
        | :? SetFontAndSize as op ->
            SetFontAndSize(font, op.Size) :> IGraphicsStateOperation
        | op -> op
         )


let changeMatrix (f: decimal[] -> decimal[] )  (ops: IGraphicsStateOperation list) =
    ops 
    |> List.map (function
        | :? SetTextMatrix as op ->
           SetTextMatrix(f op.Value)  :> IGraphicsStateOperation
        | op -> op
         )

let getSizeX (ops: IGraphicsStateOperation list) =
    ops 
    |> List.pick (function
        | :? SetTextMatrix as op ->
            Some op.Value[0]
        | _ -> None
    )

let getFont (ops: IGraphicsStateOperation list) =
    ops
    |> List.pick (function 
        | :? SetFontAndSize as op -> Some op.Font
        | _ -> None
        )

let getTextContent (ops: IGraphicsStateOperation list) =
    
    let builder = 
        (new System.Text.StringBuilder(), ops)
        ||> List.fold (fun acc op ->
            match op with
            | :? ShowText as txt ->
                acc.Append(txt.Text)
            | :? ShowTextsWithPositioning as txt ->
                for token in txt.Array do
                    match token with
                    | :? StringToken as t ->
                        acc.Append(t.Data) |> ignore
                    | _ -> ()
                acc
            | _ -> acc
        )  
    builder.ToString()

let translate x y (m: decimal[]) =
    [|m[0]; m[1]; m[2]; m[3]; m[4]+x; m[5]+y |]

let copyWithBoxes (src: Page) (builder: PdfDocumentBuilder) =
        let page = builder.AddPage(PageSize.A7)
        page.CopyFrom(src) |> ignore
        page.SetTrimBox(src.Dictionary.Data[NameToken.TrimBox] |> rect)
        page.SetArtBox(src.Dictionary.Data[NameToken.ArtBox] |> rect)
        page.SetBleedBox(src.Dictionary.Data[NameToken.BleedBox] |> rect)
        page.SetMediaBox(src.Dictionary.Data[NameToken.MediaBox] |> rect)
        page.SetCropBox(src.Dictionary.Data[NameToken.CropBox] |> rect)
        page

let textWidth (txt: string) (f: IFont) =
    let mutable w = 0m
    let bytes = new ByteArrayInputBytes( System.Text.Encoding.UTF8.GetBytes(txt) )

    while bytes.MoveNext() do
        let mutable len = 0
        let c = f.ReadCharacterCode(bytes, &len)
        // len
        let b = f.GetBoundingBox(c)
        w <- w + decimal b.Width
    w


let setInsec (text:string) =
    let insec = "\u00A0"

    text.Replace("« ", "«\u00A0" )
        .Replace(" »", "\u00A0»")
        .Replace(" !", "\u00A0!")
        .Replace(" ?", "\u00A0?")
        .Replace(" ;", "\u00A0;")
        .Replace(" :", "\u00A0:")




type AddedFonts =
    { Regular: PdfDocumentBuilder.AddedFont
      Italic: PdfDocumentBuilder.AddedFont
      Bold: PdfDocumentBuilder.AddedFont}

type Fonts =
    { Regular: Font
      Italic: Font
      Bold: Font}

type FontStyle =
    | Regular
    | Italic
    | Bold

type Style =
    { FontStyle: FontStyle
      Color: Color option}

module Fonts =
    let getStyle style fonts =
        match style with
        | Regular -> fonts.Regular
        | Italic -> fonts.Italic
        | Bold -> fonts.Bold


type PdfPageBuilder with
    member this.GetFont(font: PdfDocumentBuilder.AddedFont) =
        { Name = this.GetAddedFont(font)
          Font = this.GetWritingFont(font)}

type LineContext = 
    { Span: decimal
      Word: decimal
      LineStart: int
      WordStart: int
      StyleStart: int
      
      Text: string
      Styles: Style[]
      Index: int
      Style: Style
      Fonts: Fonts
      Line: TextSpan list
      Lines: (decimal * TextSpan list) list
      Scale: decimal
      Max: decimal
    }

module LineContext =
    let close ctx =
        let sp =
            { Font = Fonts.getStyle ctx.Style.FontStyle ctx.Fonts
              Color = ctx.Style.Color
              Text = ctx.Text.Substring(ctx.StyleStart, ctx.Index - ctx.StyleStart)}
        ((ctx.Span + ctx.Word, List.rev (sp :: ctx.Line)) :: ctx.Lines)
        |> List.rev

    let hasNext ctx =
        ctx.Index < ctx.Text.Length

    let applyStyle style ctx =
        if ctx.Style = style then
            ctx 
        else
            { ctx with
                Style = style
                StyleStart = ctx.Index
                Line = 
                    { Font = Fonts.getStyle ctx.Style.FontStyle ctx.Fonts
                      Color = ctx.Style.Color
                      Text = ctx.Text.Substring(ctx.StyleStart, ctx.Index - ctx.StyleStart)
                    } :: ctx.Line
            }

    let addSpace w ctx =
        { ctx with
            Span = ctx.Span + ctx.Word + w
            Word = 0m
            WordStart = ctx.Index+1
            Index = ctx.Index+1
        }

    let addNewLine ctx =
        { ctx with
            Span = 0m
            Word = 0m
            LineStart = ctx.Index+1
            WordStart = ctx.Index+1
            StyleStart = ctx.Index + 1
            Index = ctx.Index+1
            Line = []
            Lines =
                let sp =
                    { Font = Fonts.getStyle ctx.Style.FontStyle ctx.Fonts
                      Color = ctx.Style.Color
                      Text = ctx.Text.Substring(ctx.StyleStart, ctx.Index - ctx.StyleStart)}
                (ctx.Span + ctx.Word, List.rev (sp :: ctx.Line)) :: ctx.Lines
        }

    let addChar w ctx =
        { ctx with
            Word = ctx.Word + w
            Index = ctx.Index+1
        }

    let cutLine ctx =
        { ctx with
            Span = 0m
            LineStart = ctx.WordStart
            StyleStart = ctx.WordStart
            Line = []
            Lines =
                let start = min ctx.StyleStart ctx.WordStart

                let sp =
                    { Font = Fonts.getStyle ctx.Style.FontStyle ctx.Fonts
                      Color = ctx.Style.Color
                      Text = ctx.Text.Substring(start, ctx.WordStart - start)}
                (ctx.Span, List.rev (sp :: ctx.Line)) :: ctx.Lines

        }

    // let checkStart txt ctx =
    //     if ctx.WordStart < ctx.StyleStart then
    //         failwith $"W{ctx.WordStart} S{ctx.StyleStart} %s{txt}"
    //     else
    //         ctx
        

    let next ctx =
        let c = ctx.Text[ctx.Index]
        let style = ctx.Styles[ctx.Index]
        let font = Fonts.getStyle style.FontStyle ctx.Fonts
        let w = 
            let m = font.Font.GetFontMatrix()
            match font.Font.TryGetAdvanceWidth(c) with
            | true,w -> 
                let b = PdfRectangle(0,0,w,0)
                let t = m.Transform(b)
                ctx.Scale * decimal t.Width
            | false, _ -> 0m 

        let ctx2 = applyStyle style ctx 
        let ctx3 =
            if c = ' ' then
                addSpace w ctx2
            elif c = '\n' then
                addNewLine ctx2 
            else
                addChar w ctx2 
        if ctx3.Span + ctx3.Word > ctx3.Max then
            cutLine ctx3 
        else
            ctx3
        
let rec split' ctx =
    if LineContext.hasNext ctx then
        split' (LineContext.next ctx)
    else
        LineContext.close ctx

let split (f: Fonts) text styles sx max = 
    let ctx = 
        { Span = 0m
          Word = 0m
          LineStart = 0
          WordStart = 0
          StyleStart = 0
          Index = 0
          Text = setInsec text
          Styles = styles
          Style = { FontStyle = Regular; Color = None }
          Fonts = f
          Scale = sx
          Max = max
          Line = []

          Lines = []
        }
    split' ctx
let extractStyle (txt: (Style * string) list) =
    let text = 
        (System.Text.StringBuilder(), txt)
        ||> List.fold (fun b (_,t) -> b.Append(t))
        |> string
    let styles =
        txt 
        |> Seq.collect (fun (s,t) -> Seq.replicate t.Length s)
        |> Seq.toArray
    text, styles
    
let cartes = PdfDocument.Open(@"C:\Users\jchassaing\OneDrive\Transmissions\template\Transmission(s)_Cartes Situation (Impressions).pdf")

module Situation =
    let verso number (builder: PdfDocumentBuilder) =
        let page = copyWithBoxes (cartes.GetPage(1)) builder
        let ops = page.CurrentStream.Operations |> Seq.map (function
            | :? ShowText as st when st.Text = "1" ->
                ShowText(string number) :> IGraphicsStateOperation
            | op -> op ) |> Seq.toList

        page.CurrentStream.Operations.Clear()
        page.CurrentStream.Operations.AddRange(ops)

    let recto num (mundial: AddedFonts) (futura: AddedFonts) text (builder: PdfDocumentBuilder) =
        let text, styles = extractStyle text
        let src = cartes.GetPage(2)
        let page = copyWithBoxes src builder
        let ops = page.CurrentStream.Operations |> Seq.toList

        let newops =
            ops
            |> updateText (function 
                | ops & InnerText(txt) when txt.StartsWith("SITUATION") ->
                    let newTxt = $"SITUATION %d{num}"
                    let ft = getFont ops
                    let f = src.GetFont(ft)
                    let sizex = getSizeX ops
                    let orgWidth = sizex * textWidth txt f
                    let width = sizex * textWidth newTxt f
                    let x = (orgWidth - width) / 2m
                    let font = page.GetFont(mundial.Regular)


                    ops
                    // |> changeFontName (page.GetAddedFont(mundial)) 
                    |> changeText page [ [ { Text = newTxt; Color = None; Font = font} ] ]
                    |> changeMatrix (translate x 0m)



                | ops & InnerText(txt) when txt.StartsWith("En") ->
                    let ft = getFont ops
                    let f = src.GetFont(ft)

                    let sizex = getSizeX ops

                    let font = { Regular = page.GetFont(futura.Regular)
                                 Italic = page.GetFont(futura.Italic)
                                 Bold = page.GetFont(futura.Bold) }
                    
                    let lines =
                        split font text styles sizex 180m |> List.map snd

                    ops 
                        |> changeText page lines
                        |> changeMatrix (fun m -> 
                            let offset = (m[3] * decimal (lines.Length-2)) / 2m
                            translate 0m offset m)
                | _ -> []
            )

        page.CurrentStream.Operations.Clear()
        page.CurrentStream.Operations.AddRange(newops)

module Reaction =
    let recto num (mundial: AddedFonts) (futura: AddedFonts) text (builder: PdfDocumentBuilder) =
        let text, styles = extractStyle text
        let src = cartes.GetPage(3)
        let page = copyWithBoxes src builder
        let ops = page.CurrentStream.Operations |> Seq.toList

        let newops =
            ops
            |> updateText (function 
                | ops & InnerText(txt) when txt.StartsWith("RÉACTION") ->
                    let newTxt = $"RÉACTION S%d{num}"
                    let ft = getFont ops
                    let f = src.GetFont(ft)
                    let sizex = getSizeX ops
                    let orgWidth = sizex * textWidth txt f
                    let width = sizex * textWidth newTxt f
                    let x = (orgWidth - width) / 2m
                    let font = page.GetFont(mundial.Regular)


                    ops
                    |> changeText page [ [ { Text = newTxt; Color = None; Font = font} ] ]
                    |> changeMatrix (translate x 0m)



                | ops & InnerText(txt) when txt.StartsWith("Je") ->
                    let ft = getFont ops
                    let f = src.GetFont(ft)

                    let sizex = getSizeX ops

                    let font = { Regular = page.GetFont(futura.Regular)
                                 Italic = page.GetFont(futura.Italic)
                                 Bold = page.GetFont(futura.Bold) }
                    
                    let lines =
                        split font text styles sizex 180m 

                    ops 
                        // |> changeMatrix (fun m -> 
                        //     let offset = (m[3] * decimal (lines.Length-2)) / 2m
                        //     [| m[0];m[1];m[2];m[3]; 0m; m[5]+offset |])
                        |> changeTextCenter page lines 
                | _ -> []
            )

        page.CurrentStream.Operations.Clear()
        page.CurrentStream.Operations.AddRange(newops)

    let verso num (reaction,count) (mundial: AddedFonts) (futura: AddedFonts) text (builder: PdfDocumentBuilder) =
        let text, styles = extractStyle text
        let src = cartes.GetPage(4)
        let page = copyWithBoxes src builder
        let ops = page.CurrentStream.Operations |> Seq.toList

        let newops =
            ops
            |> updateText (function 
                | ops & InnerText(txt) when txt.StartsWith("RÉACTION") ->
                    let newTxt = $"RÉACTION S%d{num}"
                    let ft = getFont ops
                    let f = src.GetFont(ft)
                    let sizex = getSizeX ops
                    let orgWidth = sizex * textWidth txt f
                    let width = sizex * textWidth newTxt f
                    let x = (orgWidth - width) / 2m
                    let font = page.GetFont(mundial.Regular)


                    ops
                    |> changeText page [ [ { Text = newTxt; Color = None; Font = font} ] ]
                    |> changeMatrix (translate x 0m)



                | ops & InnerText(txt) when txt.StartsWith("2 à 6") ->
                    let sizex = getSizeX ops

                    let font = { Regular = page.GetFont(futura.Regular)
                                 Italic = page.GetFont(futura.Italic)
                                 Bold = page.GetFont(futura.Bold) }
                    
                    let lines =
                        split font text styles sizex 180m |> List.map snd 

                    ops 
                        |> changeMatrix (fun m -> 
                            let offset = (m[3] * decimal (lines.Length-2)) / 2m
                            translate 0m offset m )
                        |> changeText page lines 
                | ops & InnerText(txt) when txt = "1" ->
                    let font = page.GetFont(mundial.Regular)
                    ops |> changeText page [[ { Text = $"{reaction}"; Color = None; Font = font} ]]
                        |> changeMatrix (translate -2m 0m)
                | ops & InnerText(txt) when txt = "5" ->
                    let font = page.GetFont(mundial.Regular)
                    ops |> changeText page [[ { Text = $"{count}"; Color = None; Font = font} ]]
                | ops & InnerText(txt) when txt = "|" ->
                    ops 
                | _ -> []
            )

        page.CurrentStream.Operations.Clear()
        page.CurrentStream.Operations.AddRange(newops)


module Cards =
    type Situation =
        { Id: int 
          Dice: int
          Text: (Style * string) list 
          Reactions: Reaction list
          }
    and Reaction =
        { Text: (Style * string) list
          Consequences: Consequence list }    
    and Consequence =
        { Range: Range
          Text: (Style * string) list
          Score: Score }
    and Range = { Min: int; Max: int}
    and Score =
        | Score of int
        | Escalade of int


let rec toText' style (spans: MarkdownSpans) =
    match spans with
    | [] -> []
    | span :: tail ->
        [   match span with
            | MarkdownSpan.Literal(txt,_) -> style, txt
            | MarkdownSpan.Emphasis(body, _) ->
                yield! toText' { style with FontStyle = Italic } body
            | MarkdownSpan.Strong(body, _) ->
                yield! toText' { style with FontStyle = Bold } body
            | _ -> ()
            yield! toText' style tail
        ]

let toText (spans: MarkdownSpans) =
    toText' { FontStyle = Regular; Color = None } spans
    

open Cards
let md = FSharp.Formatting.Markdown.Markdown.Parse(File.ReadAllText(@"C:\Users\jchassaing\OneDrive\Transmissions\template\situations.md"))

let rangeRx = System.Text.RegularExpressions.Regex(@"^(\d+)(\sà (\d+))?\s*:\s*$")
let scoreRx = System.Text.RegularExpressions.Regex(@"\(([+\-]?\d)\)")
let escaladeRx = System.Text.RegularExpressions.Regex(@"\(\s*voir\s+Escalade\s*\)")


let parseConsequence ps =
    match ps with
    | Span(description, _) :: _ ->
        let range= 
            description |> List.tryPick ( function
                | Strong([Literal(txt,_)],_) ->
                    let m = rangeRx.Match(txt)
                    if m.Success then
                        let min = int m.Groups[1].Value
                        let max =
                            if m.Groups[3].Success then
                                int m.Groups[3].Value
                            else
                                min
                        { Min = min
                          Max = max} |> Some
                    else
                        None
                | _ -> None
            )
        let score= 
            description |> List.tryPick ( function
                | Strong([Literal(txt,_)],_) ->
                    let m = scoreRx.Match(txt)
                    if m.Success then
                        Some (Score (int m.Groups[1].Value))
                    else
                        let m = escaladeRx.Match(txt)
                        if m.Success then
                            Some (Escalade 0)
                        else
                            None
                | _ -> None
            )
        let text =
            description
            |> List.filter (function
                | Strong([Literal(txt,_)],_)  when rangeRx.IsMatch(txt) ||scoreRx.IsMatch(txt) || escaladeRx.IsMatch(txt) -> false
                | _ -> true
            ) 


        { Range = range |> Option.defaultValue { Min = 0; Max = 0}
          Text = toText text
          Score = score |> Option.defaultValue (Score 0) }


let parseReaction ps =
    match ps with
    | Span(description,_) :: ListBlock(_,items,_) ::_ ->
        { Text = toText description
          Consequences = items |> List.map parseConsequence
        }
    | [ Span(description,_) ] ->
        { Text = toText description
          Consequences = []}

let situation = 
    match md.Paragraphs with
    | Heading(2, _,_) :: Paragraph(txtSituation,_) :: ListBlock(_, items,_) :: _ ->
        


        { Id = 1
          Dice = 0
          Text = toText txtSituation
          Reactions = items |> List.map parseReaction }

let (ListBlock(_,lst,_))  = md.Paragraphs[2]
let (ListBlock(_,d,_)) = lst[0][1]
d[0][0]


let futuraBytes = File.ReadAllBytes(@"C:\Users\jchassaing\OneDrive\Transmissions\Fonts\Futura PT\FuturaPTBook.ttf")
let futuraIBytes = File.ReadAllBytes(@"C:\Users\jchassaing\OneDrive\Transmissions\Fonts\Futura PT\FuturaPTBookOblique.ttf")
let futuraBBytes = File.ReadAllBytes(@"C:\Users\jchassaing\OneDrive\Transmissions\Fonts\Futura PT\FuturaPTMedium.ttf")
let mundialBytes = File.ReadAllBytes(@"C:\Users\jchassaing\OneDrive\Transmissions\Fonts\Mundial\MundialBold.ttf")

let builder = new PdfDocumentBuilder()
let mundial = 
    let font = builder.AddTrueTypeFont(mundialBytes)
    { AddedFonts.Regular = font
      Italic = font
      Bold = font }

let futura =
    { AddedFonts.Regular = builder.AddTrueTypeFont(futuraBytes)
      Italic = builder.AddTrueTypeFont(futuraIBytes)
      Bold = builder.AddTrueTypeFont(futuraBBytes) }

// Situation.verso 8 builder
let (++) style color = { FontStyle = style; Color = Some color} 
let st style = { FontStyle = style; Color = None} 
// let txt =
//     [ st Regular, "Il est midi, tu sors acheter du pain à la boulangerie la plus proche. Sur le trajet, c'est un homme qui te dit : "
//       st Italic,"« Ouais t'es trop belle ! Bisous partout. »\n"
//       st Bold, "Tu l’ignores et poursuit ton chemin vers la boulangerie. En sortant, tu dois repasser devant lui et il te dit : "
//       st Italic, "« Tu ne me donnes même pas un bisous ? Viens. »"]
// Situation.recto 10 mundial futura  txt  builder


    
// Reaction.recto 10 mundial futura [ st Regular, "Je change de place dans l’espoir qu’il ne me suive pas."] builder
// let conseq =
//     [ Bold ++ Color.blue, "1 :"
//       Regular ++ Color.black, " La rame se vide à la station suivante et l’homme reste dans celle-ci mais n’est plus collé à vous.  Il semble que vous vous soyez trompé·e sur ses intentions. Vous vous sentez soulagé·e mais un peu honteux·se. "
//       Bold ++ Color.red, " (-1)\n\n"
//       Bold ++ Color.blue, "2 à 6 :"
//       Regular ++ Color.black, " L’homme insiste et commence à vous toucher, vous écourtez votre voyage et sortez à la station suivante pour lui échapper."
//       Bold ++ Color.yellow, " (0)\n\n"
//       Bold ++ Color.blue, "4 à 10 :"
//       Regular ++ Color.black, " L’homme insiste et commence à vous toucher, vous écourtez votre voyage et sortez à la station suivante pour lui échapper."
//       Bold ++ Color.green, " (+2)" 
//       ]
// Reaction.verso 10 (3,5) mundial futura conseq builder

Situation.verso situation.Dice builder
Situation.recto situation.Id mundial futura situation.Text builder
let count = situation.Reactions.Length
for i, reaction in situation.Reactions |> List.indexed do
    let text = reaction.Text |> List.map (fun (s,t) -> {s with FontStyle = match s.FontStyle with Bold -> Regular | x -> x },t)
    Reaction.recto situation.Id mundial futura text builder

    let consequences =
        [ for consequence in reaction.Consequences do
            if consequence.Range.Min = consequence.Range.Max then
                Bold ++ Color.blue, $"%d{consequence.Range.Min} :"
            else
                Bold ++ Color.blue, $"%d{consequence.Range.Min} à {consequence.Range.Max} :"
            yield! consequence.Text |> List.map (fun (s,t) -> { Color = Some Color.black; FontStyle = match s.FontStyle with Bold -> Regular | x -> x },t)
            match consequence.Score with
            | Score score when score > 0 ->
                Bold ++ Color.green, $"(+%d{score})\n"
            | Score score when score < 0 ->
                Bold ++ Color.red, $"(-%d{-score})\n"
            | Score _ ->
                Bold ++ Color.yellow, $"(0)\n"
            | Escalade n -> Bold ++ Color.blue , $"(voir Escalade %d{n})\n"
        ]
    Reaction.verso situation.Id (i+1, count) mundial futura consequences builder

// let page = builder.AddPage(PageSize.A7)
// let text, styles =
//     [ st Regular, "Je prends les courses et les mets dans la poubelle la plus proche en lui disant : "
//       st Italic, "« Excusez-moi, je les ai salies avec mes sales mains de noir·e ! »" ]
//     |> extractStyle 

// let fonts =
//     { Regular = page.GetFont(futura.Regular)
//       Bold = page.GetFont(futura.Bold)
//       Italic = page.GetFont(futura.Italic) }

// split fonts text styles 9m 180m



File.WriteAllBytes(@"C:\Users\jchassaing\OneDrive\Transmissions\template\Result.pdf", builder.Build())


let r = PdfDocument.Open(@"C:\Users\jchassaing\OneDrive\Transmissions\template\Result.pdf")
// let r = PdfDocument.Open(@"C:\Users\jchassaing\OneDrive\Transmissions\template\Transmission(s)_Cartes Situation (Impressions).pdf")
r.GetPage(10).Operations |> printOps
// (snd (r.GetPage(2).Dictionary.TryGet<DictionaryToken>(NameToken.Resources))).TryGet<DictionaryToken>(NameToken.Font)
// r.GetPage(2).Operations |> printOps
