#I @"C:\dev\PdfPig\src\UglyToad.PdfPig\bin\Debug\net6.0\"
#I @"C:\dev\FSharp.Formatting\src\FSharp.Formatting.Markdown\bin\Debug\netstandard2.1\"
#r @"UglyToad.PdfPig.Core"
#r @"UglyToad.PdfPig"
#r @"UglyToad.PdfPig.Tokens"
#r "FSharp.Formatting.Common"
#r "FSharp.Formatting.Markdown"
open System.IO
open System.Collections.Generic
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
open UglyToad.PdfPig.Graphics.Operations.PathConstruction


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
[<Struct>]
type Size = 
    { Width: decimal
      Height: decimal }

[<Struct>]
type Point = 
    { X: decimal
      Y: decimal }

type Font = 
    { Name: NameToken
      Font: IWritingFont }

[<Struct>]
type Rgb =
    { R: decimal
      G: decimal
      B: decimal }

[<Struct>]
type Cmyk =
    { C: decimal
      M: decimal
      Y: decimal
      K: decimal }
    
type Color =
    | Rgb of Rgb
    | Cmyk of Cmyk

type TextColor =
    | Accent
    | Color of Color
    | DefaultColor

module Color =
    let rgb r g b = Rgb {R=r; G=g; B=b}
    let black = rgb 0m 0m 0m
    let blue = rgb 0.251m 0.51m 0.612m
    let red = rgb 1m 0.19m 0.271m
    let yellow = rgb 0.961m 0.698m 0.086m
    let green = rgb 0.278m 0.612m 0.416m

type TextSpan = 
    { Text: string
      Font: Font
      Color: TextColor }

type Line =
    { Spans: TextSpan list
      Size: Size }

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

let getTextMatrix (ops: IGraphicsStateOperation list) =
    ops
    |> List.pick (function 
        | :? SetTextMatrix as op -> Some op.Value
        | _ -> None)

let translate x y (m: decimal[]) =
    [|m[0]; m[1]; m[2]; m[3]; m[4]+x; m[5]+y |]

let setPos pos (m: decimal[]) =
    [|m[0]; m[1]; m[2]; m[3]; pos.X; pos.Y |]

let getColor ops =
    let rec loop (ops: IGraphicsStateOperation list) =
        match ops with
        | (:? SetNonStrokeColorDeviceRgb as op) :: (:? AppendRectangle ) :: _ ->
            Rgb { R = op.R; G = op.G; B = op.B }
        | (:? SetNonStrokeColorDeviceCmyk as op) :: (:? AppendRectangle ) :: _ ->
            Cmyk { C = op.C; M = op.M; Y = op.Y; K = op.K }
        | _ :: tail -> loop tail
        | [] -> failwith "Color not found"
    loop ops

 
type VerticalAlign =
    | Top
    | Midle
    | Bottom

type HorizontalAlign =
    | Left
    | Center
    | Right

let setColor color =
    match color with
    | Rgb color -> SetNonStrokeColorDeviceRgb(color.R, color.G, color.B) :> IGraphicsStateOperation
    | Cmyk color -> SetNonStrokeColorDeviceCmyk(color.C, color.M, color.Y, color.K)

let changeText (page: PdfPageBuilder) (lines: Line list) divWidth halign valign accent (ops: IGraphicsStateOperation list) =
    let renderSpans matrix spans =
        match spans with
        | head :: rest ->
            [
                SetFontAndSize(head.Font.Name, 1m) :> IGraphicsStateOperation
                match head.Color with
                | DefaultColor -> ()
                | Color color -> setColor color
                | Accent -> setColor accent
                
                SetTextMatrix(matrix)
                ShowText(page.UseLetters(head.Text,head.Font.Font))
                for span in rest do
                    SetFontAndSize(span.Font.Name, 1m) :> IGraphicsStateOperation
                    match span.Color with
                    | DefaultColor -> ()
                    | Color color -> setColor color
                    | Accent -> setColor accent
                    ShowText(page.UseLetters(span.Text,span.Font.Font))
            ] 
        | [] -> []

    let ys = lines |> List.scan (fun y l -> y+l.Size.Height) 0m 
    let ylines = List.zip (List.truncate lines.Length ys) lines
    let m = getTextMatrix ops

    let totalHeight = List.last ys
    let top =
        match valign with
        | Top -> 0m
        | Midle -> totalHeight / 2m
        | Bottom -> totalHeight
    
    ops 
    |> List.collect (function
        | ShowTextsWithPositioning(_,_)
        | ShowText _ ->
            [
                let last = List.length ylines - 1 
                SetTextLeading(1m) :> IGraphicsStateOperation
                for i,(y,line) in ylines |> Seq.indexed do 
                    let x =
                        match halign with
                        | Left -> 0m
                        | Center -> line.Size.Width / 2m
                        | Right -> line.Size.Width

                    yield! renderSpans (translate -x (top-y) m) line.Spans
            ]
        | :? SetTextMatrix -> []
        | op -> [op]
         )
let changeMatrix (f: decimal[] -> decimal[] )  (ops: IGraphicsStateOperation list) =
    ops 
    |> List.map (function
        | :? SetTextMatrix as op ->
           SetTextMatrix(f op.Value)  :> IGraphicsStateOperation
        | op -> op
         )

let getSize (m: decimal[]) =
    {Width = m[0]; Height = m[3] }

let getPos (m: decimal[]) =
    { X = m[4]; Y = m[5] }

let getFont (ops: IGraphicsStateOperation list) =
    ops
    |> List.pick (function 
        | :? SetFontAndSize as op -> Some op.Font
        | _ -> None
        )


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
      Color: TextColor}

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

    member this.GetFonts(fonts: AddedFonts) =
        { Regular = this.GetFont(fonts.Regular)
          Italic = this.GetFont(fonts.Italic)
          Bold = this.GetFont(fonts.Bold) }


type LineContext = 
    { SpanWidth: decimal
      WordWidth: decimal
      LineStart: int
      WordStart: int
      StyleStart: int
      
      Text: string
      Styles: Style[]
      Index: int
      Style: Style
      Fonts: Fonts
      Line: TextSpan list
      Lines: Line list
      Scale: Size
      ParagraphSpacing: decimal
      Max: decimal
    }

module LineContext =
    let close ctx =
        let sp =
            { Font = Fonts.getStyle ctx.Style.FontStyle ctx.Fonts
              Color = ctx.Style.Color
              Text = ctx.Text.Substring(ctx.StyleStart, ctx.Index - ctx.StyleStart)}
        let line = 
            { Spans = List.rev (sp :: ctx.Line) 
              Size = { Width = ctx.SpanWidth + ctx.WordWidth 
                       Height = ctx.Scale.Height } }
        (line :: ctx.Lines)
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
            SpanWidth =
                if ctx.SpanWidth + ctx.WordWidth > 0m then
                    ctx.SpanWidth + ctx.WordWidth + w
                else
                    0m    
            WordWidth = 0m
            WordStart = ctx.Index+1
            Index = ctx.Index+1
        }

    let addNewLine ctx =
        { ctx with
            SpanWidth = 0m
            WordWidth = 0m
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
                let line = 
                    { Spans = List.rev (sp :: ctx.Line)
                      Size = 
                        { Width = ctx.SpanWidth + ctx.WordWidth
                          Height = ctx.Scale.Height * ctx.ParagraphSpacing } }
                line :: ctx.Lines
        }

    let addChar w ctx =
        { ctx with
            WordWidth = ctx.WordWidth + w
            Index = ctx.Index+1
        }

    let cutLine ctx =
        { ctx with
            SpanWidth = 0m
            LineStart = ctx.WordStart
            StyleStart = ctx.WordStart
            Line = []
            Lines =
                let start = min ctx.StyleStart ctx.WordStart

                let sp =
                    { Font = Fonts.getStyle ctx.Style.FontStyle ctx.Fonts
                      Color = ctx.Style.Color
                      Text = ctx.Text.Substring(start, ctx.WordStart - start)}
                let line =
                    { Spans = List.rev (sp :: ctx.Line)
                      Size =
                        { Width = ctx.SpanWidth
                          Height = ctx.Scale.Height} }
                line :: ctx.Lines

        }

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
                ctx.Scale.Width * decimal t.Width
            | false, _ -> 0m 

        let ctx2 = applyStyle style ctx 
        if c = '\n' then
            addNewLine ctx2
        else
            let ctx3 =
                if ctx2.SpanWidth + ctx2.WordWidth + w > ctx2.Max then
                    cutLine ctx2
                else
                    ctx2 
            if c = ' ' then
                addSpace w ctx3
            else
                addChar w ctx3

        
let rec split' ctx =
    if LineContext.hasNext ctx then
        split' (LineContext.next ctx)
    else
        LineContext.close ctx

let split'' (f: Fonts) text styles scale paragraphSpacing max = 
    let ctx = 
        { SpanWidth = 0m
          WordWidth = 0m
          LineStart = 0
          WordStart = 0
          StyleStart = 0
          Index = 0
          Text = setInsec text
          Styles = styles
          Style = { FontStyle = Regular; Color = DefaultColor }
          Fonts = f
          Scale = scale
          ParagraphSpacing = paragraphSpacing
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

let split (f: Fonts) text scale paragraphSpacing max =
    let text, styles = extractStyle text
    split'' f text styles scale paragraphSpacing max
    
let centerPage (src: Page) ops =
    let m = getTextMatrix ops
    let pos = getPos m
    let bleed = src.Dictionary.Data[NameToken.TrimBox] |> rect

    ops |> changeMatrix (setPos { pos with X = decimal bleed.Width / 2m + decimal bleed.Left })

let (++) style color = { FontStyle = style; Color = Color color} 
let st style = { FontStyle = style; Color = DefaultColor} 
let accent style = { FontStyle = style; Color = Accent} 



let cartes = PdfDocument.Open(@"C:\Users\jchassaing\OneDrive\Transmissions\template\Transmission(s)_Cartes Situation (Template).pdf")

type Colors =
    | Blue
    | Red
    | Yellow
    | Purple
    | Green

type CardType =
    | SituationRecto
    | ReactionRecto
    | ReactionVerso
    | EscaladeRecto
    | EscaladeVerso
let template color cardType  =
    let c = 
        match color with
        | Blue -> 0
        | Red -> 1
        | Yellow -> 2
        | Purple -> 3
        | Green -> 4
    let t = 
        match cardType with
        | SituationRecto -> 0
        | ReactionRecto -> 1
        | ReactionVerso -> 2
        | EscaladeRecto -> 3
        | EscaladeVerso -> 4
    2 + c * 5 + t

module Situation =
    let verso number mundial (builder: PdfDocumentBuilder) =
        let src = cartes.GetPage(1)
        let page = copyWithBoxes src builder
        let ops = page.CurrentStream.Operations |> Seq.toList
        let rects = ResizeArray()

        let newOps =
            ops
            |> updateText (function 
                | ops & InnerText txt when txt = "X" ->
                        let m = getTextMatrix ops
                        let size = getSize m
                        let pos = getPos m
                        let font = page.GetFonts(mundial)
                        let text = split font [ st Bold, $"%d{number}"] size 1m (decimal page.PageSize.Width)

                        rects.Add(pos, size)

                        (ops
                        |> changeText page text (decimal page.PageSize.Width) Center Top  Color.black)
                | ops -> ops)

        page.CurrentStream.Operations.Clear()
        page.CurrentStream.Operations.AddRange(newOps)

        if number = 6 ||number = 9 then
            for pos, size in rects do
                let dx,dy =
                    if pos.X < 180m then
                        -9.5m, -7m
                    else
                        8.5m, 3m

                page.SetTextAndFillColor(255uy,255uy,255uy)
                    .SetStrokeColor(255uy,255uy,255uy)
                    .DrawRectangle(PdfPoint(pos.X + dx, pos.Y + dy), size.Width * 0.6m, 3m, fill = true)
                |> ignore

    let recto num color (mundial: AddedFonts) (futura: AddedFonts) text (builder: PdfDocumentBuilder) =
        let src = cartes.GetPage(template color SituationRecto)
        let page = copyWithBoxes src builder
        let ops = page.CurrentStream.Operations |> Seq.toList
        let accent = getColor (cartes.GetPage(template color ReactionRecto).Operations |> Seq.toList)

        let newops =
            ops
            |> updateText (function 
                | ops & InnerText(txt) when txt.StartsWith("SITUATION") ->
                    let size =  getTextMatrix ops  |> getSize
                    let font = page.GetFonts(mundial)
                    let text = split font [ st Bold, $"SITUATION %d{num}"] size 1m (decimal page.PageSize.Width)

                    ops
                    |> centerPage src 
                    |> changeText page text (decimal page.PageSize.Width) Center Top accent


                | ops & InnerText(txt) when txt.StartsWith("Texte") ->
                    let m = getTextMatrix ops
                    let size = getSize m 
                    let font = page.GetFonts(futura)
                    let pos = getPos m
                    let trimbox = src.Dictionary.Data[NameToken.TrimBox] |> rect

                    let width = decimal trimbox.Width - (pos.X - decimal trimbox.Left) * 2m //+ decimal trimbox.Left
                    let lines = split font text size 1.5m width
                    ops |> changeText page lines width Left Midle accent
                | _ -> []
            )

        page.CurrentStream.Operations.Clear()
        page.CurrentStream.Operations.AddRange(newops)

module Reaction =


    let recto num color (mundial: AddedFonts) (futura: AddedFonts) text (builder: PdfDocumentBuilder) =
        let src = cartes.GetPage(template color ReactionRecto)
        let page = copyWithBoxes src builder
        let ops = page.CurrentStream.Operations |> Seq.toList
        let accent = getColor (cartes.GetPage(template color ReactionRecto).Operations |> Seq.toList)
        let newops =
            ops
            |> updateText (function 
                | ops & InnerText(txt) when txt.StartsWith("RÉACTION") ->
                    let size =  getTextMatrix ops  |> getSize
                    let font = page.GetFonts(mundial)
                    let text = split font [ st Bold, $"RÉACTION S%d{num}"] size 1m (decimal page.PageSize.Width)

                    ops
                    |> centerPage src 
                    |> changeText page text (decimal page.PageSize.Width) Center Top accent



                | ops & InnerText(txt) when txt.StartsWith("Texte") ->
                    let size = getTextMatrix ops |> getSize
                    let font = page.GetFonts(futura)
                    let lines = split font text size 1.5m 180m 
                    ops
                    |> centerPage src
                    |> changeText page lines 180m Center Midle accent
                | _ -> []
            )

        page.CurrentStream.Operations.Clear()
        page.CurrentStream.Operations.AddRange(newops)

    let verso num color (reaction,count) (mundial: AddedFonts) (futura: AddedFonts) text (builder: PdfDocumentBuilder) =
        let src = cartes.GetPage(template color ReactionVerso)
        let page = copyWithBoxes src builder
        let ops = page.CurrentStream.Operations |> Seq.toList
        let accent = getColor (cartes.GetPage(template color ReactionRecto).Operations |> Seq.toList)

        let newops =
            ops
            |> updateText (function 
                | ops & InnerText(txt) when txt.StartsWith("RÉACTION") ->
                    let size =  getTextMatrix ops  |> getSize
                    let font = page.GetFonts(mundial)
                    let text = split font [ st Bold, $"RÉACTION S%d{num}"] size 1m (decimal page.PageSize.Width)

                    ops
                    |> centerPage src 
                    |> changeText page text (decimal page.PageSize.Width) Center Top accent



                | ops & InnerText(txt) when txt.StartsWith("Texte") ->
                    let m = getTextMatrix ops
                    let size =  getSize m
                    let pos = getPos m
                    let trimbox = src.Dictionary.Data[NameToken.TrimBox] |> rect

                    let width = decimal trimbox.Width - (pos.X - decimal trimbox.Left) * 2m + decimal trimbox.Left
                    let font = page.GetFonts(futura)
                    let lines = split font text size 1.5m width 

                    (ops
                    |> changeMatrix (translate 0m -15m)
                    |> changeText page lines width Left Midle accent)
                    @ [ setColor accent ]

                | ops & InnerText(txt) when txt = "|" ->
                    let font = page.GetFonts(mundial)
                    let size = getTextMatrix ops |> getSize 
                    let lines =
                        split font [
                            st Bold, $"%d{reaction}" 
                            st Regular, " | "
                            st Bold, $"%d{count}" ]
                                size 1m (decimal page.PageSize.Width)
                    ops |> changeText page lines (decimal page.PageSize.Width) Center Top accent
                        |> changeMatrix (translate 2m 0m)
                | _ -> []
            )

        page.CurrentStream.Operations.Clear()
        page.CurrentStream.Operations.AddRange(newops)

module Escalade =

    let recto num k color (mundial: AddedFonts) (futura: AddedFonts) text (builder: PdfDocumentBuilder) =
        let src = cartes.GetPage(template color EscaladeRecto)
        let page = copyWithBoxes src builder
        let ops = page.CurrentStream.Operations |> Seq.toList
        let accent = getColor (cartes.GetPage(template color ReactionRecto).Operations |> Seq.toList)

        let newops =
            ops
            |> updateText (function 
                | ops & InnerText(txt) when txt.StartsWith("ESCALADE") ->
                    let size =  getTextMatrix ops  |> getSize
                    let font = page.GetFonts(mundial)
                    let text = split font [ st Bold, $"ESCALADE %d{num} %c{k}"] size 1m (decimal page.PageSize.Width)

                    ops
                    |> centerPage src 
                    |> changeText page text (decimal page.PageSize.Width) Center Top accent



                | ops & InnerText(txt) when txt.StartsWith("Texte") ->
                    let size = getTextMatrix ops |> getSize
                    let font = page.GetFonts(futura)
                    let lines = split font text size 1.5m 180m 
                    ops
                    |> centerPage src
                    |> changeText page lines 180m Center Midle accent
                | _ -> []
            )

        page.CurrentStream.Operations.Clear()
        page.CurrentStream.Operations.AddRange(newops)

    let verso num k color (reaction,count) (mundial: AddedFonts) (futura: AddedFonts) text (builder: PdfDocumentBuilder) =
        let src = cartes.GetPage(template color EscaladeVerso)
        let page = copyWithBoxes src builder
        let ops = page.CurrentStream.Operations |> Seq.toList
        let accent = getColor (cartes.GetPage(template color ReactionRecto).Operations |> Seq.toList)

        let newops =
            ops
            |> updateText (function 
                | ops & InnerText(txt) when txt.StartsWith("ESCALADE") ->
                    let size =  getTextMatrix ops  |> getSize
                    let font = page.GetFonts(mundial)
                    let text = split font [ st Bold, $"ESCALADE %d{num} %c{k}"] size 1m (decimal page.PageSize.Width)

                    ops
                    |> centerPage src 
                    |> changeText page text (decimal page.PageSize.Width) Center Top accent



                | ops & InnerText(txt) when txt.StartsWith("Texte") ->
                    let m = getTextMatrix ops
                    let size =  getSize m
                    let pos = getPos m
                    let trimbox = src.Dictionary.Data[NameToken.TrimBox] |> rect

                    let width = decimal trimbox.Width - (pos.X - decimal trimbox.Left) * 2m + decimal trimbox.Left
                    let font = page.GetFonts(futura)
                    let lines = split font text size 1.5m width 

                    (ops
                    |> changeMatrix (translate 0m -15m)
                    |> changeText page lines width Left Midle accent)
                    @ [ setColor accent ]

                | ops & InnerText(txt) when txt = "|" ->
                    let font = page.GetFonts(mundial)
                    let size = getTextMatrix ops |> getSize 
                    let lines =
                        split font [ 
                                st Bold, $"%d{reaction}" 
                                st Regular, $" | " 
                                st Bold, $"%d{count}" 
                            ] size 1m (decimal page.PageSize.Width)
                    ops |> changeText page lines (decimal page.PageSize.Width) Center Top accent
                | _ -> []
            )

        page.CurrentStream.Operations.Clear()
        page.CurrentStream.Operations.AddRange(newops)


module Cards =
    type Situation =
        { Id: int 
          Title: string
          Color: Colors
          Dice: int
          Text: (Style * string) list 
          Reactions: Reaction list 
          Escalades: Map<char, Reaction> }
    and Reaction =
        { Title: string
          Text: (Style * string) list
          Consequences: Consequence list }    
    and Consequence =
        { Range: Range
          Text: (Style * string) list
          Score: Score }
    and Range = { Min: int; Max: int}
        with
            member this.Percents = (this.Max + 1 - this.Min) * 10
            member this.Probability = decimal this.Percents / 100m
    and Score =
        | Score of (int * string) option * char list



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
    toText' { FontStyle = Regular; Color = DefaultColor } spans
    

open Cards
open Microsoft.FSharp.Core.CompilerServices
open System.Collections


let rangeRx = System.Text.RegularExpressions.Regex(@"^\s*(\d+)(\s+à\s+(\d+))?\s*:\s*")
let scoreRx = System.Text.RegularExpressions.Regex(@"\s*\(([+\-]?\d)([^)+]*)(\s*\+ Escalade\s*)?\)\s*")
let escaladeRx = System.Text.RegularExpressions.Regex(@"\(\s*voir\s+Escalade(\s+\$)?\s*\)")

let parseTitle (spans: MarkdownSpans) =
    (System.Text.StringBuilder(), spans)
    ||> List.fold (fun acc span ->
            match span with
            | Literal(text,_)  -> acc.Append text
            | _ -> acc
    )
    |> string
    |> fun s -> s.Trim()


type ConsequenceMd = MarkdownSpans * MarkdownParagraphs
type ReactionMd = MarkdownSpans * ConsequenceMd list

let textToString (text: (Style * string) list) =
    let builder = System.Text.StringBuilder()
    for _,t in text do
        builder.Append(t) |> ignore
    string builder

let tryParseLink (description: MarkdownSpans) =
    match description with
    | IndirectLink(body,_,_,_) :: _ ->
        let text = toText body
        Some (textToString text)
    | _ -> None

let rec parseEscalade (description, items) : Reaction option =
    match tryParseLink description with
    | Some _ -> None
    | None -> 
        let text = toText description
        let title = textToString text
        let conseqs = List.map (parseConsequence Map.empty) items
        Some
            { Title = title
              Text = text
              Consequences = conseqs }

and extractEscalade ps =
    match ps with
    | Span(description,_) :: ListBlock(_,items,_) :: _ ->
        description, List.map extractConsequence items
    | Span(description,_) :: _ ->
        description, []
    | _ -> failwith "Invalid escalade format"
    

and extractEscalades ps =
    match ps with
    | ListBlock(_, cases,_) :: _ ->
        List.map extractEscalade cases
    | h :: _ ->
        printfn $"Invalid escalades format %A{h}"
        []
    | [] ->
        []

and extractConsequence ps : ConsequenceMd =
    match ps with
    | Span(description, _) :: tail 
    | Paragraph( description , _) :: tail ->
        description, tail
    | s :: _ -> failwith $"{ s.GetType().Name }" 
    | [] -> failwith "Empty consequence"

and parseConsequence escalades ((description, escaladesMd): ConsequenceMd)  : Consequence =
    let range= 
        description |> List.tryPick ( function
            | Literal(txt,_) ->
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
    let escaladeKeys =
        escaladesMd
        |> extractEscalades
        |> List.choose (fun (description,_) ->
            match tryParseLink description with
            | None -> 
                let title = description |> toText |> textToString
                Map.tryFindKey (fun _ e -> e.Title = title) escalades
            | Some link ->
                Map.tryFindKey (fun _ e -> e.Title.Contains (link.Trim())) escalades
        )
        |> List.sort

    
    let score = 
        let s =
            description |> List.tryPick ( function
                    | Literal(txt,_) ->
                        let m = scoreRx.Match(txt)
                        if m.Success then
                            Some (int m.Groups[1].Value, m.Groups[2].Value)
                        else
                            None
                    | _ -> None)
        Score(s, escaladeKeys)
        

    let text =
        description
        |> List.map (function
            | Literal(txt,r) when rangeRx.IsMatch(txt) ||scoreRx.IsMatch(txt) || escaladeRx.IsMatch(txt) -> 
                let txt2 = rangeRx.Replace(txt,"")
                let txt3 = scoreRx.Replace(txt2,"")
                Literal(escaladeRx.Replace(txt3,""),r)
            | t -> t
        ) 


    { Range = range |> Option.defaultValue { Min = 0; Max = 0}
      Text = toText text
      Score = score }




let extractReaction ps : ReactionMd =
    match ps with
    | Span(description,_) :: ListBlock(_,items,_) ::_ ->
        description, List.map extractConsequence items
    | [ Span(description,_) ] ->
        description, []
    | _ -> failwith $"Format de réaction invalide:\n%A{ps}"


let parseReaction escalades (description, items) =
    let conseqs = List.map (parseConsequence escalades) items 
    let text = toText description
    { Title = textToString text
      Text = text
      Consequences = conseqs
    }



let extractReactions ps : ReactionMd list * MarkdownParagraphs =
    match ps with
    | ListBlock(_, items,_) :: tail ->
        List.map extractReaction items, tail
    | tail ->  [], tail

let escaladeId (i: int) =
    char(int 'A' + i)            

let parseSituations (dice: int seq) (md : MarkdownDocument) = 
    let rec loop id (edice: IEnumerator<int>) ps result =
        match ps with
        | Heading(2, title,_) :: Paragraph(txtSituation,_) :: tail ->
            
            let reactionsMd, tail = extractReactions tail

            let escaladesMd = 
                [ for _,conseqsMd in reactionsMd do
                    for _,escaladesMd in conseqsMd do
                        yield! extractEscalades escaladesMd ]
            let escalades =
                escaladesMd
                |> List.choose parseEscalade



                |> Seq.mapi (fun i e -> escaladeId i,e)
                |> Map.ofSeq
            
            let reactions = List.map (parseReaction escalades) reactionsMd

            let situation =
                { Id = id
                  Title = parseTitle title
                  Color = 
                    let id = id%25
                    if id <= 5 then
                        Blue
                    elif id <= 10 then
                        Red
                    elif id < 15 then
                        Yellow 
                    elif id < 20 then
                        Purple
                    else
                        Green

                  Dice = 
                    edice.MoveNext() |> ignore
                    edice.Current
                  Text = toText txtSituation
                  Reactions = reactions
                  Escalades = escalades
                   } 

            loop (id+1) edice tail (situation :: result)
        | [] -> List.rev result
        | head :: tail ->
            loop id edice tail result

    use edice = dice.GetEnumerator()
    loop 1 edice md.Paragraphs []

let renderConsequence (reaction: Reaction) =
    let plusEscalade ids =
        match ids with
        | [] -> ""
        | _ -> 
            let list = ids |> List.map string |> String.concat ""
            $" + Escalade %s{list}" 

    [ for consequence in reaction.Consequences do
        if consequence.Range.Min = consequence.Range.Max then
            accent Bold, $"%d{consequence.Range.Min} : "
        else
            accent Bold, $"%d{consequence.Range.Min} à {consequence.Range.Max} : "
        yield! consequence.Text |> List.map (fun (s,t) -> { Color = Color Color.black; FontStyle = match s.FontStyle with Bold -> Regular | x -> x },t)
        match consequence.Score with
        | Score (Some(score,txt),ids) when score > 0 ->

            Bold ++ Color.green, $" (+%d{score}%s{txt}%s{plusEscalade ids})\n"
        | Score(Some(score,txt), ids) when score < 0 ->
            Bold ++ Color.red, $" (-%d{-score}%s{txt}%s{plusEscalade ids})\n"
        | Score (Some(_,txt), ids) ->
            Bold ++ Color.yellow, $" (0%s{txt}%s{plusEscalade ids})\n"
        | Score(None, ids) ->
            let list = ids |> List.map string |> String.concat ""
            
            accent Bold , $" (Escalade %s{list})\n"
    ]
let renderSituation (situation: Situation) mundial futura builder =
    printfn "%s" situation.Title
    Situation.verso situation.Dice mundial builder
    Situation.recto situation.Id situation.Color mundial futura situation.Text builder
    let count = situation.Reactions.Length
    for i, reaction in situation.Reactions |> List.indexed do
        let text = reaction.Text |> List.map (fun (s,t) -> {s with FontStyle = match s.FontStyle with Bold -> Regular | x -> x },t)
        Reaction.recto situation.Id situation.Color mundial futura text builder

        let consequences = renderConsequence reaction

        Reaction.verso situation.Id situation.Color (i+1, count) mundial futura consequences builder

    let escalades = 
        let count = Map.count situation.Escalades
        [ for i,(k,e) in situation.Escalades |> Map.toSeq |> Seq.indexed do
            (i+1, count), k, e
        ]

    for (i,count),k, escalade in escalades do
        let text = escalade.Text |> List.map (fun (s,t) -> {s with FontStyle = match s.FontStyle with Bold -> Regular | x -> x },t)
        Escalade.recto situation.Id k situation.Color mundial futura text builder

        let consequences = renderConsequence escalade

        Escalade.verso situation.Id k situation.Color (i, count) mundial futura consequences builder

let renderSituations situations mundial futura builder=
    for situation in situations do
        renderSituation situation mundial futura builder
let rmdRx = System.Text.RegularExpressions.Regex(@"(\s*)(\*\*|_)( *)")
let punctRx = System.Text.RegularExpressions.Regex(@"(\s*)(\.\.\.|\.|,|\?[!?]?|\![!?]?|;|:|')( *)")
let quoteRx = System.Text.RegularExpressions.Regex(@"[""“”«]\s*([^""“”»]*?)\s*[""“”»]( +)?")
let normSpaceRx = System.Text.RegularExpressions.Regex(@"(\*|_) +")



let cleanMd (md: string) = 
    let md2 =
        rmdRx.Replace(md, (fun m ->
            match m.Groups[2].Value with
            | "**" -> m.Groups[1].Value + m.Groups[3].Value
            | "_" -> m.Groups[1].Value + m.Groups[3].Value
            | _ -> m.Value )
        )
    let md3 =
        punctRx.Replace(md2, (fun m ->
            match m.Groups[2].Value with
            | "." -> ". "
            | "," -> ", "
            | ";" -> "; "
            | ":" -> " : "
            | "'" -> "’"
            | "..." -> "… "
            | s when s.StartsWith("?") || s.StartsWith("!") -> $"\xA0{s} "
            | _ -> m.Value )
        )
    let md4 =
       quoteRx.Replace(md3, (fun m -> 

        if m.Groups[2].Success then
            "_«\xA0" + m.Groups[1].Value + "\xA0»_ "
        else
            "_«\xA0" + m.Groups[1].Value + "\xA0»_"
            )
        )
    let md5 =
        normSpaceRx.Replace(md4, (fun m -> m.Groups[1].Value + " "))
    md5

let futuraBytes = File.ReadAllBytes(@"..\Fonts\Futura PT\FuturaPTBook.ttf")
let futuraIBytes = File.ReadAllBytes(@"..\Fonts\Futura PT\FuturaPTBookOblique.ttf")
let futuraBBytes = File.ReadAllBytes(@"..\Fonts\Futura PT\FuturaPTMedium.ttf")
let mundialLBytes = File.ReadAllBytes(@"..\Fonts\Mundial\MundialLight.ttf")
let mundialBBytes = File.ReadAllBytes(@"..\Fonts\Mundial\MundialBold.ttf")

let parse path =
    let mdText = 
        File.ReadAllText(path)
        |> cleanMd

    let md = Markdown.Parse(mdText)
    let rand = System.Random(42)
    let dice =
        seq {
            while true do
                yield! List.sortBy (fun _ -> rand.Next()) [1..10]
        }

    let situations = parseSituations dice md
    situations

let render situations =
    let builder = new PdfDocumentBuilder()
    let mundial = 
        let font = builder.AddTrueTypeFont(mundialLBytes)
        { AddedFonts.Regular = font
          Italic = font
          Bold = builder.AddTrueTypeFont(mundialBBytes) }

    let futura =
        { AddedFonts.Regular = builder.AddTrueTypeFont(futuraBytes)
          Italic = builder.AddTrueTypeFont(futuraIBytes)
          Bold = builder.AddTrueTypeFont(futuraBBytes) }

    renderSituations situations mundial futura builder

    File.WriteAllBytes(@"Result.pdf", builder.Build())



// r.GetPage(template Red EscaladeRecto).Operations |> Seq.toList  |> printOps
// |> Seq.filter (function 
//     | :? SetNonStrokeColorDeviceRgb -> true
//     | _ -> false)
//  |> printOps

// cartes.GetPage(1).Operations |> Seq.toList |> printOps
let situationScore situation =
    let scores =
        [ let reactionProba = 1m / decimal situation.Reactions.Length
          for reaction in situation.Reactions do
            for cons in reaction.Consequences do
                    match cons.Score with
                    | Score(Some(n,_),_) -> 
                        reactionProba * cons.Range.Probability * decimal n
                    | Score(None,[]) -> 0m
                    | Score(None, es) ->
                        let escaladeReactionProba = 1m / decimal es.Length
                        for k in es do
                            let reac = situation.Escalades[k]
                            for c in reac.Consequences do
                                for j in c.Range.Min .. c.Range.Max do
                                    match c.Score with
                                    | Score(Some(n,_),_) ->
                                       reactionProba * cons.Range.Probability *
                                        escaladeReactionProba * c.Range.Probability * decimal n
                                    | _ -> 0m
        ]
    scores |> List.sum

let situationCards situation =
    1 + situation.Reactions.Length + situation.Escalades.Count

let cut len (s: string) =
    if s.Length >= len-1 then
        s.Substring(0,len-1) + "…"
    else
        s 

let check (situations: Situation list) =
    let checks =
        [
            for situation in situations do
                let errors = ResizeArray()
                let warn (s:string) =
                    errors.Add("\x1b[33m" + s + "\x1b[0m")

                let checkRanges name reaction =
                    if reaction.Consequences = [] then
                        warn $"  [{name}] consequences manquantes \x1b[38;2;128;128;128m/ { textToString reaction.Text |> cut 40}"
                    let ranges =
                        [ for cons in reaction.Consequences do
                            yield! [cons.Range.Min .. cons.Range.Max] ]
                    if List.contains 0 ranges then
                        warn $"  [{name}] pourcentage non spécifée \x1b[38;2;128;128;128m/ { textToString reaction.Text |> cut 40}"

                    let missing = (set [1..10] - set ranges)
                    if not missing.IsEmpty then
                        if missing = set [1..10] then
                            warn $"  [{name}] pourcentages manquants \x1b[38;2;128;128;128m/ { textToString reaction.Text |> cut 40}"
                        else
                            let m = missing |> Seq.map string |> String.concat ", "
                            warn $"  [{name}] pourcentages {m} manquants \x1b[38;2;128;128;128m/ { textToString reaction.Text |> cut 40}"
                    let multi = ranges |> List.countBy id |> List.filter (fun (_,c) -> c > 1) |> List.map fst
                    if not multi.IsEmpty then
                        for n in multi do
                            warn $"  [{name}] valeur {n} multiple \x1b[38;2;128;128;128m/ { textToString reaction.Text |> cut 40}"

                if situation.Text = [] then 
                    warn "  texte situation manquant"
                if situation.Reactions = [] then
                    warn "  reactions manquantes"
                else
                    for i,reaction in situation.Reactions |> Seq.indexed do
                        if reaction.Text = [] then
                            warn $"  [reaction {i+1}] texte manquant"
                        checkRanges $"reaction {i+1}" reaction

                        for cons in reaction.Consequences do
                            match cons.Score with
                            | Score(None, []) ->
                                    warn $"  [reaction {i+1}] score manquant \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
                            | Score(None, _) -> ()
                            | Score(Some(n,_),_) -> 
                                if n > 3 then
                                    warn $"  [reaction {i+1}] score trop grand ({n}) \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
                                elif n < -3 then
                                    warn $"  [reaction {i+1}] score trop petit ({n}) \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
                                
                    for k,escalade in situation.Escalades |> Map.toSeq do
                        if escalade.Text = [] then
                            warn $"  [escalade {k}] texte manquant"
                        checkRanges $"escalade {k}" escalade
                        for c in escalade.Consequences do
                            match c.Score with
                            | Score(Some(n,_),_) ->
                                if n > 3 then
                                    warn $"  [escalade {k}] score trop grand ({n}) \x1b[38;2;128;128;128m/ {textToString  c.Text |> cut 40 }"
                                elif n < -3 then
                                    warn $"  [escalade {k}] score trop petit ({n}) \x1b[38;2;128;128;128m/ {textToString  c.Text |> cut 40 }"
                            | Score(None,[]) ->
                                    warn $"  [escalade {k}] score manquant \x1b[38;2;128;128;128m/ {textToString  c.Text |> cut 40 }"

                            | _ -> ()

                    let usedEscaladeKeys =
                        [ for r in situation.Reactions do
                            for c in r.Consequences do
                                match c.Score with
                                | Score(_,es) -> yield! es
                                | _ -> ()
                        ]
                        |> set

                    let unusedEscalades = set situation.Escalades.Keys - usedEscaladeKeys
                    if not unusedEscalades.IsEmpty then
                        let ls = unusedEscalades |> Seq.map string |> String.concat ""
                        warn $"  escalades {ls} non utilisées"
                let result = 
                    if errors.Count = 0 then
                        let score = situationScore situation

                        Ok score
                        
                    else
                        Error errors
                situation,   result
        ]
        |> List.sortBy(function
            | _,Ok _ -> 1
            | _ -> 0)

    for situation, result in checks do
        let cards = situationCards situation
        let title = cut 40 situation.Title
        match result with
        | Ok score ->
            printfn "✅ S%d %s \x1b[38;2;128;128;128m(%d réactions / %d escalades / %d cards) \x1b[32m(score %.2f)\x1b[0m" situation.Id title situation.Reactions.Length situation.Escalades.Count cards score
        | Error errors ->
            printfn "❌ S%d %s \x1b[38;2;128;128;128m(%d réactions / %d escalades / %d cards)\x1b[0m" situation.Id title situation.Reactions.Length situation.Escalades.Count cards
            for error in errors do
                printfn "%s" error

    [ for situation,result in checks do
        match result with
        | Ok _ -> situation
        | Error _ -> ()
    ]



fsi.ShowDeclarationValues <- false
let situations = parse @"situations.md"
check situations
|> render
// render situations
// render (situations |> List.filter (fun m -> m.Title.Contains("pissotière") ))

// File.ReadAllText("Situations.md") |> cleanMd
// |> fun t -> File.WriteAllText("Sit.txt", t)

// let rec find txt ps =
//     match ps with
//     | Heading(2, title,_) :: Paragraph(txtSituation,_) :: tail when (textToString (toText title)).Contains(txt: string) ->
//         ps
//     | head :: tail -> find txt tail
//     | [] -> []



// let ps = find "main aux fesses" (File.ReadAllText("situations.md") |> cleanMd |> Markdown.Parse ).Paragraphs
// let r = PdfDocument.Open(@"C:\Users\jchassaing\OneDrive\Transmissions\template\Transmission(s)_Cartes Situation (Template).pdf")
// let r = PdfDocument.Open(@"C:\Users\jchassaing\OneDrive\Transmissions\template\Result.pdf")
// r.GetPage(2).Operations |> printOps



let builder = new PdfDocumentBuilder()
let futura =
    { AddedFonts.Regular = builder.AddTrueTypeFont(futuraBytes)
      Italic = builder.AddTrueTypeFont(futuraIBytes)
      Bold = builder.AddTrueTypeFont(futuraBBytes) }

let page = builder.AddPage(PageSize.A7)
let font = page.GetFonts(futura)
// let size = font.Regular.Font.GetFontMatrix() 
let lines = split font [ st Regular, "Cecixx est une phrase tres long qui tient sur plusieurs lignes" ] { Width = 9m; Height = 9m} 1.5m 100m
let m = font.Regular.Font.GetFontMatrix()
lines[0].Spans[0].Text.TrimEnd()
|> Seq.sumBy (fun c -> 
    let w = font.Regular.Font.TryGetAdvanceWidth(c) |> snd
    decimal (m.Transform(PdfRectangle(0.,0.,w,0.)).Width) * 9m
 )
lines[0].Size.Width

let situation = situations |> List.find (fun s -> s.Title.Contains("main aux fesses"))
situation.Title
let escalades = situation.Escalades |> Map.toSeq |> Seq.map snd |> Seq.toList
escalades |> List.length
escalades |> List.distinct |> List.length

escalades |> List.map (fun m -> m.Title) |> List.distinct 

for e in escalades |> List.filter (fun m -> m.Title = "Je me tais pour ne pas envenimer la situation.") do
    printfn $"{e.Title}"
    for c in e.Consequences do
        printfn $"  {c.Range.Min} => {c.Range.Max} {cut 40 (textToString c.Text)} ({c.Score})"
