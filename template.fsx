#I @"C:\dev\PdfPig\src\UglyToad.PdfPig\bin\Debug\net6.0\"
#r @"UglyToad.PdfPig.Core"
#r @"UglyToad.PdfPig"
#r @"UglyToad.PdfPig.Tokens"
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
        | _ -> None)

type Font = 
    { Name: NameToken
      Font: IWritingFont }
type TextSpan = 
    { Text: string
      Font: Font }

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

// let changeText (page: PdfPageBuilder) addedFont (newTexts: string list) (ops: IGraphicsStateOperation list) =
//     ops 
//     |> List.collect (function
//         | ShowTextsWithPositioning(_,_) ->
//             match newTexts with
//             | [head] ->
//                 [ShowText(page.UseLetters(head, addedFont)) :> IGraphicsStateOperation]
//             | head :: tail ->
//                 UglyToad.PdfPig.Graphics.Operations.TextState.SetTextLeading(1m) :> IGraphicsStateOperation
//                 :: (ShowText(page.UseLetters(head, addedFont)) :> IGraphicsStateOperation)
//                 :: (tail |> List.map (fun t -> MoveToNextLineShowText(page.UseLetters(t,addedFont)) :> IGraphicsStateOperation))
//             | [] -> []
//         | op -> [op]
//          )

let changeText (page: PdfPageBuilder) (newTexts: TextSpan list list) (ops: IGraphicsStateOperation list) =
    let renderSpans newLine spans =
        match spans with
        | head :: rest ->
            [
                SetFontAndSize(head.Font.Name, 1m) :> IGraphicsStateOperation
                if newLine then
                    MoveToNextLineShowText(page.UseLetters(head.Text,head.Font.Font))
                else
                    ShowText(page.UseLetters(head.Text,head.Font.Font))
                for span in rest do
                    SetFontAndSize(span.Font.Name, 1m) :> IGraphicsStateOperation
                    ShowText(page.UseLetters(span.Text,span.Font.Font))
            ] 
        | [] -> []
    ops 
    |> List.collect (function
        | ShowTextsWithPositioning(_,_) ->
            match newTexts with
            | [line] ->
                renderSpans false line
            | line :: rest ->
                [
                    UglyToad.PdfPig.Graphics.Operations.TextState.SetTextLeading(1m) :> IGraphicsStateOperation
                    yield! renderSpans false line
                    for line in rest do 
                        yield! renderSpans true line
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

type Style =
    | Regular
    | Italic
    | Bold

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
            { Font = Fonts.getStyle ctx.Style ctx.Fonts
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
                    { Font = Fonts.getStyle ctx.Style ctx.Fonts
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
                    { Font = Fonts.getStyle ctx.Style ctx.Fonts
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
                let sp =
                    { Font = Fonts.getStyle ctx.Style ctx.Fonts
                      Text = ctx.Text.Substring(ctx.StyleStart, ctx.WordStart - ctx.StyleStart)}
                (ctx.Span, List.rev (sp :: ctx.Line)) :: ctx.Lines

        }

    let next ctx =
        let c = ctx.Text[ctx.Index]
        let style = ctx.Styles[ctx.Index]
        let font = Fonts.getStyle style ctx.Fonts
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
    // if ctx.Word.End >= text.Length then
    //     let ctx2 = LineContext.acceptWord ctx 
    //     let rest = LineContext.getText text ctx2 
    //     let full =
    //         if rest.Text.Length > 0 then
    //             (ctx2.Span.Width, rest) :: result
    //         else
    //             result

    //     List.rev full
    // else
    //     let c = text[ctx.Word.End]
    //     let f = if styles[ctx.Word.End] then fi else fn
    //     let w = 
    //         let m = f.Font.GetFontMatrix()
    //         match f.Font.TryGetAdvanceWidth(c) with
    //         | true,w -> 
    //             let b = PdfRectangle(0,0,w,0)
    //             let t = m.Transform(b)
    //             scale * decimal t.Width
    //         | false, _ -> 0m 
    //     if c = ' ' then
    //         let span2 = span |> TextWidth.addWord word |> TextWidth.addChar w 
    //         split' f fi span2 (TextWidth.next span2) text styles scale max result
    //     elif c = '\n' then
    //         let span2 = span |> TextWidth.addWord word
    //         let span3 = span2 |> TextWidth.addChar w
    //         split' f fi (TextWidth.next span3) (TextWidth.next span3) text styles scale max ((span2.Width, TextWidth.get span2 text)  :: result)
    //     else
    //         let word2 = TextWidth.addChar w word
    //         let span2 =  span |> TextWidth.addWord word2
    //         if span2.Width > max then
    //             split' f fi (TextWidth.next span) word2 text styles scale max ((span.Width, TextWidth.get span text) :: result)
    //         else
    //             split' f fi span word2 text styles scale max result

let split (f: Fonts) text styles sx max = 
    split' 
        { Span = 0m
          Word = 0m
          LineStart = 0
          WordStart = 0
          StyleStart = 0
          Index = 0
          Text = setInsec text
          Styles = styles
          Style = Regular
          Fonts = f
          Scale = sx
          Max = max
          Line = []

          Lines = []
        }
    // f fi TextWidth.zero TextWidth.zero  (setInsec text) styles sx max [] 

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

    let recto num mundial (futura: AddedFonts) text styles (builder: PdfDocumentBuilder) =
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
                    let font = page.GetFont(mundial)


                    ops
                    // |> changeFontName (page.GetAddedFont(mundial)) 
                    |> changeText page [ [ { Text = newTxt; Font = font} ] ]
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



// let p = cartes.GetPage(21)
// let (_,r) = p.Dictionary.TryGet<DictionaryToken>(NameToken.Resources)
// let (_,fs) = r.TryGet<DictionaryToken>(NameToken.Font)
// let fonts = fs.Data.Keys |> Seq.map (fun k -> p.GetFont(NameToken.Create k)) |> Seq.toList 
// let italic = fonts |> List.find (fun f -> f.Details.IsItalic)


// let txt = getText "Je lui" (Seq.toList p.Operations) 
// let sx = getSizeX txt
// let ft = getFont txt
// let t = getTextContent txt

// let f = p.GetFont(ft)
// f.Name
// printOps txt

// let text =
//     "Il est midi, tu sors le matin acheter du pain à la boulangerie la plus proche. Sur le trajet, c'est un homme qui te dit : « Ouais t'es trop belle ! Bisous partout. » Tu l’ignores et poursuit ton chemin vers la boulangerie. En sortant, tu dois repasser devant lui et il te dit : « Tu ne me donnes même pas un bisous ? Viens. »"
//     |> 

// split TextWidth.zero TextWidth.zero  text sx 180m [] 

// let span = TextWidth.zero
// let word = TextWidth.zero
// let scale = sx
// let max = 180m
// let result: (decimal * string) list = []


// let mutable w = 0m
// let bytes = new ByteArrayInputBytes( System.Text.Encoding.UTF8.GetBytes(t) )

// while bytes.MoveNext() do
//     let mutable len = 0
//     let c = f.ReadCharacterCode(bytes, &len)
//     // len
//     let b = f.GetBoundingBox(c)
//     w <- w + decimal b.Width

// w * 16m

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
    
let futuraBytes = File.ReadAllBytes(@"C:\Users\jchassaing\OneDrive\Transmissions\Fonts\Futura PT\FuturaPTBook.ttf")
let futuraIBytes = File.ReadAllBytes(@"C:\Users\jchassaing\OneDrive\Transmissions\Fonts\Futura PT\FuturaPTBookOblique.ttf")
let futuraBBytes = File.ReadAllBytes(@"C:\Users\jchassaing\OneDrive\Transmissions\Fonts\Futura PT\FuturaPTBold.ttf")
let mundialBytes = File.ReadAllBytes(@"C:\Users\jchassaing\OneDrive\Transmissions\Fonts\Mundial\MundialBold.ttf")

let builder = new PdfDocumentBuilder()
let futuraR = builder.AddTrueTypeFont(futuraBytes)
let futuraI = builder.AddTrueTypeFont(futuraIBytes)
let futuraB = builder.AddTrueTypeFont(futuraIBytes)
let mundial = builder.AddTrueTypeFont(mundialBytes)

let futura =
    { AddedFonts.Regular = futuraR
      Italic = futuraI
      Bold = futuraB }
Situation.verso 8 builder
let txt, styles =
    [ Regular, "Il est midi, tu sors acheter du pain à la boulangerie la plus proche. Sur le trajet, c'est un homme qui te dit : "
      Italic,"« Ouais t'es trop belle ! Bisous partout. »\n"
      Bold, "Tu l’ignores et poursuit ton chemin vers la boulangerie. En sortant, tu dois repasser devant lui et il te dit : "
      Italic, "« Tu ne me donnes même pas un bisous ? Viens. »"]
    |> extractStyle
Situation.recto 10 mundial futura  txt styles builder


// Situation.recto 10 mundial futura "En prenant le bus pour rentrer chez vous, un homme frotte son sexe contre vos fesses." builder

let page = builder.AddPage(PageSize.A7)
// let font = page.GetWritingFont(futura)


split futura txt styles 9m 180m

// let src = cartes.GetPage(2)
// let ops = getText "En" (Seq.toList src.Operations)
// let f = getFont ops
// let ft = src.GetFont(f)
// ft.GetBoundingBox(int 'E')

File.WriteAllBytes(@"C:\Users\jchassaing\OneDrive\Transmissions\template\Result.pdf", builder.Build())


// let r = PdfDocument.Open(@"C:\Users\jchassaing\OneDrive\Transmissions\template\Result.pdf")
// (snd (r.GetPage(2).Dictionary.TryGet<DictionaryToken>(NameToken.Resources))).TryGet<DictionaryToken>(NameToken.Font)
// r.GetPage(2).Operations |> printOps
