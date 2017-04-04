module CardVisual

module geom =
    open System

    let polygon (yCentre, xCentre) r phi n = 
        let k = float n
        let y i = yCentre + r * sin(phi + 2.0 * Math.PI * i / k);
        let x i = xCentre + r * cos(phi + 2.0 * Math.PI * i / k);
        [for i = 0.0 to k - 1.0 do yield x i, y i]
    //polygon (0.0, 0.0) 10.0 (Math.PI * 3.0 / 2.0) 10

open System.Collections.Generic;
open System.Drawing;
open System.IO
open CommandInterpriter
open FsharpMyExtension.FSharpExt

let cropCards (x,y) (cardWidth, cardHeight) (intervalX, intervalY) (cardCountX, cardCountY) =
    let f x count width interval = 
        let rec f acc curr = function
            | 0 -> acc
            | i -> 
                let n = curr + width
                f (curr :: acc) (n + interval) (i - 1)
        f [] x count |> List.rev
    let xs = f x cardCountX cardWidth intervalX
    let ys = f y cardCountY cardHeight intervalY
    [for y in ys do
        yield [for x in xs do yield x, y]]
    |> List.map (List.map (fun (x, y) -> Rectangle(x, y, cardWidth, cardHeight)))

let copy src (section:Rectangle) =
    let bmp = new Bitmap(section.Width, section.Height);
    let g = Graphics.FromImage(bmp);
//  g.DrawImage(src, 0, 0, section, GraphicsUnit.Pixel);
    let secDest = Rectangle(Point(0, 0), section.Size )
    g.DrawImage(src, secDest, section, GraphicsUnit.Pixel);
    g.Dispose();
    bmp;
let cardWidth, cardHeight = (261, 370)
    
    //Image.FromFile(@"c:\All\video\147_objects\card deck.png")
let rankCount, suitCount = (13, 4)
let initPics () = 
    let rects = cropCards (168, 99) (cardWidth, cardHeight) (20, 25) (rankCount, suitCount)
    let src = WindowsFormsApplication1.Properties.Resources.card_deck
    let cards = List.map (List.map (copy src)) rects

    let path = @"pics\"
    if Directory.Exists path |> not then Directory.CreateDirectory path |> ignore
    cards |> List.iteri (fun s xs -> xs |> List.iteri (fun r img -> img.Save(path + r.ToString() + "_" + s.ToString() + ".png", Imaging.ImageFormat.Png)))

    cards
let suitIdToImg = 
    //бубна, креста, черва, пика
    //diamonds, clubs, hearts, spades
    let s = 
        [ WindowsFormsApplication1.Properties.Resources.diamonds;
            WindowsFormsApplication1.Properties.Resources.clubs;
            WindowsFormsApplication1.Properties.Resources.hearts;
            WindowsFormsApplication1.Properties.Resources.spades; ]
        |> List.mapi (fun i x -> i, x) |> Map.ofList
    (fun x -> Map.tryFind x s)

let rankNameToId = 
    let ns = [2..10] |> List.map (fun x -> x.ToString())
    let ls = [ "jack"; "queen"; "king"; "ace" ]
    let m = ns@ls |> List.mapi (fun i x -> x, i) |> Map.ofList
    
    flip Map.tryFind m
    //m.TryFind

let get() = 
    printfn "loading resources..."
        
    let cards = 
        let f r s = WindowsFormsApplication1.Properties.Resources.ResourceManager.GetObject("_" + r.ToString() + "_" + s.ToString())
        [ for r = 0 to rankCount - 1 do 
            yield [ for s = 0 to suitCount - 1 do
                        yield f r s :?> Bitmap ] ]
    //let cards = initPics()
    printfn "done!"
    (fun ({Rank = rank; Suit = suit}) -> (cards.[rank]).[suit])
    
let place (bmp:Image) cards (get:CommandInterpriter.PlayingCard -> Bitmap) = //(cards:(int*int) list) = 
    let rects = 
        let inline rects w l n = 
            if (float n)*w <= l then 
                let s = l / float(n+1) //in [ s .. s .. l - s]
                List.init n (fun i -> (float (i+1)) * s)
            else
                let s = (l - w) / float(n - 1) //in [ w / 2.0 .. s .. l - w / 2.0 ]
                [ yield w / 2.0; yield! List.init (n-1) (fun i -> float (i+1) * s + w / 2.0 ) ]
            |> List.map (fun x -> x - w / 2.0)
            |> List.map int
        //rects 1.0 10.0 3
        //rects 3.0 5.0 6
            
        let k = float bmp.Height / float cardHeight
        let w,h = (float cardWidth * k) |> int, bmp.Height
        let lenght = bmp.Size.Width

        rects (float w) (float lenght) (Seq.length cards)
        |> List.map (fun x -> Rectangle(x, 0, w, h))

    use g = Graphics.FromImage bmp
    use bmpReg = new Region(Rectangle(Point(0, 0), bmp.Size))

    //g.FillRegion(Brushes.Gray, bmpReg)
    use brush = new System.Drawing.SolidBrush(System.Drawing.SystemColors.Control)
    g.FillRegion(brush, bmpReg)

    let imgs = Seq.map get cards
    Seq.zip imgs rects
    |> Seq.iter (fun (img, rect) -> g.DrawImage(img, rect, Rectangle(Point(0,0), img.Size), GraphicsUnit.Pixel))
(*
let dirOutput = @"c:\All\Output\"
rects |> List.mapi (fun i x -> i, x)
|> List.iter (fun (i, r) ->
    let bmp = copy src r
    bmp.Save(dirOutput + i.ToString() + ".png", Imaging.ImageFormat.Png)) *)