module Program
#nowarn "40"
open System
open System.Threading
open System.IO;
open System.Net;
open System.Net.Sockets;
open System.Windows.Forms
open CommandInterpriter

//printfn "thread create: %A" Thread.CurrentThread.ManagedThreadId

module geom =
    let polygon (y_C, x_C) R phi n = 
        let k = float n
        let y i = y_C + R * sin(phi + 2.0 * Math.PI * i / k);
        let x i = x_C + R * cos(phi + 2.0 * Math.PI * i / k);
        [for i = 0.0 to k - 1.0 do yield x i, y i]
    //polygon (0.0, 0.0) 10.0 (Math.PI * 3.0 / 2.0) 10
module CardVisual = 
    open System.Collections.Generic;
    open System.Drawing;

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
//        g.DrawImage(src, 0, 0, section, GraphicsUnit.Pixel);
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
    
    let rankNameToId = 
        let ns = [2..10] |> List.map (fun x -> x.ToString())
        let ls = [ "jack"; "queen"; "king"; "ace" ]
        let m = ns@ls |> List.mapi (fun i x -> x, i) |> Map.ofList

        (fun x -> m.TryFind x)

    let get() = 
        printfn "loading resources..."
        
        let cards = 
            let f r s = WindowsFormsApplication1.Properties.Resources.ResourceManager.GetObject("_" + r.ToString() + "_" + s.ToString())
            [ for r = 0 to rankCount - 1 do 
                yield [ for s = 0 to suitCount - 1 do
                            yield f r s :?> Bitmap ] ]
        //let cards = initPics()
        printfn "done!"
        (fun (rank, suit) -> (cards.[rank]).[suit])
    
    let place (bmp:Image) cards (get:int*int -> Bitmap) = //(cards:(int*int) list) = 
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

            let w,h = cardWidth / 2, cardHeight / 2
            let lenght = bmp.Size.Width

            rects (float w) (float lenght) (List.length cards)
            |> List.map (fun x -> Rectangle(x, 0, w, h))

        use g = Graphics.FromImage bmp
        use bmpReg = new Region(Rectangle(Point(0, 0), bmp.Size))

        //g.FillRegion(Brushes.Gray, bmpReg)
        use brush = new System.Drawing.SolidBrush(System.Drawing.SystemColors.Control)
        g.FillRegion(brush, bmpReg)

        let imgs = List.map get cards
        List.zip imgs rects
        |> List.iter (fun (img, rect) -> g.DrawImage(img, rect, Rectangle(Point(0,0), img.Size), GraphicsUnit.Pixel))
    (*
    let dirOutput = @"c:\All\Output\"
    rects |> List.mapi (fun i x -> i, x)
    |> List.iter (fun (i, r) ->
        let bmp = copy src r
        bmp.Save(dirOutput + i.ToString() + ".png", Imaging.ImageFormat.Png)) *)

module GUI = 
    type readDel = delegate of (string -> unit) -> unit
    type showCardsDel = delegate of (int*int) list -> unit
    type SetTextCallback = delegate of string -> unit
    
    open System.Drawing

    type MainForm () as this = 
        //inherit Form()
        inherit WindowsFormsApplication1.Form1()
        
        let bmp = new Bitmap(500, 400)

        do
            this.button1.Enabled <- false
            this.DoubleBuffered <- true
            this.textBox2.TextChanged.Add
                (fun _ -> this.textBox2.SelectionStart <- this.textBox2.Text.Length; this.textBox2.ScrollToCaret();)
            let bmpRect = Rectangle(Point(0,0), bmp.Size)
            this.Paint.Add(fun (e:PaintEventArgs) -> e.Graphics.DrawImage(bmp, bmpRect))
        
        let get = CardVisual.get()
        
        let rec showcards cards =
            if this.InvokeRequired then
                let d = showCardsDel showcards
                this.Invoke(d, [| box cards |]) |> ignore
            else
                CardVisual.place bmp cards get
                this.Invalidate()

        let rec print s = 
            if this.InvokeRequired then
                let d = SetTextCallback print
                this.Invoke(d, [| box s |]) |> ignore
            else 
                this.textBox2.AppendText (s + Environment.NewLine)
        let rec read (f:string -> unit) =
            if this.button1.InvokeRequired then
                let d = readDel read
                this.Invoke(d, [| box f |]) |> ignore
            else
                printfn "thread start: %A" Thread.CurrentThread.ManagedThreadId
                this.button1.Enabled <- true
                this.textBox1.Focus() |> ignore
                let rec h = new EventHandler(fun _ _ -> 
                        let s = this.textBox1.Text
                        this.textBox1.Text <- ""
                        this.button1.Click.RemoveHandler h
                        this.button1.Enabled <- false
                        async{f s} |> Async.Start)
                this.button1.Click.AddHandler h
        member __.Input x = read x
        member __.Print x = print x
        member __.ShowCards x = showcards x

module Core = 
    let writeRead stream write = 
        let req = ClientReq.unpars write
        StreamToStream req stream
        let ms = ServerAnswer.pars stream
        ms

    let stream =
        let connect () =
            let ip = "127.0.0.1"
            let client = new TcpClient();
            client.Connect(IPAddress.Parse ip, 5000)
            client.GetStream()
        let rec f () = try connect () with e -> e.Message |> printfn "%s"; Threading.Thread.Sleep 500; f()
        in f()

    let form = new GUI.MainForm()
    let main () =
        let print = form.Print
        let cards = System.Collections.Generic.List<int*int>()
        let showcards () = cards.ToArray() |> List.ofArray |> form.ShowCards
        
        let inter = function
            | ServerAnswer.Write str -> print str
            | ServerAnswer.Info msg -> 
                match msg with
                | (ServerAnswer.InfoMsg.CardAdd { ServerAnswer.Rank = ServerAnswer.Rank rank; ServerAnswer.Suit = ServerAnswer.Suit suit }) as x -> cards.Add(rank, suit); showcards (); //print (sprintf "%A" x)
                | (ServerAnswer.InfoMsg.CardRemove { ServerAnswer.Rank = ServerAnswer.Rank rank; ServerAnswer.Suit = ServerAnswer.Suit suit }) as x-> cards.Remove(rank, suit) |> ignore; showcards (); //print (sprintf "%A" x)
                | x -> print (sprintf "%A" x)
            | x -> print (sprintf "%A" x)
                 
        let update cmd read =
            let rec update latest = 
                Thread.Sleep 100
            
                let fin x =
                    match latest with
                    | Some prev -> 
                        if prev = x then update latest else cmd x; update (Some x)
                    | None -> 
                        cmd x; update (Some x)

                match writeRead stream ClientReq.Getstate with
                | ServerAnswer.cmd.GetRank ->
                    print "input rank [2..10; jack; queen; king; ace]"
                    let rec f s = 
                        match CardVisual.rankNameToId s with
                        | None -> print "not identify. Try again"; read f
                        | Some s -> 
                            writeRead stream (ClientReq.Rank s)
                            |> sprintf "%A" |> print
                            update None
                    read f
                | ServerAnswer.Read ->
                    let f s = 
                        writeRead stream (ClientReq.Write s)
                        |> sprintf "%A" |> print
                        update None
                    read f
                | x -> fin x
            update None

        let login () =
            form.Print "input name:"
            let rec f name = 
                match writeRead stream (ClientReq.EnterBy name) with
                | ServerAnswer.cmd.Success true -> 
                    form.Print "login success"
                    match writeRead stream ClientReq.GetCards with
                    | ServerAnswer.cmd.Cards c -> 
                        cards.AddRange c
                        showcards ()
                        update inter form.Input
                    | _ -> failwith "должно было быть ServerAnswer.cmd.Cards"
                
                | ServerAnswer.cmd.Success false -> 
                    form.Print "name busied. Try again."; form.Input f
                | x -> failwithf "EnterBy return %A" x
            form.Input f
            ()

        match writeRead stream ClientReq.Getstate with
        | ServerAnswer.LoginReq -> login()
        | ServerAnswer.SlotBeAbsent -> form.Print "Мест нет. Можно выходить"
        | x -> failwithf "Getstate return %A" x

    let threadId = Thread.CurrentThread.ManagedThreadId
    async { main () } |> Async.Start

(*
    let login () =
        print "input name:"
        let rec getNameFunc = 
            new EventHandler(fun _ _ -> 
                name <- this.textBox1.Text;
                this.textBox1.Text <- ""
                match writeRead stream (ClientReq.EnterBy name) with
                | ServerAnswer.cmd.Success true -> 
                    this.button1.Click.RemoveHandler getNameFunc
                    update()
                    print "login success"
                | ServerAnswer.cmd.Success false -> print "name busied. Try again."
                | _ -> failwith "сервер неправильно отвечает на попытку авторизации"
                )
                *)

module sandbox = 
    module noisy =
        type SetAction = Added | Removed
        type SetOperationEventArgs<'a>(value: 'a) =
            inherit System.EventArgs()
            member this.Value = value

        type SetOperationDelegate<'a> = delegate of obj * SetOperationEventArgs<'a> -> unit

        type NoisySet< 'a when 'a : comparison> () =
            let mutable m_set = Set.empty : Set<'a>
            let m_itemAdded = new Event<SetOperationDelegate<'a>, SetOperationEventArgs<'a>>()
            member this.Add x = 
                m_set <- m_set.Add x
                m_itemAdded.Trigger (this, new SetOperationEventArgs<_>(x))
            [<CLIEvent>]
            member this.ItemAddedEvent = m_itemAdded.Publish
    (*
        let nset = new NoisySet<int>()
        nset.ItemAddedEvent.Add(fun x -> printfn "val=%A" x.Value)
    
        nset.Add(10) *)

    let sandbox () =
        (*
        let conect () =
            let ip = "127.0.0.1"
            let client = new TcpClient();
            client.Connect(IPAddress.Parse ip, 5000)
            let stream = client.GetStream()
            let writer = new StreamWriter(stream)
            let reader = new StreamReader(stream)
            stream, reader, writer

        let stream, r, w = conect()
        //printfn "жду"
        *)
    
        let m = MailboxProcessor.Start(fun inbox -> let rec loop () = async{ return! loop () } in loop () )
        //m.
        (*
        Thread.Sleep 5000
        printfn "send..."
        let res = writeRead stream ClientReq.Getstate
        printfn "%A" res *)


        printfn "DONE!"
        Console.ReadKey() |> ignore

//let form = new Gui ()
//let form = new sandboxGUI ()
(*
let form = new sandbox.Form ()
form.ShowCards [0,1]
let rec f str = 
    async { 
        printfn "%s" str
        if str = "terminate" then ()
        else form.Input f} |> Async.Start
f "input any"
*)
Application.Run (Core.form)
//sandbox ()

