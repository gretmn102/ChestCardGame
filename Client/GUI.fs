module GUI
open System.Drawing
open System
open System.Threading
open System.Windows.Forms
open CommandInterpriter

#nowarn "40"

type readDel = delegate of (string -> unit) -> unit
type showCardsDel = delegate of PlayingCard Set -> unit
type SetTextCallback = delegate of string -> unit
    
type MainForm () as this = 
    inherit WindowsFormsApplication1.Form1()
        
    let bmp = new Bitmap(this.panel1.Width, this.panel1.Height)
    
    do
        this.button1.Enabled <- false
        this.DoubleBuffered <- true
        this.textBox2.TextChanged.Add
            (fun _ -> this.textBox2.SelectionStart <- this.textBox2.Text.Length; this.textBox2.ScrollToCaret();)
        let bmpRect = Rectangle(Point(0,0), bmp.Size)
        //this.Paint.Add(fun (e:PaintEventArgs) -> e.Graphics.DrawImage(bmp, bmpRect))
        this.panel1.Paint.Add(fun (e:PaintEventArgs) -> e.Graphics.DrawImage(bmp, bmpRect))
        
    let get = CardVisual.get()
        
    let rec showcards cards =
        if this.InvokeRequired then
            this.Invoke(showCardsDel showcards, [| box cards |]) |> ignore
        else
            CardVisual.place bmp cards get
            this.panel1.Invalidate()

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

let suitSelectDialog suitCount =
    let suitCounter = ref suitCount
    let widthForm = 300

    let buttonOk = 
        let btn = new Button(
                    Enabled = false,
                    Text = "OK",
                    DialogResult = DialogResult.OK)
        let width = 75
        let x = widthForm / 2 - width/2
        let y = widthForm / 4 + 10
        btn.SetBounds(x, y, width, 23)
        btn
    //buttonOk.Anchor <- AnchorStyles.Bottom ||| AnchorStyles.Right

    let toggleButtons = 
        let btnCount = 4
        let length = widthForm / btnCount

        let resize (img:Bitmap) width =

            let widthf = float width
            //let k =  widthf / float (max img.Height img.Width)
            //if img.Height > img.Width then
            //    Size(
            let k = min (widthf / float img.Height) (widthf / float img.Width)
            let size = Size( int(float img.Width * k), int(float img.Height * k) )
            let b = new Bitmap(size.Width, size.Height)
            use g = Graphics.FromImage b
            let rect = Rectangle(Point(0, 0), size)
            g.DrawImage(img, rect, Rectangle(Point(0,0), img.Size), GraphicsUnit.Pixel)
            b

        [0..btnCount - 1]
        |> List.map (fun i ->
            let b = new CheckBox(Width = length, Height = length)
            let img = CardVisual.suitIdToImg i |> Option.get
                
            b.Image <- resize img (length - 5)
            b.Location <- Point(i * length, 0)
            b.CheckedChanged.Add(fun _ -> 
                if b.Checked then decr suitCounter else incr suitCounter;
                buttonOk.Enabled <- !suitCounter = 0
                )
            b.Appearance <- System.Windows.Forms.Appearance.Button
            b)

    let form = new Form(
                    Text = sprintf "Select %d suit" suitCount,
                    ClientSize = new Size(widthForm, 107),
                    FormBorderStyle = FormBorderStyle.FixedDialog,
                    StartPosition = FormStartPosition.CenterScreen,
                    MinimizeBox = false,
                    MaximizeBox = false,
                    AcceptButton = buttonOk)
    do
        form.Closing.Add(fun e -> e.Cancel <- !suitCounter <> 0 )
        let (control:Control list) = [buttonOk :> Control;] 
        let rbuttons = toggleButtons |> List.map (fun x -> x :> Control)
        form.Controls.AddRange([| yield! control; yield! rbuttons |])

    let dialogResult = form.ShowDialog()
    toggleButtons |> List.mapi (fun i x -> i, x.Checked)
    |> List.choose (function (i, true) -> Some i | _ -> None)