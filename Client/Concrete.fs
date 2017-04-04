module Concrete

open System.Net
open CommandInterpriter
open System.Net.Sockets

let stream =
    let connect () =
        let ip = "127.0.0.1"
        let client = new TcpClient();
        client.Connect(IPAddress.Parse ip, 5000)
        client.GetStream()
    let rec f () = try connect () with e -> e.Message |> printfn "%s"; System.Threading.Thread.Sleep 500; f()
    in f()
let form = new GUI.MainForm()
let writeRead write = 
    let req = ClientReq.unpars write
    streamToStream req stream
    let ms = ServerAnswer.pars stream
    ms


open Abstr
open FsharpMyExtension.FSharpExt


type State = { Cards: PlayingCard Set }
let rec interpConcrete st = function
    | Print(x, f) -> 
        form.Print x
        f() |> interpConcrete st
    | Read(f) -> form.Input (f >> interpConcrete st)
    | WriteRead(r, f) -> ClientReq.GameReq r |> writeRead |> f |> interpConcrete st
    | SuitSelect(count, f) -> 
        let r = GUI.suitSelectDialog count
        Set r |> f |> interpConcrete st
    | DisplayCards(cards, f) ->
        form.ShowCards cards
        f() |> interpConcrete { st with Cards = cards }
    | Next f -> ClientReq.GameReq GameReq.GetState |> writeRead |> f |> interpConcrete st
    | NextUntil(last, f) -> 
        let fn () = 
            System.Threading.Thread.Sleep 1000
            ClientReq.GameReq GameReq.GetState |> writeRead
        let c = until ((<>) last) (ignore >> fn) (fn())
        interpConcrete st <| f c
    | Until((init, cond), f) ->
        let fn () = 
            System.Threading.Thread.Sleep 1000
            ClientReq.GameReq init |> writeRead
        let c = until (cond) (ignore >> fn) (fn())
        interpConcrete st <| f c                    
    | GameAnswerInfo(gameInfo, f) ->
        let fn = function
            | GameAnswer.TakeCards(p, cards) ->
                let cards = st.Cards + cards
                form.ShowCards cards
                { st with Cards = cards }
            | GameAnswer.GiveCards(p, cards') ->
                let cards = Set.fold (flip Set.remove) st.Cards cards'
                form.ShowCards cards
                { st with Cards = cards }
            | GameAnswer.AddCardFromDeck c ->
                let cards = Set.add c st.Cards
                form.ShowCards cards
                { st with Cards = cards }
            | x -> 
                form.Print <| sprintf "%A" x
                st
        let st = fn gameInfo
        f () |> interpConcrete st
    | SlotsFull -> form.Print "slot full. Maybe exist?"
    | EndGame -> form.Print "Game end"
    | Login(f) -> form.Input (ClientReq.Login >> writeRead >> f >> interpConcrete st)
open System.Windows.Forms
async { writeRead ClientReq.GetServerState |> interp |> interpConcrete { Cards = Set.empty }} |> Async.Start
Application.Run form