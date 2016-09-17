module Remade

module List =
    let next l =
        //List.permute(fun x -> (x + 1) % List.length l) l // [3;1;2] -> [2;3;1]
        [ yield! List.tail l; yield List.head l]
    (* test
    next [1;2;3] // [2;3;1]
    next [2;3;1] // [3;1;2]
    next [3;1;2] // [1;2;3]
    next ([]:int list) // fail
    next [1] // [1] *)

    let next2 curr rest =
        //List.permute(fun x -> (x + 1) % List.length l) l // [3;1;2] -> [2;3;1]
         [ yield! rest; yield curr]
    let others current l =
        if not <| List.exists ((=) current) l then failwith "такой элемент в списке отсутствует"
        let to' = Seq.takeWhile ((<>) current) l
        let from = Seq.skipWhile ((<>) current) l
        [ yield! Seq.skip 1 from; yield! to']
    (*
    // test
    others 1 [1..5] // [2;3;4;5]
    others 3 [1..5] // [4;5;1;2]
    others 5 [1..5] // [1;2;3;4]
    others 6 [1..5] // fail
    others 1 []     // fail
    others 1 [1]    // [] *)

    let nextWhile func xs =
        let rec f func acc = function
            | [] -> None
            | h::t as pls -> 
                if func h then
                    pls @ (List.rev acc) |> Some
                else
                    f func (h::acc) t
        f func [] xs
    assert
        List.forall id [
            nextWhile ((=) 10) [0..20] = Some[10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9];
            nextWhile ((=) -1) [0..20] = None
            nextWhile ((=) -1) [] = None;
            nextWhile (fun _ -> true) ([]:int list) = None;
        ]


type PlayerId = int

module Intuitive =
    module List = 
        let next l = [ yield! List.tail l; yield List.head l]
    type Player =
        { Name:string; ReadLine: unit -> string; WriteLine: string -> unit}
    let rec gameCircle = function
        | h::t as xs -> 
            let msg = h.ReadLine()
            t |> List.iter (fun x -> x.WriteLine(sprintf "player %s send: %s." h.Name msg ))
            List.next xs |> gameCircle
        | _ -> ()
    type Cmd = ReadLine | WriteLine of string
    type Ty(users:Set<_>) =
        let mutable m_suspend = false
        let mutable m_input = ""
        let mutable m_state = "", ReadLine
        let suspend () =
            if m_suspend then failwith "already suspend"
            m_suspend <- true
            printfn "%A" m_state
            while m_suspend do System.Threading.Thread.Sleep 100
        do
            let getInput () = let input = m_input in m_input <- ""; input
            let read p () = 
                m_state <- p, ReadLine
                suspend()
                getInput ()
            let write p s =
                m_state <- p, WriteLine s
                suspend()
            users |> List.ofSeq
            |> List.map (fun name -> {Name = name; ReadLine = read name; WriteLine = write name} )
            |> (fun x -> async {gameCircle x} |> Async.Start)
        member __.State = m_state
        member __.Input s =
            if m_suspend |> not then failwith "not suspend for input"
            m_input <- s
            m_suspend <- false
        member __.Continue () =
            if m_suspend |> not then failwith "not suspend for continue"
            m_suspend <- false
///<summary> реализация через продолжения</summary>
module Continues =
    module Sample = 
        type T =
            | Read of (string -> T)
            | Write of (string * string) * (unit -> T)
            
        let next l = [ yield! List.tail l; yield List.head l]
        let rec f xs =
            let writers msg = 
                (fun () -> f (List.tail xs))
                |> List.foldBack (fun x state -> (fun () -> Write((x, msg), state))) xs
            Read(fun msg -> writers msg ())
    
    
    type PlayersCircleT = 
        | MoveCircle of PlayerId list * (unit -> PlayersCircleT)
        | DeckIsEmpty of (bool -> PlayersCircleT)
        | PlayerHaveCards of PlayerId * (bool -> PlayersCircleT)
        | PlayerTakeCardFromDeck of PlayerId * (unit -> PlayersCircleT)
        | End
        | Fail of string
    let rec playersCircle pls =
        DeckIsEmpty(function
            | false ->
                match pls with
                | [] -> Fail "deck not empty, player empty"
                | [h] -> PlayerTakeCardFromDeck(h, fun () -> playersCircle [h] )
                | (h::_ as pls) ->
                    MoveCircle(pls, fun () ->
                        PlayerTakeCardFromDeck(h, fun () ->
                                List.next pls |> playersCircle))
            | true ->
                let rec f = function
                    | [] -> End
                    | [h] -> 
                        PlayerHaveCards(h, function
                            | true -> Fail "deck is empty, one player not over"
                            | false -> End)
                    | h::t as pls ->
                        MoveCircle(pls, fun () ->
                            PlayerHaveCards(h, fun res ->
                                let pl = if res then List.next pls else t
                                f pl))
                f pls)
                
    type MoveCircleT =
        | EndMoveCircle
        | GetMove of (PlayerId * PlayerId * PlayerId list) * (bool -> MoveCircleT)
        | PlayerHaveCards of PlayerId * (bool -> MoveCircleT)
    let moveCircle = function
        | [] -> failwith "pls is empty"
        | [_] -> failwith "pls only one"
        | curr::t -> 
            let rec f xs =
                let nextWhile cont xs =
                    let rec f = function
                        | [] -> EndMoveCircle
                        | h::t as pls -> 
                            PlayerHaveCards(h, function
                                | true -> cont pls
                                | false -> f t)
                    f xs
                let f'' = function
                    | h::t as xs -> 
                        GetMove((curr, h, t), function 
                            | true -> f (List.next xs)
                            | false -> EndMoveCircle)
                    | [] -> failwith "nextWhile has return Some([])"
                xs |> nextWhile f''
            f t

type Rank = int
type Suit = int
type PlayingCard = { Rank:Rank; Suit:Suit }

module Deck =
    type Deck = Deck of PlayingCard list
    let take = function
        | Deck [] -> failwith "card is no more in deck"
        | Deck (h::t) -> h, Deck t
    let init = 
        [ for suit in [ 0..3 ] do
            for rank in 0..13 -> { Rank = rank; Suit = suit } ]
        |> Deck
    let isEmpty = function
        | Deck [] -> true
        | _ -> false

module Player = 
    type Player = { Id:int; Cards:Set<PlayingCard> }
    let takeFromDeck ({Cards = cards} as p) d =
        let (card, d') = Deck.take d
        {p with Cards = cards.Add card }, d'
    let takeFromDeck2 ({Cards = cards} as p) d =
        let (card, d') = Deck.take d
        {p with Cards = cards.Add card }, d', card
    let haveCards { Cards = cards } = Set.isEmpty cards |> not
    let haveCard card { Cards = cards } = Set.contains card cards
    let give p1 p2 card =
        let p1 = {p1 with Cards = Set.remove card p1.Cards}
        let p2 = {p2 with Cards = Set.add card p2.Cards}
        p1, p2

type Ask =
    | IsRank of Rank
    | IsCount of Rank * int
    | IsSuit of Rank * Suit list
type Info =
    | AddCard of PlayingCard
    | RemoveCard of PlayingCard

    | AskXOnYou  of Ask * PlayerId // противник спрашивает у тебя
    | AskXOnY    of Ask * PlayerId * PlayerId

    | MoveYouOnX of PlayerId
    | MoveXOnY   of PlayerId * PlayerId // кто-то на кого-то начинает ход
    | MoveXOnYou of PlayerId // на тебя ходит
    
type Answ =
    | Wait of PlayerId
    | EndGame
    | MoveCircleAnsw
    | GetMoveAnsw of PlayerId * PlayerId * PlayerId list
    | Info of Info

type Req = 
    | GetState of PlayerId
open Continues
type Msg =
    | Post of Req * AsyncReplyChannel<Answ>

type State = { PCircle: PlayersCircleT; Deck: Deck.Deck; Players: Map<PlayerId, Player.Player> }
type State2 = { MoveCircle: MoveCircleT; Deck: Deck.Deck; Players: Map<PlayerId, Player.Player> }
let requester (inbox:MailboxProcessor<_>) pId f =
    let rec loop () =
        async {
            let! msg = inbox.Receive ()
            match msg with
            | Post(req, r) ->
                match req with
                | GetState(pId') -> 
                    if pId = pId' then return f r
                    else
                        r.Reply(Answ.Wait pId)
                        return! loop ()
        }
    printfn "start"
    loop()

let loopMoveCircle (inbox:MailboxProcessor<_>) st =
    let rec loopMoveCircle ({ MoveCircle = mc; Deck = d; Players = pls } as st) =
        match mc with
        | MoveCircleT.EndMoveCircle -> async { return st }
        | MoveCircleT.GetMove((p1, p2, xs), f) -> 
            async {
                do! requester inbox p2 (fun r -> r.Reply(Info(Info.MoveXOnYou p1)))
                let! msg = inbox.Receive ()
                match msg with
                | Post(req, r) ->
                    match req with
                    | GetState _ -> 
                        printfn "before"
                        let f' (r:AsyncReplyChannel<_>) =
                            let (p, d, card) = Player.takeFromDeck2 pls.[p2] d
                            r.Reply(Answ.Info(Info.AddCard card))
                            1
                        //let! x = requester inbox p2 f' //(fun r -> r.Reply(Info(Info.MoveXOnYou p1)))
                        printfn "after"
//                        for x in xs do
//                            do! requester inbox x (fun r -> r.Reply(Info(Info.MoveXOnY(p1, p2))))
                        //r.Reply(Answ.GetMoveAnsw(p1, p2, xs))
                        return! loopMoveCircle { st with MoveCircle = f false }
            }
        | MoveCircleT.PlayerHaveCards(pId, f) ->
            loopMoveCircle {st with MoveCircle = Player.haveCards pls.[pId] |> f}
    loopMoveCircle st

let mail plsId =
    MailboxProcessor.Start (fun inbox ->
        //let rec loopMoveCircle ()
        let rec loopCircle ({ PCircle = pcl; Deck = d; Players = pls } as st) =
            let pUpdate id v = Map.add id v pls
            match pcl with
            | PlayersCircleT.DeckIsEmpty f -> 
                loopCircle {st with PCircle = Deck.isEmpty d |> f}
            | PlayersCircleT.End -> 
                async {
                    let! msg = inbox.Receive ()
                    match msg with
                    | Post(req, r) ->
                        match req with
                        | GetState _ -> 
                            r.Reply(Answ.EndGame)
                            return! loopCircle st
                }
            | PlayersCircleT.MoveCircle(pl, f) as x ->
                async {
                    let! r = 
                        loopMoveCircle inbox { MoveCircle = moveCircle pl; Deck = d; Players = pls }
                    let st = { PCircle = f(); Deck = r.Deck; Players = r.Players }
                    return! loopCircle st
                }
            | PlayersCircleT.PlayerHaveCards(pl, f) ->
                loopCircle {st with PCircle = Player.haveCards pls.[pl] |> f}
            | PlayersCircleT.PlayerTakeCardFromDeck(pId, f) ->
                async {
                    let f (r:AsyncReplyChannel<_>) =
                        let (p, d, card) = Player.takeFromDeck2 pls.[pId] d
                        r.Reply(Answ.Info(Info.AddCard card))
                        {PCircle = f(); Deck = d; Players = pUpdate pId p}
                    let! res = requester inbox pId f
                    return! loopCircle res
                }
        let pCircleSt = playersCircle (List.ofSeq plsId)
        let pls = plsId |> Set.map (fun x -> {Player.Id = x; Player.Cards = Set.empty})
        let d = Deck.init
        let (d, pls) = 
            let f (d,pls) p =
                let (p, d) = Player.takeFromDeck p d
                (d, p::pls)
            pls |> Set.fold f (d, [])
        let pls = pls |> List.map (fun x -> x.Id, x) |> Map.ofList
        let initSt = { PCircle = pCircleSt; Deck = d; Players = pls }
        loopCircle initSt)
