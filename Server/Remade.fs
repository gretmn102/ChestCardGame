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
    let haveRank r { Cards = cards } =
        cards |> Set.exists (fun { Rank = x} -> x = r )
    let haveCount (r, count) { Cards = cards } =
        cards |> Set.filter (fun { Rank = x } -> x = r ) |> Set.count = count
//    let haveSuit (r, suits) {Cards = cards} =
//        let guessCards = Set [for s in suits -> { Rank = r; Suit = s }]
//        Set.isSubset guessCards cards
    let give p1 p2 card =
        let p1 = {p1 with Cards = Set.remove card p1.Cards}
        let p2 = {p2 with Cards = Set.add card p2.Cards}
        p1, p2

type Ask =
    | IsRank of Rank
    | IsCount of int
    | IsSuit of Set<Suit>
type Info =
    | AddCard of PlayingCard
    | RemoveCard of PlayingCard

    | AskXOnYou  of Ask * PlayerId // противник спрашивает у тебя
    | AskXOnY    of Ask * PlayerId * PlayerId

    | MoveYouOnX of PlayerId
    | MoveXOnY   of PlayerId * PlayerId // кто-то на кого-то начинает ход
    | MoveXOnYou of PlayerId // на тебя ходит

    | Success of bool
    
type Answ =
    | Wait of PlayerId
    | EndGame

    | GetRank
    | GetCount
    | GetSuit

    | Info of Info
    | FailAnsw
    | Nil

type Inputs = 
    | RankInput of Rank
    | CountInput of int
    | SuitInput of Set<Suit>
type Req = 
    | GetState of PlayerId
    | Input of PlayerId * Inputs
    
open Continues
type Msg =
    | Post of Req * AsyncReplyChannel<Answ>

type State = { PCircle: PlayersCircleT; Deck: Deck.Deck; Players: Map<PlayerId, Player.Player> }
type State2 = { MoveCircle: MoveCircleT; Deck: Deck.Deck; Players: Map<PlayerId, Player.Player> }

let replyChannel (r:AsyncReplyChannel<_>) x = r.Reply x
let receive (mailboxProc:MailboxProcessor<_>) = mailboxProc.Receive()

let requester inbox interp =
    let rec loop () =
        async {
            let! msg = receive inbox
            match msg with
            | Post(req, r) -> 
                let (answ, ret) = interp req
                replyChannel r answ
                match ret with
                | Some x -> return x
                | None -> return! loop()
        }
    loop()
let inputer inbox pId getStateAnsw inputState =
    let interp = function
        | GetState pId' -> 
            if pId' = pId then getStateAnsw, None
            else Answ.Wait pId, None
        | Input(pId', inputs) ->
            if pId' = pId then inputState inputs
            else FailAnsw, None
    requester inbox interp

let informer inbox pId msg =
    let f = function
        | GetState pId' -> 
            if pId' = pId then msg, Some ()
            else Answ.Wait pId, None
        | _ -> FailAnsw, None
    requester inbox f

let loopMoveCircle (inbox:MailboxProcessor<_>) st =
    let rec loopMoveCircle ({ MoveCircle = mc; Deck = d; Players = pls } as st) =
        match mc with
        | MoveCircleT.EndMoveCircle -> async { return st }
        | MoveCircleT.GetMove((p1, p2, xs), f) -> 
            async {
                let informs answOnYou answXonY =
                    async {
                        do! informer inbox p2 (Info answOnYou)
                        for x in xs do
                            do! informer inbox x (Info answXonY)
                    }
                do! informs (MoveXOnYou p1) (MoveXOnY(p1, p2))

                let! rankInput = 
                    let f = function 
                        | RankInput rank -> Nil, Some rank
                        | _ -> FailAnsw, None
                    inputer inbox p1 GetRank f
                let ask = IsRank rankInput
                do! informs (AskXOnYou(ask, p1)) (AskXOnY(ask, p1, p2))
                let res = Player.haveRank rankInput pls.[p2]

                let answ = Success res
                do! informer inbox p1 (Info answ)
                do! informs answ answ

                if res then
                    let! countInput =
                        let f = function
                            | CountInput count -> Nil, Some count
                            | _ -> FailAnsw, None
                        inputer inbox p1 GetCount f
                    let ask = IsCount countInput
                    do! informs (AskXOnYou(ask, p1)) (AskXOnY(ask, p1, p2))
                    let res = Player.haveCount (rankInput, countInput) pls.[p2]

                    let answ = Success res
                    do! informer inbox p1 (Info answ)
                    do! informs answ answ
                    
                    if res then
                        let! suitInput =
                            let f = function
                                | SuitInput x -> Nil, Some x
                                | _ -> FailAnsw, None
                            inputer inbox p1 GetSuit f
                        let ask = IsSuit suitInput
                        do! informs (AskXOnYou(ask, p1)) (AskXOnY(ask, p1, p2))
                        let guessCards = [for s in suitInput -> {Rank = rankInput; Suit = s}]
                        
                        let res = 
                            guessCards |> List.forall (fun x -> Player.haveCard x pls.[p2])
                        
                        let answ = Success res
                        do! informer inbox p1 (Info answ)
                        do! informs answ answ
                        if res then
                            let give c pls = 
                                async {
                                    do! informer inbox p1 (Info.AddCard c |> Info)
                                    do! informer inbox p2 (Info.RemoveCard c |> Info)
                                    let (pd1, pd2) = Map.find p1 pls, Map.find p2 pls
                                    let (pd2, pd1) = Player.give pd2 pd1 c
                                    return Map.add p1 pd1 pls |> Map.add p2 pd2
                                }
                            failwith "not impl"
                            return! loopMoveCircle { st with MoveCircle = f false }
                        else
                            return! loopMoveCircle { st with MoveCircle = f false }
                    else
                        return! loopMoveCircle { st with MoveCircle = f false }
                else
                    return! loopMoveCircle { st with MoveCircle = f false }
            }
        | MoveCircleT.PlayerHaveCards(pId, f) ->
            loopMoveCircle {st with MoveCircle = Player.haveCards pls.[pId] |> f}
    loopMoveCircle st

let mail plsId =
    MailboxProcessor.Start (fun inbox ->
        let rec loopCircle ({ PCircle = pcl; Deck = d; Players = pls } as st) =
            let pUpdate id v = Map.add id v pls
            match pcl with
            | PlayersCircleT.DeckIsEmpty f -> 
                loopCircle {st with PCircle = Deck.isEmpty d |> f}
            | PlayersCircleT.End -> 
                requester inbox (function _ -> Answ.EndGame, None)
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
                    let (p, d, card) = Player.takeFromDeck2 pls.[pId] d
                    do! informer inbox pId (Info(AddCard card))
                    return! loopCircle {PCircle = f(); Deck = d; Players = pUpdate pId p}
                }
            | PlayersCircleT.Fail str -> failwith str
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
